open Query
open Document

type results = {
  docId : int;
  score : float;
}

type corpus_stats = {
  n : float;
  average_document_len : float;
  doc_lengths : (int, float) Hashtbl.t;
}

let compute_corpus_stats (documents : document list) : corpus_stats =
  let n = float_of_int (List.length documents) in
  let doc_lengths = Hashtbl.create (List.length documents) in
  let total =
    List.fold_left
      (fun acc (d : document) ->
        let len = float_of_int (List.length d.text) in
        Hashtbl.add doc_lengths d.index len;
        acc +. len)
      0.0 documents
  in
  { n; average_document_len = total /. n; doc_lengths }

(* Documents sorted by decreasing score (ties broken by doc id). Documents
   with score 0 share no term with the query and are not part of the ranking. *)
let rank (results : results list) =
  results
  |> List.filter (fun r -> r.score > 0.0)
  |> List.sort (fun a b ->
         match Float.compare b.score a.score with
         | 0 -> Int.compare a.docId b.docId
         | c -> c)

(* ------------------------------------------------------------------ *)
(* BM25                                                               *)
(* ------------------------------------------------------------------ *)

type bm25_params = {
  k1 : float;
  b : float;
}

(* idf(t) = ln(1 + (N - df + 0.5) / (df + 0.5)); always > 0 *)
let bm25_idf ~(stats : corpus_stats) ~df =
  Float.log (1.0 +. ((stats.n -. df +. 0.5) /. (df +. 0.5)))

(* Contribution of one query term to one document:
   idf(t) * tf * (k1 + 1) / (tf + k1 * (1 - b + b * |d| / avgdl)) *)
let bm25_term_score ~(params : bm25_params) ~(stats : corpus_stats) ~df ~tf
    ~doc_len =
  let length_norm =
    1.0 -. params.b +. (params.b *. doc_len /. stats.average_document_len)
  in
  bm25_idf ~stats ~df *. (tf *. (params.k1 +. 1.0))
  /. (tf +. (params.k1 *. length_norm))

let posting_tf inverted_index term doc_index =
  match Hashtbl.find_opt inverted_index term with
  | None -> (0.0, 0.0)
  | Some postings ->
      ( float_of_int (Hashtbl.length postings),
        float_of_int (Option.value ~default:0 (Hashtbl.find_opt postings doc_index)) )

(* Score of a single document; each occurrence of a term in the query counts
   (query term frequency enters linearly). Useful to explain a score. *)
let bm25_score ~(query : query) ~doc_index ~inverted_index
    ~(params : bm25_params) ~(stats : corpus_stats) =
  let doc_len = Hashtbl.find stats.doc_lengths doc_index in
  List.fold_left
    (fun acc t ->
      let df, tf = posting_tf inverted_index t doc_index in
      if tf = 0.0 then acc
      else acc +. bm25_term_score ~params ~stats ~df ~tf ~doc_len)
    0.0 query.text

(* Same as [bm25_score] for every document, walking the postings lists so
   only documents containing a query term are touched. *)
let bm25 ~(query : query) ~(documents : document list) ~inverted_index
    ~(params : bm25_params) ~(stats : corpus_stats) =
  let scores : (int, float) Hashtbl.t = Hashtbl.create 1024 in
  List.iter
    (fun term ->
      match Hashtbl.find_opt inverted_index term with
      | None -> ()
      | Some postings ->
          let df = float_of_int (Hashtbl.length postings) in
          Hashtbl.iter
            (fun doc_id tf ->
              let doc_len = Hashtbl.find stats.doc_lengths doc_id in
              let s =
                bm25_term_score ~params ~stats ~df ~tf:(float_of_int tf) ~doc_len
              in
              let prev = Option.value ~default:0.0 (Hashtbl.find_opt scores doc_id) in
              Hashtbl.replace scores doc_id (prev +. s))
            postings)
    query.text;
  List.map
    (fun (d : document) ->
      { docId = d.index;
        score = Option.value ~default:0.0 (Hashtbl.find_opt scores d.index) })
    documents

(* ------------------------------------------------------------------ *)
(* Vector space model: ltc weights (1 + ln tf) * ln(N / df), cosine    *)
(* ------------------------------------------------------------------ *)

type vector_space = {
  idf : (string, float) Hashtbl.t;
  doc_norms : (int, float) Hashtbl.t;  (* euclidean norm of each document vector *)
}

let tf_weight freq = if freq <= 0.0 then 0.0 else 1.0 +. Float.log freq

let build_vector_space ~inverted_index ~(stats : corpus_stats) : vector_space =
  let idf = Hashtbl.create (Hashtbl.length inverted_index) in
  let squares : (int, float) Hashtbl.t = Hashtbl.create 1024 in
  Hashtbl.iter
    (fun term postings ->
      let df = float_of_int (Hashtbl.length postings) in
      let term_idf = Float.log (stats.n /. df) in
      Hashtbl.replace idf term term_idf;
      Hashtbl.iter
        (fun doc_id tf ->
          let w = tf_weight (float_of_int tf) *. term_idf in
          let prev = Option.value ~default:0.0 (Hashtbl.find_opt squares doc_id) in
          Hashtbl.replace squares doc_id (prev +. (w *. w)))
        postings)
    inverted_index;
  let doc_norms = Hashtbl.create (Hashtbl.length squares) in
  Hashtbl.iter (fun d s -> Hashtbl.replace doc_norms d (Float.sqrt s)) squares;
  { idf; doc_norms }

(* cos(q, d) = sum_t w(t,q) w(t,d) / (|q| |d|). Terms absent from the
   collection have no weight, so they change neither the dot product nor the
   norm of the query. *)
let vector ~(query : query) ~(documents : document list) ~inverted_index
    ~(space : vector_space) =
  let counts : (string, float) Hashtbl.t = Hashtbl.create 16 in
  List.iter
    (fun t ->
      let c = Option.value ~default:0.0 (Hashtbl.find_opt counts t) in
      Hashtbl.replace counts t (c +. 1.0))
    query.text;
  let dot : (int, float) Hashtbl.t = Hashtbl.create 1024 in
  let query_sq = ref 0.0 in
  Hashtbl.iter
    (fun term count ->
      match (Hashtbl.find_opt space.idf term, Hashtbl.find_opt inverted_index term) with
      | Some term_idf, Some postings ->
          let wq = tf_weight count *. term_idf in
          query_sq := !query_sq +. (wq *. wq);
          Hashtbl.iter
            (fun doc_id tf ->
              let wd = tf_weight (float_of_int tf) *. term_idf in
              let prev = Option.value ~default:0.0 (Hashtbl.find_opt dot doc_id) in
              Hashtbl.replace dot doc_id (prev +. (wq *. wd)))
            postings
      | _ -> ())
    counts;
  let query_norm = Float.sqrt !query_sq in
  List.map
    (fun (d : document) ->
      let num = Option.value ~default:0.0 (Hashtbl.find_opt dot d.index) in
      let denom =
        query_norm *. Option.value ~default:0.0 (Hashtbl.find_opt space.doc_norms d.index)
      in
      { docId = d.index; score = (if denom = 0.0 then 0.0 else num /. denom) })
    documents
