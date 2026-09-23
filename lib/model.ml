open Query
open Index
open Document


type bm25_results = {
  docId: int;
  score: float;
}

type bm25_params = {
  k1: float;
  b: float;
}

type corpus_stats = {
  n : float;
  average_document_len : float;
  doc_lengths : (int, float) Hashtbl.t;
}

let compute_corpus_stats documents : corpus_stats =
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


let bm25_score ~query ~doc_index ~inverted_index ~(params : bm25_params) ~(stats : corpus_stats) =
  let tf term =
    let document_table =
      Option.value ~default:(Hashtbl.create 1) (Hashtbl.find_opt inverted_index term)
    in
    float_of_int @@ Option.value ~default:0 (Hashtbl.find_opt document_table doc_index)
  in
  let df t =
    float_of_int @@ Hashtbl.length
    @@ Option.value ~default:(Hashtbl.create 1) (Hashtbl.find_opt inverted_index t)
  in
  let document_len =
    match Hashtbl.find_opt stats.doc_lengths doc_index with
    | Some len -> len
    | None -> failwith (Printf.sprintf "document with index %d not found" doc_index)
  in
  let second_term t =
    let tf_td = tf t in
    tf_td *. (params.k1 +. 1.0)
    /. (tf_td +. params.k1 *. (1.0 -. params.b +. (params.b *. document_len /. stats.average_document_len)))
  in
  let idf term =
    Float.log (1.0 +. (stats.n -. df term +. 0.5) /. (df term +. 0.5))
  in
  List.fold_left (fun acc t -> acc +. idf t +. second_term t) 0.0 query.text


let bm25 ~query ~documents ~inverted_index ~(params : bm25_params) = 
  let stats = compute_corpus_stats documents in
  List.map
  (fun (doc : document) ->
    {docId = doc.index ; score = bm25_score ~query ~doc_index:doc.index ~inverted_index ~params ~stats})
  documents


let get_vocabulary ~inverted_index =
  Hashtbl.to_seq inverted_index |> Seq.map (fun (x,_) -> x) |> List.of_seq


let sum_list_of_float l = 
  List.fold_left (fun acc x ->
    acc +. x
    ) 0.0 l



let vector_score ~query ~document ~inverted_index ~(stats : corpus_stats) ~vocabulary =
  let df t =
    match Hashtbl.find_opt inverted_index t with
    | None -> 0.0
    | Some tbl -> float_of_int (Hashtbl.length tbl)
  in
  let doc_freq term =
    match Hashtbl.find_opt inverted_index term with
    | None -> 0.0
    | Some tbl -> float_of_int (Option.value ~default:0 (Hashtbl.find_opt tbl document.index))
  in
  let count_occurrences term text =
    List.fold_left (fun acc w -> if String.equal w term then acc +. 1.0 else acc) 0.0 text
  in
  let idf term = Float.log (stats.n /. (1.0 +. df term)) in
  let tf_weight freq = if freq <= 0.0 then 0.0 else 1.0 +. Float.log freq in

  let query_vector =
    List.map (fun t -> tf_weight (count_occurrences t query.text) *. idf t) vocabulary
  in
  let document_vector =
    List.map (fun t -> tf_weight (doc_freq t) *. idf t) vocabulary
  in

  let dot = sum_list_of_float (List.map2 ( *. ) query_vector document_vector) in
  let norm v = Float.sqrt (sum_list_of_float (List.map (fun x -> x *. x) v)) in
  let denom = norm document_vector *. norm query_vector in
  if denom = 0.0 then 0.0 else dot /. denom

  
let vector ~query ~documents ~inverted_index ~(params : bm25_params) = 
  let stats = compute_corpus_stats documents in
  let vocabulary = get_vocabulary ~inverted_index in
  List.map
  (fun (document : document) ->
    {docId = document.index ; score = vector_score ~query ~document ~inverted_index ~stats ~vocabulary})
  documents