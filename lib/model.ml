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

let vector_score ~query ~document ~inverted_index ~(stats : corpus_stats) ~vocabulary =
   let df t =
    float_of_int @@ Hashtbl.length
    @@ Option.value ~default:(Hashtbl.create 1) (Hashtbl.find_opt inverted_index t)
  in
  let frequency term =
    let document_table =
      Option.value ~default:(Hashtbl.create 1) (Hashtbl.find_opt inverted_index term)
    in
    float_of_int @@ Option.value ~default:0 (Hashtbl.find_opt document_table document.index)
  in

  let count_occurrences term text = 
    List.fold_left (fun acc word ->
      if (String.equal word term) then acc +. 1.0
      else acc
      ) 0.0 text 
    in

  let query_vector =
    List.map (fun term ->
      (1.0 +. Float.log (frequency term)) *. (Float.log (stats.n /. (count_occurrences term query.text)))
      ) vocabulary
    in
  let document_vector =
    List.map (fun term ->
      (1.0 +. Float.log (frequency term)) *. (Float.log (stats.n /. (count_occurrences term query.text)))
      ) vocabulary
    in

  
