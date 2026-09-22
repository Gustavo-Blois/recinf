open Utils
open Cranqrel
type query = {
  index: int;
  text: string list;
  relevant_documents: int list
}

let create_query word_list relevant_documents_tbl = 
  let queries = ref [] in
  let deserialize word_list =
    let index =
      take_in_between word_list ".I" ".W" |> int_of_string_singleton
    in
    let relevant_documents = Option.value ~default:[] (Hashtbl.find_opt relevant_documents_tbl index) in
    let text = take_in_between word_list ".W" ".I" in
    let is_doc_marker x = String.equal x ".I" in
    let word_list =
      word_list
      |> List.drop_while is_doc_marker
      |> List.drop_while (fun x -> not (is_doc_marker x))
    in
    ({ index; text ; relevant_documents}, word_list)
  in
  let rec deserialize_while_not_empty words =
    match deserialize words with
    | query, [] -> query :: !queries
    | query, next_words ->
        queries := query :: !queries;
        deserialize_while_not_empty next_words
  in
  deserialize_while_not_empty word_list