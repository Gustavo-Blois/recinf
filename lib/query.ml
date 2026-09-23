open Utils
open Cranqrel
type query = {
  index: int;  (* 1-based position in cran.qry; this is the id used by cranqrel *)
  original_id: int;  (* id found in the ".I" line of cran.qry *)
  text: string list;
  judgments: (int * int) list;  (* (doc, grade), only grade >= 1 *)
  relevant_documents: int list;  (* docs with grade >= 1 *)
}

let create_query word_list judgments_tbl = 
  let queries = ref [] in
  let deserialize word_list =
    let original_id =
      take_in_between word_list ".I" ".W" |> int_of_string_singleton
    in
    let index = List.length !queries + 1 in
    let judgments =
      Option.value ~default:[] (Hashtbl.find_opt judgments_tbl index)
      |> List.filter (fun (_, grade) -> grade >= 1)
    in
    let relevant_documents = List.map fst judgments in
    let text = Token.clean_tokens @@ take_in_between word_list ".W" ".I" in
    let is_doc_marker x = String.equal x ".I" in
    let word_list =
      word_list
      |> List.drop_while is_doc_marker
      |> List.drop_while (fun x -> not (is_doc_marker x))
    in
    ({ index; original_id; text; judgments; relevant_documents }, word_list)
  in
  let rec deserialize_while_not_empty words =
    match deserialize words with
    | query, [] -> query :: !queries
    | query, next_words ->
        queries := query :: !queries;
        deserialize_while_not_empty next_words
  in
  deserialize_while_not_empty word_list |> List.rev