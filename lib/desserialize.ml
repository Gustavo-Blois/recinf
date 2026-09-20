open Utils

type document = {
  index : int;
  title : string list;
  authors : string list;
  bibliography : string list;
  text : string list;
}

type query = {
  index: int;
  text: string list;
}

let create_query word_list = 
  let queries = ref [] in
  let desserialize word_list =
    let index =
      take_in_between word_list ".I" ".W" |> int_of_string_singleton
    in
    let text = take_in_between word_list ".W" ".I" in
    let is_doc_marker x = String.equal x ".I" in
    let word_list =
      word_list
      |> List.drop_while is_doc_marker
      |> List.drop_while (fun x -> not (is_doc_marker x))
    in
    ({ index; text }, word_list)
  in
  let rec desserialize_while_not_empty words =
    match desserialize words with
    | query, [] -> query :: !queries
    | query, next_words ->
        queries := query :: !queries;
        desserialize_while_not_empty next_words
  in
  desserialize_while_not_empty word_list

let create_documents word_list =
  let documents = ref [] in
  let desserialize word_list =
    let index =
      take_in_between word_list ".I" ".T" |> int_of_string_singleton
    in
    let title = take_in_between word_list ".T" ".A" in
    let authors = take_in_between word_list ".A" ".B" in
    let bibliography = take_in_between word_list ".B" ".W" in
    let text = take_in_between word_list ".W" ".I" in
    let is_doc_marker x = String.equal x ".I" in
    let word_list =
      word_list
      |> List.drop_while is_doc_marker
      |> List.drop_while (fun x -> not (is_doc_marker x))
    in
    ({ index; title; authors; bibliography; text }, word_list)
  in
  let rec desserialize_while_not_empty words =
    match desserialize words with
    | document, [] -> document :: !documents
    | document, next_words ->
        documents := document :: !documents;
        desserialize_while_not_empty next_words
  in
  desserialize_while_not_empty word_list
