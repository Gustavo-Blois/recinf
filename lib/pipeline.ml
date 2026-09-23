open Document
open Token
open Stopwords 
open Query

type pipeline_config = {
  stemming: bool;
  stopwords: bool;
}


let stemmer =
  lazy
    (let language =
       List.find
         (fun (l : Snowball.Language.t) -> String.equal (l :> string) "english")
         Snowball.languages
     in
     Snowball.create language)

let stem words = List.map (Snowball.stem (Lazy.force stemmer)) words

let normalize words =
  List.map (String.lowercase_ascii) words

let apply_preprocessing (pipeline_config : pipeline_config) (text : string list) : string list =
  text
  |> normalize
  |> (if pipeline_config.stopwords then remove_stop_words else Fun.id)
  |> (if pipeline_config.stemming then stem else Fun.id)

let preprocessing ~(documents : document list) ~(queries : query list) (pipeline_config : pipeline_config) =
  let documents: document list =
    List.map
      (fun (doc: document) -> { doc with text = apply_preprocessing pipeline_config (clean_tokens doc.text) })
      documents
  in
  let queries =
    List.map
      (fun query -> { query with text = apply_preprocessing pipeline_config (clean_tokens query.text) })
      queries
  in
  documents, queries

