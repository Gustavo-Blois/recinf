open Document
open Token
open Stopwords 
open Model
open Query

type pipeline_config = {
  stemming: bool;
  stopwords: bool;
}


type config = {
  pipeline: pipeline_config;
  model: model
}



let all_pipeline_configs =
  [ { stemming = true;  stopwords = true  };
    { stemming = true;  stopwords = false };
    { stemming = false; stopwords = true  };
    { stemming = false; stopwords = false } ]

let stem words = 
  let language =
    List.find
      (fun (l : Snowball.Language.t) -> String.equal (l :> string) "english")
      Snowball.languages
  in
  let stemmer = Snowball.create language in
  List.map (Snowball.stem stemmer) words

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

