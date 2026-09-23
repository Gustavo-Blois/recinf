(* Everything needed to query the collection under one preprocessing
   configuration: preprocessed documents and queries, inverted index and
   corpus statistics. *)
type t = {
  config : Pipeline.pipeline_config;
  documents : Document.document list;
  queries : Query.query list;
  inverted_index : (string, (int, int) Hashtbl.t) Hashtbl.t;
  stats : Model.corpus_stats;
  space : Model.vector_space;
  by_id : (int, Document.document) Hashtbl.t;
}

type model =
  | Vector
  | Bm25 of Model.bm25_params

let build ~documents ~queries config =
  let documents, queries = Pipeline.preprocessing ~documents ~queries config in
  let inverted_index = Index.build_inverted_index documents in
  let stats = Model.compute_corpus_stats documents in
  let space = Model.build_vector_space ~inverted_index ~stats in
  let by_id = Hashtbl.create 2048 in
  List.iter (fun (d : Document.document) -> Hashtbl.replace by_id d.index d) documents;
  { config; documents; queries; inverted_index; stats; space; by_id }

let score t model (query : Query.query) =
  match model with
  | Vector ->
      Model.vector ~query ~documents:t.documents ~inverted_index:t.inverted_index
        ~space:t.space
  | Bm25 params ->
      Model.bm25 ~query ~documents:t.documents ~inverted_index:t.inverted_index ~params
        ~stats:t.stats

let ranked t model query = Model.rank (score t model query)

let query t idx =
  match List.find_opt (fun (q : Query.query) -> q.index = idx) t.queries with
  | Some q -> q
  | None -> failwith (Printf.sprintf "no query %d (valid: 1..%d)" idx (List.length t.queries))

let doc t id =
  match Hashtbl.find_opt t.by_id id with
  | Some d -> d
  | None -> failwith (Printf.sprintf "no document %d" id)

(* Same query (same qrels) with different text, preprocessed like the rest. *)
let with_text t (q : Query.query) text =
  { q with
    text = Pipeline.apply_preprocessing t.config (Token.clean_tokens (Token.word_list text)) }

let model_name = function
  | Vector -> "vector"
  | Bm25 p -> Printf.sprintf "bm25(k1=%g,b=%g)" p.k1 p.b
