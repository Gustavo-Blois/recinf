open Recinf

let close a b = Float.abs (a -. b) < 1e-9
let check name cond = if not cond then (Printf.eprintf "FAIL: %s\n" name; exit 1)

let doc index text : Document.document =
  { index; title = []; authors = []; bibliography = []; text }

let query text : Query.query =
  { index = 1; original_id = 1; text; judgments = []; relevant_documents = [] }

(* toy corpus: N = 3, avgdl = 3 *)
let docs = [ doc 1 [ "a"; "b"; "b" ]; doc 2 [ "a"; "c"; "c"; "c" ]; doc 3 [ "d"; "d" ] ]
let index = Index.build_inverted_index docs
let stats = Model.compute_corpus_stats docs
let score_of results id = (List.find (fun (r : Model.results) -> r.docId = id) results).score

let () =
  (* BM25, query "b", k1 = 1.2, b = 0.75. df(b) = 1, tf in d1 = 2, |d1| = 3 = avgdl *)
  let params : Model.bm25_params = { k1 = 1.2; b = 0.75 } in
  let idf = Float.log (1.0 +. ((3.0 -. 1.0 +. 0.5) /. (1.0 +. 0.5))) in
  let expected = idf *. (2.0 *. 2.2) /. (2.0 +. 1.2 *. 1.0) in
  let r = Model.bm25 ~query:(query [ "b" ]) ~documents:docs ~inverted_index:index ~params ~stats in
  check "bm25 hand value" (close (score_of r 1) expected);
  check "bm25 non-matching doc is 0" (score_of r 2 = 0.0);
  let single =
    Model.bm25_score ~query:(query [ "b" ]) ~doc_index:1 ~inverted_index:index ~params ~stats
  in
  check "bm25 == bm25_score" (close single expected);
  (* b = 0 disables length normalisation: d2 (longer) must not be penalised *)
  let p0 : Model.bm25_params = { k1 = 1.2; b = 0.0 } in
  let r0 = Model.bm25 ~query:(query [ "c" ]) ~documents:docs ~inverted_index:index ~params:p0 ~stats in
  let idf_c = Float.log (1.0 +. (2.5 /. 1.5)) in
  check "bm25 b=0" (close (score_of r0 2) (idf_c *. (3.0 *. 2.2) /. (3.0 +. 1.2)));
  (* two-term query: contributions are summed (idf * saturation), not added raw *)
  let two = Model.bm25 ~query:(query [ "a"; "b" ]) ~documents:docs ~inverted_index:index ~params ~stats in
  let idf_a = Float.log (1.0 +. (1.5 /. 2.5)) in
  let a_in_d1 = idf_a *. 2.2 /. (1.0 +. 1.2 *. 1.0) in
  check "bm25 sum of terms" (close (score_of two 1) (a_in_d1 +. expected))

let () =
  (* vector: query "b" against d1. idf(b) = ln 3; d1 = {a: idf ln(3/2), b: (1+ln2) ln 3} *)
  let space = Model.build_vector_space ~inverted_index:index ~stats in
  let ia = Float.log 1.5 and ib = Float.log 3.0 in
  let wb = (1.0 +. Float.log 2.0) *. ib in
  let norm_d1 = Float.sqrt ((ia *. ia) +. (wb *. wb)) in
  let r = Model.vector ~query:(query [ "b" ]) ~documents:docs ~inverted_index:index ~space in
  check "cosine hand value" (close (score_of r 1) (wb *. ib /. (norm_d1 *. ib)));
  check "cosine of disjoint docs is 0" (score_of r 3 = 0.0);
  let same = Model.vector ~query:(query [ "d"; "d" ]) ~documents:docs ~inverted_index:index ~space in
  check "cosine of identical direction is 1" (close (score_of same 3) 1.0)

let () =
  let relevant = [ 1; 3; 9 ] and ranking = [ 1; 2; 3; 4; 5 ] in
  check "P@5" (close (Metrics.precision_at 5 ~relevant ranking) 0.4);
  check "R@5" (close (Metrics.recall_at 5 ~relevant ranking) (2.0 /. 3.0));
  (* AP = (1/1 + 2/3) / 3 *)
  check "AP" (close (Metrics.average_precision ~relevant ranking) ((1.0 +. (2.0 /. 3.0)) /. 3.0));
  (* AP@2: only doc 1 is inside the cutoff, divided by all 3 relevant docs *)
  check "AP@2" (close (Metrics.average_precision_at 2 ~relevant ranking) (1.0 /. 3.0));
  check "AP@k beyond the ranking equals AP" (close (Metrics.average_precision_at 99 ~relevant ranking) (Metrics.average_precision ~relevant ranking));
  check "AP@2 of a relevant doc at rank 3 is 0" (close (Metrics.average_precision_at 2 ~relevant:[ 3 ] ranking) 0.0);
  check "RR" (close (Metrics.reciprocal_rank ~relevant:[ 3 ] ranking) (1.0 /. 3.0));
  check "F1" (close (Metrics.f1_at 5 ~relevant ranking) (2.0 *. 0.4 *. (2.0 /. 3.0) /. (0.4 +. (2.0 /. 3.0))));
  (* NDCG: grades d1 = 1 (gain 4), d3 = 4 (gain 1); ideal order = [d1; d3] *)
  let judgments = [ (1, 1); (3, 4) ] in
  let dcg = 4.0 +. (1.0 /. Float.log2 4.0) and idcg = 4.0 +. (1.0 /. Float.log2 3.0) in
  check "NDCG" (close (Metrics.ndcg_at 5 ~judgments [ 1; 2; 3 ]) (dcg /. idcg));
  check "NDCG perfect" (close (Metrics.ndcg_at 5 ~judgments [ 1; 3 ]) 1.0)

let () =
  (* qrels are keyed by position in cran.qry, and grade -1 is not relevant *)
  let judgments = Cranqrel.judgments_table "../data/cranqrel" in
  let queries = Query.create_query (Utils.words_of_file "../data/cran.qry") judgments in
  let q3 = List.nth queries 2 in
  check "third query keeps original id 4" (q3.original_id = 4 && q3.index = 3);
  check "-1 grades dropped"
    (List.for_all (fun (q : Query.query) -> List.for_all (fun (_, g) -> g >= 1) q.judgments) queries);
  check "225 queries" (List.length queries = 225);
  check "every query has relevant docs"
    (List.for_all (fun (q : Query.query) -> q.relevant_documents <> []) queries);
  print_endline "all tests passed"
