open Pipeline

let preprocessing_configs =
  [ { stemming = false; stopwords = false };
    { stemming = false; stopwords = true };
    { stemming = true; stopwords = false };
    { stemming = true; stopwords = true } ]

let preprocessing_name (c : pipeline_config) =
  match (c.stopwords, c.stemming) with
  | false, false -> "raw"
  | true, false -> "stopwords"
  | false, true -> "stemming"
  | true, true -> "stopwords+stemming"

let preprocessing_of_name name =
  match List.find_opt (fun c -> preprocessing_name c = name) preprocessing_configs with
  | Some c -> c
  | None ->
      failwith
        (Printf.sprintf "unknown preprocessing %S (raw | stopwords | stemming | stopwords+stemming)" name)

let k1_values = [ 0.5; 1.2; 2.0 ]
let b_values = [ 0.0; 0.75; 1.0 ]
let default_bm25 : Model.bm25_params = { k1 = 1.2; b = 0.75 }

(* Ranking depth saved in rankings.csv: deep for the baseline runs (needed to
   locate relevant documents outside the top 10), shallow for the grid. *)
let deep_depth = 100
let shallow_depth = 10

type run = {
  preprocessing : string;
  model : string;
  params : Model.bm25_params option;
  per_query : Metrics.query_metrics list;
  summary : Metrics.summary;
}

let csv_params = function
  | None -> ("", "")
  | Some (p : Model.bm25_params) -> (Printf.sprintf "%g" p.k1, Printf.sprintf "%g" p.b)

let grade_of (q : Query.query) doc =
  Option.value ~default:0 (List.assoc_opt doc q.judgments)

let rec take k = function x :: r when k > 0 -> x :: take (k - 1) r | _ -> []

let run_all ~documents ~queries ~output_dir =
  if not (Sys.file_exists output_dir) then Sys.mkdir output_dir 0o755;
  let open_out name = Out_channel.open_text (Filename.concat output_dir name) in
  let rankings_oc = open_out "rankings.csv" in
  Printf.fprintf rankings_oc "preprocessing,model,k1,b,query,rank,doc,score,grade\n";
  let runs = ref [] in
  List.iter
    (fun pipeline_config ->
      let name = preprocessing_name pipeline_config in
      let env = Env.build ~documents ~queries pipeline_config in
      let queries = env.queries in
      let evaluate ~model ~params score_query =
        let ranked =
          List.map (fun (q : Query.query) -> (q, Model.rank (score_query q))) queries
        in
        let per_query =
          List.map
            (fun ((q : Query.query), ranking) ->
              Metrics.evaluate_query q (List.map (fun (r : Model.results) -> r.docId) ranking))
            ranked
        in
        let k1, b = csv_params params in
        let baseline =
          match params with None -> true | Some p -> p = default_bm25
        in
        let depth = if baseline then deep_depth else shallow_depth in
        List.iter
          (fun ((q : Query.query), ranking) ->
            List.iteri
              (fun i (r : Model.results) ->
                Printf.fprintf rankings_oc "%s,%s,%s,%s,%d,%d,%d,%.6f,%d\n" name model
                  k1 b q.index (i + 1) r.docId r.score (grade_of q r.docId))
              (take depth ranking))
          ranked;
        runs :=
          { preprocessing = name; model; params; per_query;
            summary = Metrics.aggregate per_query }
          :: !runs
      in
      evaluate ~model:"vector" ~params:None (fun query -> Env.score env Env.Vector query);
      List.iter
        (fun k1 ->
          List.iter
            (fun b ->
              let params : Model.bm25_params = { k1; b } in
              evaluate ~model:"bm25" ~params:(Some params) (fun query ->
                  Env.score env (Env.Bm25 params) query))
            b_values)
        k1_values)
    preprocessing_configs;
  Out_channel.close rankings_oc;
  let runs = List.rev !runs in
  let summary_oc = open_out "summary.csv" in
  Printf.fprintf summary_oc "preprocessing,model,k1,b,P@10,R@10,F1@10,MAP,MAP@10,MRR,NDCG@10\n";
  let per_query_oc = open_out "per_query.csv" in
  Printf.fprintf per_query_oc
    "preprocessing,model,k1,b,query,P@10,R@10,F1@10,AP@all,AP@10,RR,NDCG@10\n";
  List.iter
    (fun r ->
      let k1, b = csv_params r.params in
      let s = r.summary in
      Printf.fprintf summary_oc "%s,%s,%s,%s,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f\n"
        r.preprocessing r.model k1 b s.mean_p10 s.mean_r10 s.mean_f1_10 s.map s.map10 s.mrr
        s.mean_ndcg10;
      List.iter
        (fun (m : Metrics.query_metrics) ->
          Printf.fprintf per_query_oc "%s,%s,%s,%s,%d,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f\n"
            r.preprocessing r.model k1 b m.query m.p10 m.r10 m.f1_10 m.ap m.ap10 m.rr m.ndcg10)
        r.per_query)
    runs;
  Out_channel.close summary_oc;
  Out_channel.close per_query_oc;
  runs
