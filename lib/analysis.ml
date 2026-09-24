(* Items 5 and 6: vector vs BM25, per configuration and per query.
   Every BM25 configuration is compared with the vector model under the same
   preprocessing (4 preprocessings x 9 (k1, b) = 36 comparisons). *)

let eps = 1e-9
let clear_gap = 0.1  (* |delta AP| >= this counts as a "clear" win *)
let poor_ap = 0.1  (* "both poor": max(AP@all vector, AP@all bm25) below this *)
let candidates_per_category = 10
let case_studies_per_category = 5
let case_study_depth = 5

type comparison = {
  preprocessing : string;
  params : Model.bm25_params;
  vector : Metrics.query_metrics array;
  bm25 : Metrics.query_metrics array;
}

let comparisons (runs : Experiment.run list) =
  List.concat_map
    (fun (vr : Experiment.run) ->
      if vr.model <> "vector" then []
      else
        List.filter_map
          (fun (r : Experiment.run) ->
            match r.params with
            | Some params when r.preprocessing = vr.preprocessing ->
                Some
                  { preprocessing = vr.preprocessing; params;
                    vector = Array.of_list vr.per_query; bm25 = Array.of_list r.per_query }
            | _ -> None)
          runs)
    runs

let mean f a = Array.fold_left (fun s m -> s +. f m) 0.0 a /. float_of_int (Array.length a)
let delta_metric metric c i = Metrics.value metric c.bm25.(i) -. Metrics.value metric c.vector.(i)
let delta_ap c i = delta_metric Metrics.Ap c i

type stat = {
  comp : comparison;
  metric : Metrics.metric;
  wins_bm25 : int;
  wins_vector : int;
  ties : int;
  clear_bm25 : int;
  clear_vector : int;
}

let stat ?(metric = Metrics.Ap) comp =
  let n = Array.length comp.vector in
  let ds = List.init n (delta_metric metric comp) in
  let count p = List.length (List.filter p ds) in
  { comp; metric;
    wins_bm25 = count (fun d -> d > eps);
    wins_vector = count (fun d -> d < -.eps);
    ties = count (fun d -> Float.abs d <= eps);
    clear_bm25 = count (fun d -> d >= clear_gap);
    clear_vector = count (fun d -> d <= -.clear_gap) }

type category = Bm25_better | Vector_better | Both_poor

let category_name = function
  | Bm25_better -> "bm25_better"
  | Vector_better -> "vector_better"
  | Both_poor -> "both_poor"

let categories = [ Bm25_better; Vector_better; Both_poor ]

(* Positions (0-based) in the comparison arrays, best candidate first. *)
let candidate_positions ?(metric = Metrics.Ap) comp ~n_relevant category =
  let n = Array.length comp.vector in
  let all = List.init n Fun.id in
  let by_delta sign =
    all
    |> List.filter (fun i -> sign *. delta_metric metric comp i > eps)
    |> List.sort (fun a b ->
           (* P@10, R@10, F1@10 and RR take few distinct values: break ties by the
              difference in AP@all, then by query position *)
           let by m i = sign *. delta_metric m comp i in
           match Float.compare (by metric b) (by metric a) with
           | 0 -> (
               match Float.compare (by Metrics.Ap b) (by Metrics.Ap a) with
               | 0 -> Int.compare a b
               | c -> c)
           | c -> c)
  in
  match category with
  | Bm25_better -> by_delta 1.0
  | Vector_better -> by_delta (-1.0)
  | Both_poor ->
      let best i = Float.max comp.vector.(i).ap comp.bm25.(i).ap in
      all
      |> List.filter (fun i -> best i < poor_ap)
      |> List.sort (fun a b ->
             match Float.compare (best a) (best b) with
             | 0 -> (
                 match Int.compare (n_relevant b) (n_relevant a) with
                 | 0 -> Int.compare a b
                 | c -> c)
             | c -> c)

let rec take k = function x :: r when k > 0 -> x :: take (k - 1) r | _ -> []

(* One row per (configuration, metric): mean of each model, difference, per-query
   wins/losses/ties and Wilcoxon p-value on the per-query differences. *)
let write_summary ~output_dir stats =
  let oc = Out_channel.open_text (Filename.concat output_dir "comparison_summary.csv") in
  Printf.fprintf oc
    "preprocessing,k1,b,metric,mean_vector,mean_bm25,delta,wins_bm25,wins_vector,ties,clear_bm25,clear_vector\n";
  List.iter
    (fun s ->
      let c = s.comp in
      let mv = mean (Metrics.value s.metric) c.vector and mb = mean (Metrics.value s.metric) c.bm25 in
      Printf.fprintf oc "%s,%g,%g,%s,%.4f,%.4f,%+.4f,%d,%d,%d,%d,%d\n" c.preprocessing
        c.params.k1 c.params.b (Metrics.mean_name s.metric) mv mb (mb -. mv) s.wins_bm25
        s.wins_vector s.ties s.clear_bm25 s.clear_vector)
    stats;
  Out_channel.close oc

let write_candidates ~output_dir ~(queries : Query.query list) comps =
  let by_index = Hashtbl.create 256 in
  List.iter (fun (q : Query.query) -> Hashtbl.replace by_index q.index q) queries;
  let n_relevant i = List.length (Hashtbl.find by_index (i + 1)).relevant_documents in
  let oc = Out_channel.open_text (Filename.concat output_dir "candidates.csv") in
  Printf.fprintf oc
    "preprocessing,k1,b,category,rank,query,cran_id,AP@all_vector,AP@all_bm25,dAP@all,P@10_vector,P@10_bm25,n_relevant,n_terms,text\n";
  List.iter
    (fun c ->
      List.iter
        (fun cat ->
          List.iteri
            (fun r i ->
              let q = Hashtbl.find by_index (i + 1) in
              Printf.fprintf oc "%s,%g,%g,%s,%d,%d,%d,%.4f,%.4f,%+.4f,%.2f,%.2f,%d,%d,\"%s\"\n"
                c.preprocessing c.params.k1 c.params.b (category_name cat) (r + 1) q.index q.original_id
                c.vector.(i).ap c.bm25.(i).ap (delta_ap c i) c.vector.(i).p10 c.bm25.(i).p10
                (n_relevant i) (List.length q.text) (Display.raw_text q))
            (take candidates_per_category (candidate_positions c ~n_relevant cat)))
        categories)
    comps;
  Out_channel.close oc

(* Plain-text top-5 of both models for the best candidates of every category,
   for every configuration. *)
let write_case_studies ~output_dir ~documents ~(queries : Query.query list) comps =
  let envs = Hashtbl.create 4 in
  let env_for name =
    match Hashtbl.find_opt envs name with
    | Some e -> e
    | None ->
        let e = Env.build ~documents ~queries (Experiment.preprocessing_of_name name) in
        Hashtbl.replace envs name e;
        e
  in
  let by_index = Hashtbl.create 256 in
  List.iter (fun (q : Query.query) -> Hashtbl.replace by_index q.index q) queries;
  let n_relevant i = List.length (Hashtbl.find by_index (i + 1)).relevant_documents in
  let oc = Out_channel.open_text (Filename.concat output_dir "case_studies.txt") in
  Ansi.without_color (fun () ->
      List.iter
        (fun c ->
          let env = env_for c.preprocessing in
          List.iter
            (fun cat ->
              List.iteri
                (fun r i ->
                  let raw = Hashtbl.find by_index (i + 1) in
                  let q = Env.query env raw.index in
                  Printf.fprintf oc
                    "%s\n[%s | k1=%g b=%g | %s #%d]  dAP@all(bm25-vector) = %+.4f\n%s\n%s\n"
                    (String.make 100 '=') c.preprocessing c.params.k1 c.params.b
                    (category_name cat) (r + 1) (delta_ap c i)
                    (Display.query_header env ~raw q)
                    (Display.both_models env ~bm25:c.params ~query:q ~top:case_study_depth))
                (take case_studies_per_category (candidate_positions c ~n_relevant cat)))
            categories)
        comps);
  Out_channel.close oc

(* Mean length of a query's relevant documents relative to the collection
   average (< 1: shorter than average). Length normalisation favours these. *)
let rel_len_ratio (env : Env.t) (q : Query.query) =
  let lens = List.map (fun d -> Hashtbl.find env.stats.doc_lengths d) q.relevant_documents in
  List.fold_left ( +. ) 0.0 lens /. float_of_int (List.length lens) /. env.stats.average_document_len

(* Item 7: queries whose AP moves the most when only b changes (k1 fixed),
   with the columns that help explain it: how long the relevant documents are
   compared with the average, and how much the top 10 changes between b=0 and
   b=1. *)
let b_sensitivity_per_group = 10

let write_b_sensitivity ~output_dir ~documents ~(queries : Query.query list) (runs : Experiment.run list) =
  let oc = Out_channel.open_text (Filename.concat output_dir "b_sensitivity.csv") in
  Printf.fprintf oc
    "preprocessing,k1,query,cran_id,AP@all_b0,AP@all_b0.75,AP@all_b1,spread,rel_len_ratio,top10_overlap_b0_b1,n_terms,n_relevant,text\n";
  List.iter
    (fun cfg ->
      let name = Experiment.preprocessing_name cfg in
      let env = Env.build ~documents ~queries cfg in
      List.iter
        (fun k1 ->
          let ap b =
            let r =
              List.find
                (fun (r : Experiment.run) ->
                  r.preprocessing = name && r.model = "bm25" && r.params = Some { Model.k1; b })
                runs
            in
            Array.of_list r.per_query
          in
          let a0 = ap 0.0 and a75 = ap 0.75 and a1 = ap 1.0 in
          let spread i =
            let l = [ a0.(i).ap; a75.(i).ap; a1.(i).ap ] in
            List.fold_left Float.max 0.0 l -. List.fold_left Float.min 1.0 l
          in
          let ranked =
            List.init (Array.length a0) Fun.id
            |> List.sort (fun x y ->
                   match Float.compare (spread y) (spread x) with 0 -> Int.compare x y | c -> c)
          in
          List.iter
            (fun i ->
              let q = Env.query env (i + 1) in
              let ratio = rel_len_ratio env q in
              let top b =
                Env.ranked env (Env.Bm25 { Model.k1; b }) q |> Display.doc_ids |> take 10
              in
              let t0 = top 0.0 and t1 = top 1.0 in
              let overlap = List.length (List.filter (fun d -> List.mem d t1) t0) in
              let raw = List.nth queries i in
              Printf.fprintf oc "%s,%g,%d,%d,%.4f,%.4f,%.4f,%.4f,%.3f,%d,%d,%d,\"%s\"\n" name k1
                q.index q.original_id a0.(i).ap a75.(i).ap a1.(i).ap (spread i) ratio overlap
                (List.length raw.text) (List.length q.relevant_documents) (Display.raw_text raw))
            (take b_sensitivity_per_group ranked))
        Experiment.k1_values)
    Experiment.preprocessing_configs;
  Out_channel.close oc

(* Item 5: the queries where the two models differ the most, in each direction,
   with what helps to explain it: query length after preprocessing, number of
   relevant documents and how long they are. *)
type difference = {
  category : category;
  position : int;  (* 1 = largest difference *)
  query : Query.query;  (* raw query, for its text *)
  vec : Metrics.query_metrics;
  bm : Metrics.query_metrics;
  n_terms : int;
  n_relevant : int;
  length_ratio : float;
}

(* Ranked by the difference in [metric]; every metric is kept so the caller can
   show whether they agree. *)
let largest_differences ~documents ~(queries : Query.query list) ~metrics comp ~n =
  let env = Env.build ~documents ~queries (Experiment.preprocessing_of_name comp.preprocessing) in
  let n_relevant i = List.length (List.nth queries i).relevant_documents in
  List.map
    (fun metric ->
      ( metric,
        List.concat_map
    (fun category ->
      List.mapi
        (fun r i ->
          let raw = List.nth queries i in
          let q = Env.query env raw.index in
          { category; position = r + 1; query = raw; vec = comp.vector.(i); bm = comp.bm25.(i);
            n_terms = List.length q.text; n_relevant = List.length q.relevant_documents;
            length_ratio = rel_len_ratio env q })
        (take n (candidate_positions ~metric comp ~n_relevant category)))
    [ Bm25_better; Vector_better ] ))
    metrics

(* [results]: for each metric, its rows; the CSV has all of them (see ranked_by). *)
let write_largest_differences ~output_dir comp results =
  let oc = Out_channel.open_text (Filename.concat output_dir "largest_differences.csv") in
  let cols = String.concat "," (List.map (fun m -> Metrics.name m ^ "_vector," ^ Metrics.name m ^ "_bm25") Metrics.all_metrics) in
  Printf.fprintf oc "preprocessing,k1,b,ranked_by,direction,position,query,cran_id,%s,n_terms,n_relevant,rel_len_ratio,text\n" cols;
  List.iter
    (fun (metric, rows) ->
      List.iter
        (fun d ->
          let vals =
            String.concat ","
              (List.map (fun m -> Printf.sprintf "%.4f,%.4f" (Metrics.value m d.vec) (Metrics.value m d.bm)) Metrics.all_metrics)
          in
          Printf.fprintf oc "%s,%g,%g,%s,%s,%d,%d,%d,%s,%d,%d,%.3f,\"%s\"\n" comp.preprocessing comp.params.k1
            comp.params.b (Metrics.name metric) (category_name d.category) d.position d.query.index
            d.query.original_id vals d.n_terms d.n_relevant d.length_ratio (Display.raw_text d.query))
        rows)
    results;
  Out_channel.close oc

(* Item 7, terminal version: one query's AP under b = 0, 0.75 and 1 (k1 fixed),
   with the same explanatory columns as b_sensitivity.csv. *)
type b_row = {
  b_query : Query.query;  (* raw query, for its text *)
  ap_b0 : float;
  ap_b75 : float;
  ap_b1 : float;
  b_spread : float;  (* max - min of the three APs *)
  b_len_ratio : float;
  b_overlap : int;  (* documents shared by the top 10 with b=0 and with b=1 *)
  b_terms : int;
  b_relevant : int;
}

let b_effect ~documents ~(queries : Query.query list) ~preprocessing ~k1 =
  let env = Env.build ~documents ~queries (Experiment.preprocessing_of_name preprocessing) in
  List.map
    (fun (raw : Query.query) ->
      let q = Env.query env raw.index in
      let ranking b = Env.ranked env (Env.Bm25 { Model.k1; b }) q |> Display.doc_ids in
      let ap b = (Metrics.evaluate_query q (ranking b)).ap in
      let a0 = ap 0.0 and a75 = ap 0.75 and a1 = ap 1.0 in
      let t0 = take 10 (ranking 0.0) and t1 = take 10 (ranking 1.0) in
      { b_query = raw; ap_b0 = a0; ap_b75 = a75; ap_b1 = a1;
        b_spread = Float.max a0 (Float.max a75 a1) -. Float.min a0 (Float.min a75 a1);
        b_len_ratio = rel_len_ratio env q;
        b_overlap = List.length (List.filter (fun d -> List.mem d t1) t0);
        b_terms = List.length q.text;
        b_relevant = List.length q.relevant_documents })
    queries
  |> List.sort (fun x y ->
         match Float.compare y.b_spread x.b_spread with
         | 0 -> Int.compare x.b_query.index y.b_query.index
         | c -> c)

(* Not tied to one item: every query (not just the pre-picked candidates),
   both models' AP@all and P@10 under one configuration, so the group can
   browse and pick their own queries (e.g. for item 8) instead of only
   seeing the candidates this program already ranked for items 5-7. *)
let write_query_overview ~output_dir ~(queries : Query.query list) (comp : comparison) =
  let oc = Out_channel.open_text (Filename.concat output_dir "queries.csv") in
  Printf.fprintf oc "query,cran_id,n_terms,n_relevant,AP_vector,P10_vector,AP_bm25,P10_bm25,dAP,text\n";
  List.iteri
    (fun i (q : Query.query) ->
      Printf.fprintf oc "%d,%d,%d,%d,%.4f,%.2f,%.4f,%.2f,%+.4f,\"%s\"\n" q.index q.original_id
        (List.length q.text) (List.length q.relevant_documents) comp.vector.(i).ap comp.vector.(i).p10
        comp.bm25.(i).ap comp.bm25.(i).p10 (delta_ap comp i) (Display.raw_text q))
    queries;
  Out_channel.close oc

(* Item 5 (aggregate comparison): writes comparison_summary.csv. *)
let compare_models ~output_dir runs =
  let comps = comparisons runs in
  let stats =
    List.concat_map (fun c -> List.map (fun metric -> stat ~metric c) Metrics.all_metrics) comps
  in
  write_summary ~output_dir stats;
  stats

(* Item 6: candidate queries per category; writes candidates.csv and case_studies.txt. *)
let write_cases ~output_dir ~documents ~queries runs =
  let comps = comparisons runs in
  write_candidates ~output_dir ~queries comps;
  write_case_studies ~output_dir ~documents ~queries comps

let analyze ~output_dir ~documents ~queries runs =
  let stats = compare_models ~output_dir runs in
  write_cases ~output_dir ~documents ~queries runs;
  write_b_sensitivity ~output_dir ~documents ~queries runs;
  stats
