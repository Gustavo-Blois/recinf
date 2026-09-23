open Recinf

let usage =
  {|recinf - vector model and BM25 over the Cranfield collection

  recinf experiments                 run every configuration; writes results/{summary,per_query,rankings}.csv
  recinf compare [--metric M] [--preproc --k1 --b]
                                     item 5: vector vs BM25 for all 36 configurations (by M, default MAP),
                                     every metric for one configuration, and the queries with the largest
                                     differences: one pair of tables per metric, each ranked by its own metric
                                     (only M's with --metric). M = ap | ap10 | p10 | r10 | f1 | rr | ndcg10.
                                     --top N: rows per table (default 5, or 10 with --metric).
                                     configuration default: stopwords+stemming, k1=1.2, b=0.75.
                                     Writes results/{comparison_summary,largest_differences}.csv
  recinf cases [--metric M] [--preproc --k1 --b] [--top N] [--detail N]
                                     item 6: candidate queries in three categories (bm25 better, vector better,
                                     both poor); --top N per category (default 10); --detail N also prints the
                                     top-5 of both models for the first N of each. Writes results/{candidates.csv,case_studies.txt}
  recinf b-effect [Q] [--preproc --k1] [--top N]
                                     item 7: queries whose AP changes most with b (k1 fixed; default 1.2);
                                     with Q, the top-10 of that query under b=0 and b=1. Writes results/b_sensitivity.csv
  recinf analyze                     compare + cases + b-effect in one go (same options)
  recinf plot                        charts (PNG + PDF) and their gnuplot scripts in results/plots/;
                                     needs the `gnuplot` program; --preproc/--k1/--b choose the comparison
  recinf show Q [--text T]           top-N of both models for query Q (T replaces the query text,
                                     keeping the qrels of Q)
  recinf explain Q DOC [--text T]    term-by-term score breakdown of DOC under both models
  recinf modify Q TEXT               item 8: original vs modified query, both models
  recinf modify --file F             same for every ".I <id>" / ".T <text>" entry of F (.I ids, as in cran.qry); writes results/modifications.csv

options (show / explain / modify):
  --preproc raw|stopwords|stemming|stopwords+stemming   (default stopwords+stemming)
  --k1 X --b Y                                          BM25 parameters (default 1.2 / 0.75)
  --top N                                               rows per ranking (default 10)
  --file F                                              modifications file
  --by-id                                               the query number is the ".I" id from cran.qry, not the position
  --color / --no-color                                  force colors on/off (default: on only in a terminal)
|}

let fail fmt = Printf.ksprintf (fun s -> prerr_endline s; exit 1) fmt

(* "--flag value" pairs and positional arguments *)
let parse args =
  let rec go pos flags = function
    | [] -> (List.rev pos, flags)
    | "--no-color" :: rest -> Ansi.enabled := false; go pos flags rest
    | "--color" :: rest -> Ansi.enabled := true; go pos flags rest
    | "--by-id" :: rest -> go pos (("--by-id", "1") :: flags) rest
    | f :: v :: rest when String.length f > 2 && String.sub f 0 2 = "--" -> go pos ((f, v) :: flags) rest
    | [ f ] when String.length f > 2 && String.sub f 0 2 = "--" -> fail "missing value for %s" f
    | p :: rest -> go (p :: pos) flags rest
  in
  go [] [] args

let flag flags name = List.assoc_opt name flags
let float_flag flags name default =
  match flag flags name with Some v -> float_of_string v | None -> default

let load () =
  let documents = Document.create_documents (Utils.words_of_file "data/cran.all.1400") in
  let judgments = Cranqrel.judgments_table "data/cranqrel" in
  let queries = Query.create_query (Utils.words_of_file "data/cran.qry") judgments in
  (documents, queries)

let bm25_params flags : Model.bm25_params =
  { k1 = float_flag flags "--k1" Experiment.default_bm25.k1; b = float_flag flags "--b" Experiment.default_bm25.b }

let env_of flags ~documents ~queries =
  let name = Option.value ~default:"stopwords+stemming" (flag flags "--preproc") in
  Env.build ~documents ~queries (Experiment.preprocessing_of_name name)

(* Queries are numbered 1..225 by position in cran.qry (the numbering of the
   qrels); with --by-id the number is the ".I" id written in cran.qry instead. *)
let resolve_query flags (queries : Query.query list) n =
  if flag flags "--by-id" <> None then
    match List.find_opt (fun (q : Query.query) -> q.original_id = n) queries with
    | Some q -> q.index
    | None -> fail "no query with \".I %d\" in cran.qry" n
  else if n >= 1 && n <= List.length queries then n
  else fail "no query %d (valid: 1..%d by position; use --by-id for the .I ids)" n (List.length queries)

let int_arg what s = match int_of_string_opt s with Some n -> n | None -> fail "%s must be an integer, got %S" what s

let print_runs runs =
  Printf.printf "%-20s %-7s %-4s %-5s %6s %6s %6s %6s %7s %6s %6s\n" "preprocessing" "model" "k1" "b"
    "P@10" "R@10" "F1@10" "MAP" "MAP@10" "MRR" "NDCG10";
  List.iter
    (fun (r : Experiment.run) ->
      let k1, b = Experiment.csv_params r.params in
      let s = r.summary in
      Printf.printf "%-20s %-7s %-4s %-5s %6.4f %6.4f %6.4f %6.4f %7.4f %6.4f %6.4f\n" r.preprocessing
        r.model k1 b s.mean_p10 s.mean_r10 s.mean_f1_10 s.map s.map10 s.mrr s.mean_ndcg10)
    runs

let colored_delta ?(fmt = "%+.4f") d =
  let t = Printf.sprintf (Scanf.format_from_string fmt "%f") d in
  if d > 1e-9 then Ansi.green t else if d < -1e-9 then Ansi.red t else Ansi.dim t

let metric_flag flags =
  match flag flags "--metric" with
  | None -> Metrics.Ap
  | Some m -> (
      match Metrics.metric_of_string m with
      | Some m -> m
      | None -> fail "unknown metric %S (ap | ap10 | p10 | r10 | f1 | rr | ndcg10)" m)

(* the 36 comparisons for one metric *)
let print_stats ~metric stats =
  let mean = Metrics.mean_name metric in
  Printf.printf "%s\n%s\n" (Ansi.bold (Printf.sprintf "Vector vs BM25 by %s, all configurations" mean))
    (Ansi.dim "delta = BM25 - vector | bm25> / vec> = queries won by each | wilcoxon_p on per-query differences");
  Printf.printf "%-20s %-4s %-5s %9s %9s %8s %6s %6s %5s %10s\n" "preprocessing" "k1" "b"
    (mean ^ "_vec") (mean ^ "_bm25") "delta" "bm25>" "vec>" "ties" "wilcoxon_p";
  List.iter
    (fun (s : Analysis.stat) ->
      if s.metric = metric then begin
        let c = s.comp in
        let m a = Analysis.mean (Metrics.value metric) a in
        Printf.printf "%-20s %-4g %-5g %9.4f %9.4f %s %6d %6d %5d %10.3g\n" c.preprocessing c.params.k1
          c.params.b (m c.vector) (m c.bm25)
          (colored_delta ~fmt:"%+8.4f" (m c.bm25 -. m c.vector))
          s.wins_bm25 s.wins_vector s.ties s.p_value
      end)
    stats

(* every metric for one configuration *)
let print_metrics_table stats (comp : Analysis.comparison) =
  Printf.printf "\n%s\n%s\n"
    (Ansi.bold (Printf.sprintf "All metrics: %s, BM25 k1=%g b=%g vs vector" comp.preprocessing comp.params.k1 comp.params.b))
    (Ansi.dim "delta = BM25 - vector | wins counted per query | clear = difference >= 0.1");
  Printf.printf "%-8s %9s %9s %8s %6s %6s %5s %6s %6s %10s\n" "metric" "vector" "bm25" "delta" "bm25>" "vec>"
    "ties" "clear+" "clear-" "wilcoxon_p";
  List.iter
    (fun (s : Analysis.stat) ->
      if s.comp.preprocessing = comp.preprocessing && s.comp.params = comp.params then begin
        let m a = Analysis.mean (Metrics.value s.metric) a in
        Printf.printf "%-8s %9.4f %9.4f %s %6d %6d %5d %6d %6d %10.3g\n" (Metrics.mean_name s.metric)
          (m comp.vector) (m comp.bm25) (colored_delta ~fmt:"%+8.4f" (m comp.bm25 -. m comp.vector))
          s.wins_bm25 s.wins_vector s.ties s.clear_bm25 s.clear_vector s.p_value
      end)
    stats

let comparison_of_flags runs flags =
  let preprocessing = Option.value ~default:"stopwords+stemming" (flag flags "--preproc") in
  ignore (Experiment.preprocessing_of_name preprocessing);
  let params = bm25_params flags in
  match
    List.find_opt
      (fun (c : Analysis.comparison) -> c.preprocessing = preprocessing && c.params = params)
      (Analysis.comparisons runs)
  with
  | Some c -> c
  | None -> fail "no such BM25 configuration: k1=%g b=%g (grid: k1 in 0.5,1.2,2; b in 0,0.75,1)" params.k1 params.b

(* One pair of tables (BM25 better / vector better) per metric: the queries with
   the largest differences depend on the metric used to rank them. *)
let print_differences ~output_dir ~documents ~queries ~metrics ~top (comp : Analysis.comparison) =
  let results = Analysis.largest_differences ~documents ~queries ~metrics comp ~n:10 in
  Analysis.write_largest_differences ~output_dir comp results;
  let shown = List.filter (fun m -> m <> Metrics.F1) Metrics.all_metrics in
  Printf.printf "\n%s\n%s\n"
    (Ansi.bold
       (Printf.sprintf "Largest per-query differences: %s, BM25 k1=%g b=%g" comp.preprocessing comp.params.k1
          comp.params.b))
    (Ansi.dim
       "One pair of tables per metric: each is ranked by that metric's difference (ties: by dAP@all).\n\
        Q = query number (position in cran.qry, as in the qrels) | .I = id written in cran.qry\n\
        dX = X_bm25 - X_vector, for each metric (do they agree?)\n\
        rel = number of relevant documents | terms = query terms after preprocessing\n\
        len = mean length of the relevant docs / collection average (< 1: shorter than average)");
  List.iter
    (fun (metric, rows) ->
      List.iter
        (fun (cat, title) ->
          Printf.printf "\n%s\n"
            (Ansi.bold
               (Ansi.cyan
                  (Printf.sprintf "[ranked by d%s] %s (%s, k1=%g b=%g)" (Metrics.name metric) title
                     comp.preprocessing comp.params.k1 comp.params.b)));
          Printf.printf "%s\n"
            (Ansi.dim
               (Printf.sprintf "  %3s %5s %s %5s %5s %5s  %s" "Q" ".I"
                  (String.concat " " (List.map (fun m -> Printf.sprintf "%8s" ("d" ^ Metrics.name m)) shown))
                  "rel" "terms" "len" "text"));
          List.iter
            (fun (d : Analysis.difference) ->
              if d.category = cat && d.position <= top then
                Printf.printf "  %3d %5d %s %5d %5d %5.2f  %s\n" d.query.index d.query.original_id
                  (String.concat " "
                     (List.map
                        (fun m ->
                          let t = Printf.sprintf "%+8.3f" (Metrics.value m d.bm -. Metrics.value m d.vec) in
                          let t = if m = metric then Ansi.bold t else t in
                          let v = Metrics.value m d.bm -. Metrics.value m d.vec in
                          if v > 1e-9 then Ansi.green t else if v < -1e-9 then Ansi.red t else Ansi.dim t)
                        shown))
                  d.n_relevant d.n_terms d.length_ratio
                  (Display.truncate 50 (Display.raw_text d.query)))
            rows)
        [ (Analysis.Bm25_better, "BM25 much better than vector"); (Analysis.Vector_better, "Vector much better than BM25") ])
    results

(* Item 6: candidates of the three categories. "Both poor" always uses AP@all
   (max of the two models < Analysis.poor_ap); --metric only changes how the two
   "better" categories are ranked. [detail] queries per category also get the
   top-5 of both models. *)
let print_cases ~documents ~queries ~metric ~top ~detail (comp : Analysis.comparison) =
  let env = Env.build ~documents ~queries (Experiment.preprocessing_of_name comp.preprocessing) in
  let n_relevant i = List.length (List.nth queries i).relevant_documents in
  Printf.printf "%s\n%s\n"
    (Ansi.bold
       (Printf.sprintf "Item 6 candidates: %s, BM25 k1=%g b=%g vs vector" comp.preprocessing comp.params.k1
          comp.params.b))
    (Ansi.dim
       (Printf.sprintf
          "Q = query number (position in cran.qry) | .I = id written in cran.qry\n\
           AP = AP@all | dAP = AP_bm25 - AP_vec | 1st rel = rank of the first relevant document (- = none retrieved) | rel = relevant docs\n\
           bm25 better / vector better: ranked by d%s = bm25 - vector (ties: dAP)\n\
           both poor: max(AP vector, AP bm25) < %g, worst first (ties: more relevant docs first)"
          (Metrics.name metric) Analysis.poor_ap));
  List.iter
    (fun cat ->
      let all = Analysis.candidate_positions ~metric comp ~n_relevant cat in
      Printf.printf "\n%s\n"
        (Ansi.bold
           (Ansi.cyan
              (Printf.sprintf "[%s] %d candidates in total (%s, k1=%g b=%g)" (Analysis.category_name cat)
                 (List.length all) comp.preprocessing comp.params.k1 comp.params.b)));
      Printf.printf "%s\n"
        (Ansi.dim
           (Printf.sprintf "  %3s %5s %8s %8s %8s %8s %8s %4s %5s  %s" "Q" ".I" "AP_vec" "AP_bm25" "dAP" "1st_vec"
              "1st_bm25" "rel" "terms" "text"));
      let shown = List.filteri (fun r _ -> r < top) all in
      List.iter
        (fun i ->
          let raw = List.nth queries i in
          let q = Env.query env raw.index in
          let v = Env.ranked env Env.Vector q and b = Env.ranked env (Env.Bm25 comp.params) q in
          Printf.printf "  %3d %5d %8.4f %8.4f %s %8s %8s %4d %5d  %s\n" raw.index raw.original_id
            comp.vector.(i).ap comp.bm25.(i).ap
            (colored_delta ~fmt:"%+8.4f" (Analysis.delta_ap comp i))
            (Display.first_relevant q v) (Display.first_relevant q b)
            (n_relevant i) (List.length q.text) (Display.truncate 50 (Display.raw_text raw)))
        shown;
      List.iteri
        (fun r i ->
          if r < detail then begin
            let raw = List.nth queries i in
            let q = Env.query env raw.index in
            Printf.printf "\n%s [%s]\n%s" (Ansi.bold (Printf.sprintf "Q%d in detail" raw.index))
              (Analysis.category_name cat) (Display.query_header env ~raw q);
            print_string (Display.both_models env ~bm25:comp.params ~query:q ~top:5)
          end)
        shown)
    Analysis.categories

(* Item 7: queries whose AP moves most with b (k1 fixed). With a query number,
   its top of the ranking under b=0 and b=1. *)
let print_b_effect ~documents ~queries ~preprocessing ~k1 ~top ~detail =
  Printf.printf "%s\n%s\n"
    (Ansi.bold (Printf.sprintf "Effect of b on BM25: %s, k1=%g" preprocessing k1))
    (Ansi.dim
       "AP = AP@all with b = 0 / 0.75 / 1 | spread = max AP - min AP | overlap = docs shared by the top 10 with b=0 and b=1\n\
        len = mean length of the relevant docs / collection average (< 1: shorter than average)\n\
        rel = relevant docs | terms = query terms after preprocessing");
  Printf.printf "%s\n"
    (Ansi.dim
       (Printf.sprintf "  %3s %5s %7s %7s %7s %7s %8s %5s %4s %5s  %s" "Q" ".I" "AP_b0" "AP_b.75" "AP_b1"
          "spread" "overlap" "len" "rel" "terms" "text"));
  let rows = Analysis.b_effect ~documents ~queries ~preprocessing ~k1 in
  List.iteri
    (fun r (x : Analysis.b_row) ->
      if r < top then
        Printf.printf "  %3d %5d %7.4f %7.4f %7.4f %7.4f %5d/10 %5.2f %4d %5d  %s\n" x.b_query.index
          x.b_query.original_id x.ap_b0 x.ap_b75 x.ap_b1 x.b_spread x.b_overlap x.b_len_ratio
          x.b_relevant x.b_terms (Display.truncate 50 (Display.raw_text x.b_query)))
    rows;
  match detail with
  | None -> ()
  | Some qn ->
      let env = Env.build ~documents ~queries (Experiment.preprocessing_of_name preprocessing) in
      let q = Env.query env qn in
      let raw = List.nth queries (qn - 1) in
      let ranking b = Env.ranked env (Env.Bm25 { Model.k1; b }) q in
      let r0 = ranking 0.0 and r1 = ranking 1.0 in
      Printf.printf "\n%s\n" (Display.query_header env ~raw q);
      List.iter
        (fun (b, r, other) ->
          print_string
            (Display.model_section env ~label:(Printf.sprintf "BM25 (k1=%g, b=%g)" k1 b) ~query:q ~ranking:r
               ~other ~top:10);
          print_newline ())
        [ (0.0, r0, r1); (1.0, r1, r0) ]

let () =
  (match Utils.find_project_root () with
  | Some root -> Sys.chdir root
  | None -> fail "cannot find data/cran.all.1400 from %s: run recinf inside the project" (Sys.getcwd ()));
  let output_dir = "results" in
  match Array.to_list Sys.argv |> List.tl with
  | [] | ("help" | "--help" | "-h") :: _ -> print_string usage
  | "experiments" :: _ ->
      let documents, queries = load () in
      print_runs (Experiment.run_all ~documents ~queries ~output_dir);
      print_endline "\nCSV files written to results/"
  | ("compare" | "cases" | "b-effect" | "analyze") as cmd :: rest ->
      let pos, flags = parse rest in
      let documents, queries = load () in
      let runs = Experiment.run_all ~documents ~queries ~output_dir in
      let all = cmd = "analyze" in
      let comp () = comparison_of_flags runs flags in
      if cmd = "compare" || all then begin
        let stats = Analysis.compare_models ~output_dir runs in
        (* --metric M: only that metric; otherwise the 36-row table is by MAP and the
           largest differences are listed for every metric *)
        let metrics = match flag flags "--metric" with None -> Metrics.all_metrics | Some _ -> [ metric_flag flags ] in
        let top =
          match flag flags "--top" with Some n -> int_arg "--top" n | None -> if List.length metrics = 1 then 10 else 5
        in
        print_stats ~metric:(metric_flag flags) stats;
        print_metrics_table stats (comp ());
        print_differences ~output_dir ~documents ~queries ~metrics ~top (comp ());
        print_endline "\nwrote results/comparison_summary.csv and results/largest_differences.csv"
      end;
      if cmd = "cases" || all then begin
        if all then print_newline ();
        Analysis.write_cases ~output_dir ~documents ~queries runs;
        let top = match flag flags "--top" with Some n -> int_arg "--top" n | None -> 10 in
        let detail = match flag flags "--detail" with Some n -> int_arg "--detail" n | None -> 0 in
        print_cases ~documents ~queries ~metric:(metric_flag flags) ~top ~detail (comp ());
        print_endline "\nwrote results/candidates.csv and results/case_studies.txt"
      end;
      if cmd = "b-effect" || all then begin
        if all then print_newline ();
        Analysis.write_b_sensitivity ~output_dir ~documents ~queries runs;
        let preprocessing = Option.value ~default:"stopwords+stemming" (flag flags "--preproc") in
        ignore (Experiment.preprocessing_of_name preprocessing);
        let top = match flag flags "--top" with Some n -> int_arg "--top" n | None -> 10 in
        let detail =
          match pos with
          | [ q ] when cmd = "b-effect" -> Some (resolve_query flags queries (int_arg "query" q))
          | _ -> None
        in
        print_b_effect ~documents ~queries ~preprocessing ~k1:(float_flag flags "--k1" Experiment.default_bm25.k1)
          ~top ~detail;
        print_endline "\nwrote results/b_sensitivity.csv"
      end
  | "plot" :: rest ->
      let _, flags = parse rest in
      let documents, queries = load () in
      let runs = Experiment.run_all ~documents ~queries ~output_dir in
      let preprocessing = Option.value ~default:"stopwords+stemming" (flag flags "--preproc") in
      ignore (Experiment.preprocessing_of_name preprocessing);
      let dir = Plots.all ~output_dir ~queries runs ~preprocessing ~params:(bm25_params flags) in
      Printf.printf "charts written to %s/ (*.png, *.pdf and the *.gp scripts)\n" dir
  | "show" :: rest -> (
      let pos, flags = parse rest in
      match pos with
      | [ q ] ->
          let documents, queries = load () in
          let env = env_of flags ~documents ~queries in
          let raw = List.nth queries (resolve_query flags queries (int_arg "query" q) - 1) in
          let query = Env.query env raw.index in
          let query = match flag flags "--text" with Some t -> Env.with_text env query t | None -> query in
          print_string (Display.query_header env ~raw query);
          print_newline ();
          let top = match flag flags "--top" with Some n -> int_arg "--top" n | None -> 10 in
          print_string (Display.both_models env ~bm25:(bm25_params flags) ~query ~top)
      | _ -> fail "usage: recinf show Q [--text T] [options]")
  | "explain" :: rest -> (
      let pos, flags = parse rest in
      match pos with
      | [ q; d ] ->
          let documents, queries = load () in
          let env = env_of flags ~documents ~queries in
          let query = Env.query env (resolve_query flags queries (int_arg "query" q)) in
          let query = match flag flags "--text" with Some t -> Env.with_text env query t | None -> query in
          let doc = int_arg "doc" d in
          let d = Env.doc env doc in
          let grade = match List.assoc_opt doc query.judgments with Some g -> Printf.sprintf "relevant (grade %d)" g | None -> "not relevant" in
          Printf.printf "%s\n  doc %d: %s  [%s]\n  query terms: %s\n\n" (Ansi.bold (Printf.sprintf "Query %d" query.index)) doc
            (Display.title d) grade (String.concat " " query.text);
          print_string (Explain.bm25 env ~params:(bm25_params flags) ~query doc);
          print_newline ();
          print_string (Explain.vector env ~query doc)
      | _ -> fail "usage: recinf explain Q DOC [--text T] [options]")
  | "modify" :: rest -> (
      let pos, flags = parse rest in
      let documents, queries = load () in
      let env = env_of flags ~documents ~queries in
      let models = [ Env.Vector; Env.Bm25 (bm25_params flags) ] in
      let one ?(flags = flags) q text =
        let raw = List.nth queries (resolve_query flags queries q - 1) in
        Modify.run env ~models ~raw ~text
      in
      match (pos, flag flags "--file") with
      | [ q; text ], None -> print_string (fst (one (int_arg "query" q) text))
      | [], Some file ->
          let rows =
            List.concat_map
              (fun (q, text) ->
                (* the ids in the file are always the ".I" ids *)
                let s, rows = one ~flags:(("--by-id", "1") :: flags) q text in
                print_string (String.make 100 '=' ^ "\n");
                print_string s;
                rows)
              (Modify.read_file file)
          in
          if not (Sys.file_exists output_dir) then Sys.mkdir output_dir 0o755;
          Modify.write_csv (Filename.concat output_dir "modifications.csv") rows;
          print_endline "wrote results/modifications.csv"
      | _ -> fail "usage: recinf modify Q TEXT | recinf modify --file F")
  | cmd :: _ -> fail "unknown command %S\n\n%s" cmd usage
