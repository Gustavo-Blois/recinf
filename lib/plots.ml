(* Charts. Each figure is a self-contained gnuplot script (data inlined) written
   to <output_dir>/plots/<name>.gp and rendered to PNG and PDF by the `gnuplot`
   program, so any figure can be tweaked by editing its script. *)

open Experiment

let vector_color = "#4c78a8"
let bm25_color = "#f58518"
let better_color = "#2a9d8f"
let worse_color = "#c44e52"

let short_name = function
  | "raw" -> "raw"
  | "stopwords" -> "stop"
  | "stemming" -> "stem"
  | _ -> "both"  (* stopwords + stemming *)

type metric = Map | P10 | R10 | Ndcg10

let metric_name = function Map -> "MAP" | P10 -> "P@10" | R10 -> "R@10" | Ndcg10 -> "NDCG@10"

(* "@" is the overlay operator in gnuplot's enhanced text, so it must be escaped
   in anything rendered as a title. *)
let gp_text s = String.concat "\\\\@" (String.split_on_char '@' s)

let metric_value (s : Metrics.summary) = function
  | Map -> s.map
  | P10 -> s.mean_p10
  | R10 -> s.mean_r10
  | Ndcg10 -> s.mean_ndcg10

let find runs ~preprocessing ~model ~params =
  List.find
    (fun r -> r.preprocessing = preprocessing && r.model = model && r.params = params)
    runs

let vector_run runs preprocessing = find runs ~preprocessing ~model:"vector" ~params:None
let bm25_run runs preprocessing params = find runs ~preprocessing ~model:"bm25" ~params:(Some params)

let block name rows = Printf.sprintf "$%s << EOD\n%s\nEOD\n" name (String.concat "\n" rows)

let preamble =
  {|set encoding utf8
set border 3 lw 1.2 lc rgb "#444444"
set tics nomirror scale 0.6 textcolor rgb "#333333"
set grid ytics lc rgb "#dddddd" lw 1
set key textcolor rgb "#333333"
|}

let render ~dir ~name ~w ~h body =
  let terminal =
    Printf.sprintf
      "if (ext eq \"png\") { set terminal pngcairo size %d,%d font \"Sans,17\" } else { set terminal pdfcairo size %g,%g font \"Sans,9\" }\nset output \"%s.\".ext\n"
      (int_of_float (w *. 150.)) (int_of_float (h *. 150.)) w h name
  in
  let gp = Filename.concat dir (name ^ ".gp") in
  Out_channel.with_open_text gp (fun oc -> output_string oc (terminal ^ preamble ^ body));
  List.iter
    (fun ext ->
      let err = Filename.concat dir (name ^ ".err") in
      let code =
        Sys.command
          (Printf.sprintf "cd %s && gnuplot -e \"ext='%s'\" %s 2> %s"
             (Filename.quote dir) ext (Filename.quote (name ^ ".gp")) (Filename.quote (name ^ ".err")))
      in
      let msg = String.trim (In_channel.with_open_text err In_channel.input_all) in
      Sys.remove err;
      if code <> 0 || msg <> "" then Printf.eprintf "gnuplot (%s.%s): %s\n%!" name ext msg)
    [ "png"; "pdf" ]

let max_of = List.fold_left Float.max 0.0

(* Grouped bars: vector vs BM25 (given params) for every preprocessing. *)
let fig_metrics ~dir runs (params : Model.bm25_params) =
  let metrics = [ (Map, "map"); (P10, "p10"); (R10, "r10") ] in
  let value m name model_run = metric_value (model_run runs name).summary m in
  let rows m =
    List.map
      (fun cfg ->
        let n = preprocessing_name cfg in
        Printf.sprintf "%s %.4f %.4f" (short_name n)
          (value m n vector_run)
          (value m n (fun runs n -> bm25_run runs n params)))
      preprocessing_configs
  in
  let blocks = String.concat "" (List.map (fun (m, id) -> block id (rows m)) metrics) in
  let panels =
    List.mapi
      (fun i (m, id) ->
        let top =
          1.25
          *. max_of
               (List.concat_map
                  (fun cfg ->
                    let n = preprocessing_name cfg in
                    [ value m n vector_run; value m n (fun r n -> bm25_run r n params) ])
                  preprocessing_configs)
        in
        Printf.sprintf
          "set title \"%s\"\nset yrange [0:%.3f]\n%splot $%s using 2:xtic(1) title \"Vetorial\" lc rgb \"%s\", '' using 3 title \"BM25\" lc rgb \"%s\"\n"
          (gp_text (metric_name m)) top
          (if i = 0 then "set key top left samplen 1.2\n" else "unset key\n")
          id vector_color bm25_color)
      metrics
    |> String.concat ""
  in
  render ~dir ~name:"metrics_by_preprocessing" ~w:11.0 ~h:3.8
    (blocks
    ^ Printf.sprintf
        "set multiplot layout 1,3 title \"Vetorial vs BM25 (k1=%g, b=%g) por pré-processamento (both = stopwords + stemming)\"\nset style data histograms\nset style histogram clustered gap 1\nset style fill solid 0.9 border rgb \"#444444\"\nset boxwidth 0.9\n%sunset multiplot\n"
        params.k1 params.b panels)

(* One heatmap per preprocessing: metric over the (k1, b) grid, common color scale. *)
let fig_grid ~dir runs metric =
  let cell name k1 b = metric_value (bm25_run runs name { Model.k1; b }).summary metric in
  let all =
    List.concat_map
      (fun cfg ->
        let n = preprocessing_name cfg in
        List.concat_map (fun k1 -> List.map (cell n k1) b_values) k1_values)
      preprocessing_configs
  in
  let lo = List.fold_left Float.min 1.0 all and hi = max_of all in
  let thr = (lo +. hi) /. 2.0 in
  let panels =
    List.mapi
      (fun i cfg ->
        let n = preprocessing_name cfg in
        let rows =
          List.map
            (fun k1 -> String.concat " " (List.map (fun b -> Printf.sprintf "%.4f" (cell n k1 b)) b_values))
            k1_values
        in
        block (Printf.sprintf "g%d" i) rows
        ^ Printf.sprintf
            "set title \"%s\"\nplot $g%d matrix using 1:2:3 with image notitle, '' matrix using 1:2:(sprintf(\"%%.3f\",$3)):($3>%.5f?0xffffff:0x000000) with labels tc rgb variable notitle\n"
            (short_name n) i thr)
      preprocessing_configs
    |> String.concat ""
  in
  render ~dir ~name:("bm25_grid_" ^ String.lowercase_ascii (String.map (fun c -> if c = '@' then '_' else c) (metric_name metric)))
    ~w:7.0 ~h:6.0
    (Printf.sprintf
       "set multiplot layout 2,2 title \"%s do BM25 por (k1, b)\"\nunset grid\nunset key\nset tics scale 0\nset xrange [-0.5:2.5]\nset yrange [-0.5:2.5]\nset xtics (\"0\" 0, \"0.75\" 1, \"1\" 2)\nset ytics (\"0.5\" 0, \"1.2\" 1, \"2.0\" 2)\nset xlabel \"b\"\nset ylabel \"k_1\"\nset cbrange [%.5f:%.5f]\nset palette defined (0 \"#f2f7fb\", 1 \"#1f4e79\")\n%sunset multiplot\n"
       (gp_text (metric_name metric)) lo hi panels)

(* Sorted per-query difference in AP for one comparison. *)
let fig_delta ~dir (comp : Analysis.comparison) =
  let n = Array.length comp.vector in
  let deltas = Array.init n (Analysis.delta_ap comp) |> Array.to_list |> List.sort (fun a b -> Float.compare b a) in
  let better = List.length (List.filter (fun d -> d > Analysis.eps) deltas) in
  let worse = List.length (List.filter (fun d -> d < -.Analysis.eps) deltas) in
  render ~dir ~name:"delta_ap_per_query" ~w:8.0 ~h:4.0
    (block "d" (List.mapi (fun i d -> Printf.sprintf "%d %.5f" (i + 1) d) deltas)
    ^ Printf.sprintf
        "set title \"AP\\\\@all por consulta: BM25 (k1=%g, b=%g) − vetorial, %s\"\nset style fill solid 1.0 noborder\nset boxwidth 1.0 relative\nset xrange [0:%d]\nset xlabel \"consultas, ordenadas por diferença\"\nset ylabel \"ΔAP\\\\@all = AP\\\\@all_{BM25} − AP\\\\@all_{vetorial}\"\nset key top right\nplot $d using 1:($2>0?$2:1/0) with boxes lc rgb \"%s\" title \"BM25 melhor (%d)\", '' using 1:($2<0?$2:1/0) with boxes lc rgb \"%s\" title \"vetorial melhor (%d)\", 0 lc rgb \"#444444\" lw 1 notitle\n"
        comp.params.k1 comp.params.b comp.preprocessing (n + 1) better_color better worse_color worse)

(* AP of one model against the other; the strongest candidates of each
   category are labelled with the query number. *)
let fig_scatter ~dir ~(queries : Query.query list) (comp : Analysis.comparison) =
  let n_relevant i = List.length (List.nth queries i).relevant_documents in
  let n = Array.length comp.vector in
  let all = List.init n (fun i -> Printf.sprintf "%.4f %.4f" comp.vector.(i).ap comp.bm25.(i).ap) in
  let labelled id cat =
    let ps = Analysis.candidate_positions comp ~n_relevant cat |> Analysis.take 3 in
    ( ps,
      block id
        (List.map
           (fun i -> Printf.sprintf "%.4f %.4f %d" comp.vector.(i).ap comp.bm25.(i).ap comp.vector.(i).query)
           ps) )
  in
  let bm, bm_block = labelled "b" Analysis.Bm25_better in
  let ve, ve_block = labelled "v" Analysis.Vector_better in
  let series =
    List.concat
      [ [ "x with lines dt 2 lc rgb \"#888888\" notitle";
          "$all using 1:2 with points pt 7 ps 0.8 lc rgb \"#66888888\" title \"consultas\"" ];
        (if bm = [] then []
         else
           [ Printf.sprintf "$b using 1:2 with points pt 7 ps 1.6 lc rgb \"%s\" title \"BM25 muito melhor\"" better_color;
             "'' using 1:2:3 with labels offset 1.4,0.6 font \",10\" notitle" ]);
        (if ve = [] then []
         else
           [ Printf.sprintf "$v using 1:2 with points pt 7 ps 1.6 lc rgb \"%s\" title \"vetorial muito melhor\"" worse_color;
             "'' using 1:2:3 with labels offset 1.4,0.6 font \",10\" notitle" ]) ]
  in
  render ~dir ~name:"scatter_ap" ~w:5.5 ~h:5.5
    (block "all" all ^ bm_block ^ ve_block
    ^ Printf.sprintf
        "set title \"AP\\\\@all por consulta, %s (BM25 k1=%g, b=%g)\"\nset size square\nset xrange [-0.02:1.02]\nset yrange [-0.02:1.02]\nset xlabel \"AP\\\\@all vetorial\"\nset ylabel \"AP\\\\@all BM25\"\nset key bottom right\nplot %s\n"
        comp.preprocessing comp.params.k1 comp.params.b (String.concat ", \\\n     " series))

(* Effect of b for each k1, with the vector model as a reference line. *)
let fig_b_effect ~dir runs preprocessing =
  let panel metric id =
    let v = metric_value (vector_run runs preprocessing).summary metric in
    block id
      (List.map
         (fun b ->
           Printf.sprintf "%g %s %.4f" b
             (String.concat " "
                (List.map
                   (fun k1 -> Printf.sprintf "%.4f" (metric_value (bm25_run runs preprocessing { Model.k1; b }).summary metric))
                   k1_values))
             v)
         b_values)
  in
  let plot id metric =
    let lines =
      List.mapi
        (fun i k1 ->
          Printf.sprintf "$%s using 1:%d with linespoints pt 7 ps 1.2 lw 2 lc rgb \"%s\" title \"k1=%g\""
            id (i + 2)
            (List.nth [ "#4c78a8"; "#f58518"; "#54a24b" ] i)
            k1)
        k1_values
      @ [ Printf.sprintf "$%s using 1:%d with lines dt 2 lw 2 lc rgb \"#444444\" title \"vetorial\"" id (List.length k1_values + 2) ]
    in
    Printf.sprintf "set title \"%s\"\nplot %s\n" (gp_text (metric_name metric)) (String.concat ", \\\n     " lines)
  in
  render ~dir ~name:"bm25_b_effect" ~w:9.0 ~h:3.8
    (panel Map "m" ^ panel Ndcg10 "n"
    ^ Printf.sprintf
        "set multiplot layout 1,2 title \"Efeito de b no BM25 (%s)\"\nset xlabel \"b\"\nset xrange [-0.1:1.1]\nset xtics (0, 0.75, 1)\nset key bottom right\nset yrange [*:*]\n%s%sunset multiplot\n"
        preprocessing (plot "m" Map) (plot "n" Ndcg10))

let all ~output_dir ~queries runs ~preprocessing ~(params : Model.bm25_params) =
  let dir = Filename.concat output_dir "plots" in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let comp =
    List.find
      (fun (c : Analysis.comparison) -> c.preprocessing = preprocessing && c.params = params)
      (Analysis.comparisons runs)
  in
  fig_metrics ~dir runs params;
  fig_grid ~dir runs Map;
  fig_grid ~dir runs Ndcg10;
  fig_delta ~dir comp;
  fig_scatter ~dir ~queries comp;
  fig_b_effect ~dir runs preprocessing;
  dir
