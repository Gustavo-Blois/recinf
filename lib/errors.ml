(* Item 9: error analysis. Two kinds of mistake, for one or more model
   configurations:
   - false positives: non-relevant documents that rank in the first positions
     of a query's ranking (a document with score > 0 but not in the qrels).
   - false negatives: relevant documents (grade >= 1) that fall outside the
     Top-10, or that are never retrieved at all (score 0, no shared term).
   Both are collected across every query so the report can pick concrete
   examples and "explain" (Explain.ml) can be used on them to investigate why. *)

let top_k = 10  (* "does not appear in the Top-10" *)
let fp_rank_cutoff = 5  (* "first positions of the ranking" *)
let not_retrieved = max_int  (* sentinel: worse than any real rank, for sorting *)

type false_positive = {
  fp_model : string;
  fp_query : Query.query;  (* raw, for its original text *)
  fp_rank : int;
  fp_doc : int;
  fp_score : float;
}

type false_negative = {
  fn_model : string;
  fn_query : Query.query;
  fn_doc : int;
  fn_grade : int;  (* 1 = complete answer .. 4 = minimum interest *)
  fn_rank : int option;  (* None = not retrieved at all *)
  fn_n_relevant : int;
}

let false_positives (env : Env.t) ~model_name model (queries : Query.query list) =
  List.concat_map
    (fun (raw : Query.query) ->
      let q = Env.query env raw.index in
      Env.ranked env model q
      |> List.filteri (fun i _ -> i < fp_rank_cutoff)
      |> List.mapi (fun i (r : Model.results) -> (i + 1, r))
      |> List.filter_map (fun (rank, (r : Model.results)) ->
             if List.mem r.docId q.relevant_documents then None
             else Some { fp_model = model_name; fp_query = raw; fp_rank = rank; fp_doc = r.docId; fp_score = r.score }))
    queries

(* worst offenders first: rank 1 mistakes before rank 5. Scores are not
   comparable across models (BM25 and cosine live on different scales), so
   ties within the same rank are broken by query order, not by score. *)
let sort_false_positives fps =
  List.sort (fun a b -> match Int.compare a.fp_rank b.fp_rank with 0 -> Int.compare a.fp_query.index b.fp_query.index | c -> c) fps

let false_negatives (env : Env.t) ~model_name model (queries : Query.query list) =
  List.concat_map
    (fun (raw : Query.query) ->
      let q = Env.query env raw.index in
      let ranking = Env.ranked env model q in
      let pos = Display.positions ranking in
      let n_relevant = List.length q.relevant_documents in
      List.filter_map
        (fun doc ->
          let grade = List.assoc doc q.judgments in
          match Hashtbl.find_opt pos doc with
          | Some r when r <= top_k -> None
          | Some r -> Some { fn_model = model_name; fn_query = raw; fn_doc = doc; fn_grade = grade; fn_rank = Some r; fn_n_relevant = n_relevant }
          | None -> Some { fn_model = model_name; fn_query = raw; fn_doc = doc; fn_grade = grade; fn_rank = None; fn_n_relevant = n_relevant })
        q.relevant_documents)
    queries

(* most relevant grade first (1 before 4), then the misses that hurt most:
   fewer other relevant documents to fall back on, buried the deepest *)
let sort_false_negatives fns =
  let rank_val r = Option.value ~default:not_retrieved r in
  List.sort
    (fun a b ->
      match Int.compare a.fn_grade b.fn_grade with
      | 0 -> (
          match Int.compare a.fn_n_relevant b.fn_n_relevant with
          | 0 -> Int.compare (rank_val b.fn_rank) (rank_val a.fn_rank)
          | c -> c)
      | c -> c)
    fns

let models_of_flag ~(bm25 : Model.bm25_params) = function
  | "vector" -> [ ("vector", Env.Vector) ]
  | "bm25" -> [ (Printf.sprintf "bm25(k1=%g,b=%g)" bm25.k1 bm25.b, Env.Bm25 bm25) ]
  | "both" -> [ ("vector", Env.Vector); (Printf.sprintf "bm25(k1=%g,b=%g)" bm25.k1 bm25.b, Env.Bm25 bm25) ]
  | s -> failwith (Printf.sprintf "unknown model %S (vector | bm25 | both)" s)

let collect env ~models queries =
  let fps = List.concat_map (fun (name, m) -> false_positives env ~model_name:name m queries) models |> sort_false_positives in
  let fns = List.concat_map (fun (name, m) -> false_negatives env ~model_name:name m queries) models |> sort_false_negatives in
  (fps, fns)

let write_csv path ~preprocessing (fps : false_positive list) (fns : false_negative list) =
  let oc = Out_channel.open_text path in
  Printf.fprintf oc
    "category,model,preprocessing,query,cran_id,doc,rank,score,grade,n_relevant,text\n";
  List.iter
    (fun (fp : false_positive) ->
      Printf.fprintf oc "false_positive,%s,%s,%d,%d,%d,%d,%.4f,,,\"%s\"\n" fp.fp_model preprocessing
        fp.fp_query.index fp.fp_query.original_id fp.fp_doc fp.fp_rank fp.fp_score (Display.raw_text fp.fp_query))
    fps;
  List.iter
    (fun (fn : false_negative) ->
      let rank = match fn.fn_rank with Some r -> string_of_int r | None -> "" in
      Printf.fprintf oc "false_negative,%s,%s,%d,%d,%d,%s,,%d,%d,\"%s\"\n" fn.fn_model preprocessing
        fn.fn_query.index fn.fn_query.original_id fn.fn_doc rank fn.fn_grade fn.fn_n_relevant
        (Display.raw_text fn.fn_query))
    fns;
  Out_channel.close oc

let print_false_positives ~top fps =
  Printf.printf "\n%s\n%s\n"
    (Ansi.bold (Printf.sprintf "False positives: non-relevant documents in the first %d positions" fp_rank_cutoff))
    (Ansi.dim "Q = query number (position in cran.qry) | .I = id written in cran.qry | rank = position in that model's ranking");
  Printf.printf "%s\n" (Ansi.dim (Printf.sprintf "  %-24s %3s %5s %4s %5s %10s  %s" "model" "Q" ".I" "doc" "rank" "score" "text"));
  List.iteri
    (fun i (fp : false_positive) ->
      if i < top then
        Printf.printf "  %-24s %3d %5d %4d %5d %10.4f  %s\n" fp.fp_model fp.fp_query.index
          fp.fp_query.original_id fp.fp_doc fp.fp_rank fp.fp_score (Display.truncate 55 (Display.raw_text fp.fp_query)))
    fps

let print_false_negatives ~top fns =
  Printf.printf "\n%s\n%s\n"
    (Ansi.bold (Printf.sprintf "False negatives: relevant documents outside the Top-%d" top_k))
    (Ansi.dim "grade: 1 = complete answer .. 4 = minimum interest | rank = '-' means never retrieved (score 0, no shared term) | rel = relevant docs for that query");
  Printf.printf "%s\n" (Ansi.dim (Printf.sprintf "  %-24s %3s %5s %4s %5s %5s %4s  %s" "model" "Q" ".I" "doc" "grade" "rank" "rel" "text"));
  List.iteri
    (fun i (fn : false_negative) ->
      if i < top then
        let rank = match fn.fn_rank with Some r -> string_of_int r | None -> "-" in
        Printf.printf "  %-24s %3d %5d %4d %5d %5s %4d  %s\n" fn.fn_model fn.fn_query.index
          fn.fn_query.original_id fn.fn_doc fn.fn_grade rank fn.fn_n_relevant
          (Display.truncate 55 (Display.raw_text fn.fn_query)))
    fns

(* For the first [detail] rows of each category: the query header, both
   models' Top-10 (for context) and an Explain breakdown of the offending
   document under the model that produced the mistake. *)
let explain_for env model (query : Query.query) doc =
  match model with
  | Env.Vector -> Explain.vector env ~query doc
  | Env.Bm25 params -> Explain.bm25 env ~params ~query doc

let print_detail (env : Env.t) ~bm25 ~models ~detail fps fns =
  let model_named name = List.assoc name models in
  List.iteri
    (fun i (fp : false_positive) ->
      if i < detail then begin
        let q = Env.query env fp.fp_query.index in
        Printf.printf "\n%s\n%s"
          (Ansi.bold (Printf.sprintf "False positive #%d: Q%d doc %d, rank %d under %s" (i + 1) fp.fp_query.index fp.fp_doc fp.fp_rank fp.fp_model))
          (Display.query_header env ~raw:fp.fp_query q);
        print_string (Display.both_models env ~bm25 ~query:q ~top:5);
        print_newline ();
        print_string (explain_for env (model_named fp.fp_model) q fp.fp_doc)
      end)
    fps;
  List.iteri
    (fun i (fn : false_negative) ->
      if i < detail then begin
        let q = Env.query env fn.fn_query.index in
        Printf.printf "\n%s\n%s"
          (Ansi.bold
             (Printf.sprintf "False negative #%d: Q%d doc %d, grade %d, %s under %s" (i + 1) fn.fn_query.index
                fn.fn_doc fn.fn_grade
                (match fn.fn_rank with Some r -> Printf.sprintf "rank %d" r | None -> "never retrieved")
                fn.fn_model))
          (Display.query_header env ~raw:fn.fn_query q);
        print_string (Display.both_models env ~bm25 ~query:q ~top:5);
        print_newline ();
        print_string (explain_for env (model_named fn.fn_model) q fn.fn_doc)
      end)
    fns
