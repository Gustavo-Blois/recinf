(* Terminal rendering. Every function returns a string so the same output can
   go to stdout (with colors) or to a file (without). *)

let truncate n s = if String.length s <= n then s else String.sub s 0 (n - 3) ^ "..."
let title (d : Document.document) = String.concat " " d.title
let raw_text (q : Query.query) = String.concat " " q.text

let positions ranking =
  let tbl = Hashtbl.create 256 in
  List.iteri (fun i (r : Model.results) -> Hashtbl.replace tbl r.docId (i + 1)) ranking;
  tbl

let doc_ids ranking = List.map (fun (r : Model.results) -> r.docId) ranking

let grade_cell (q : Query.query) doc =
  match List.assoc_opt doc q.judgments with
  | Some g -> Ansi.green (Printf.sprintf "g%-2d" g)
  | None -> Ansi.dim "-- "

let metrics_line (m : Metrics.query_metrics) =
  Printf.sprintf "P@10 %.3f  R@10 %.3f  F1@10 %.3f  AP@all %.3f  AP@10 %.3f  RR %.3f  NDCG@10 %.3f" m.p10
    m.r10 m.f1_10 m.ap m.ap10 m.rr m.ndcg10

let relevance_summary (q : Query.query) ranking =
  let ids = doc_ids ranking in
  let n_rel = List.length q.relevant_documents in
  let retrieved = List.filter (fun d -> List.mem d ids) q.relevant_documents in
  let first =
    let rec go i = function
      | [] -> "none"
      | d :: rest -> if List.mem d q.relevant_documents then Printf.sprintf "#%d" i else go (i + 1) rest
    in
    go 1 ids
  in
  Printf.sprintf "relevant: %d | retrieved (score>0): %d | first relevant: %s" n_rel
    (List.length retrieved) first

let query_header (env : Env.t) ~(raw : Query.query) (q : Query.query) =
  ignore env;
  Printf.sprintf "%s %s\n  text:      %s\n  processed: %s\n  relevant docs: %d\n"
    (Ansi.bold (Printf.sprintf "Query %d" q.index))
    (Ansi.dim (Printf.sprintf "(id %03d in cran.qry)" q.original_id))
    (raw_text raw)
    (String.concat " " q.text)
    (List.length q.relevant_documents)

(* [other] is the ranking of the other model, used for the "also" column. *)
let top_table (env : Env.t) ~(query : Query.query) ~ranking ~other ~top =
  let buf = Buffer.create 512 in
  let other_pos = positions other in
  Buffer.add_string buf
    (Ansi.dim (Printf.sprintf "  %-4s %-5s %-8s %-4s %-6s %s\n" "#" "doc" "score" "rel" "other" "title"));
  List.iteri
    (fun i (r : Model.results) ->
      if i < top then begin
        let rel = List.mem r.docId query.relevant_documents in
        let other_cell =
          match Hashtbl.find_opt other_pos r.docId with
          | Some p -> Printf.sprintf "#%d" p
          | None -> "-"
        in
        let doc_cell = Printf.sprintf "%-5d" r.docId in
        Buffer.add_string buf
          (Printf.sprintf "  %-4d %s %-8.4f %s %-6s %s\n" (i + 1)
             (if rel then Ansi.green doc_cell else doc_cell)
             r.score (grade_cell query r.docId) other_cell
             (truncate 62 (title (Env.doc env r.docId))))
      end)
    ranking;
  Buffer.contents buf

let model_section env ~label ~(query : Query.query) ~ranking ~other ~top =
  let m = Metrics.evaluate_query query (doc_ids ranking) in
  Printf.sprintf "%s\n  %s\n  %s\n%s" (Ansi.bold (Ansi.cyan label)) (metrics_line m)
    (Ansi.dim (relevance_summary query ranking))
    (top_table env ~query ~ranking ~other ~top)

let both_models env ~(bm25 : Model.bm25_params) ~query ~top =
  let v = Env.ranked env Env.Vector query in
  let b = Env.ranked env (Env.Bm25 bm25) query in
  model_section env ~label:"VECTOR (cosine, ltc)" ~query ~ranking:v ~other:b ~top
  ^ "\n"
  ^ model_section env
      ~label:(Printf.sprintf "BM25 (k1=%g, b=%g)" bm25.k1 bm25.b)
      ~query ~ranking:b ~other:v ~top

(* "#3" = rank of the first relevant document, "-" if none was retrieved. *)
let first_relevant (q : Query.query) ranking =
  let rec go i = function
    | [] -> "-"
    | (r : Model.results) :: rest ->
        if List.mem r.docId q.relevant_documents then Printf.sprintf "#%d" i else go (i + 1) rest
  in
  go 1 ranking
