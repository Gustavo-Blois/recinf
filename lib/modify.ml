(* Item 8: run an original query and a hand-written variant through both
   models and show what changed in the top 10. The variant is evaluated
   against the qrels of the original query. *)

type row = {
  query : int;
  cran_id : int;
  model : string;
  version : string;
  text : string;
  metrics : Metrics.query_metrics;
  top10 : int list;
}

let top = 10

let rec take k = function x :: r when k > 0 -> x :: take (k - 1) r | _ -> []

let describe (q : Query.query) doc = Printf.sprintf "%d%s" doc (if List.mem doc q.relevant_documents then "*" else "")

let compare_one env ~model ~(original : Query.query) ~(modified : Query.query) =
  let orig = Env.ranked env model original and modi = Env.ranked env model modified in
  let ids r = take top (Display.doc_ids r) in
  let o = ids orig and m = ids modi in
  let pos l d = let rec go i = function [] -> None | x :: r -> if x = d then Some i else go (i + 1) r in go 1 l in
  let entered = List.filter (fun d -> not (List.mem d o)) m in
  let left = List.filter (fun d -> not (List.mem d m)) o in
  let moved =
    List.filter_map
      (fun d -> match (pos o d, pos m d) with Some a, Some b when a <> b -> Some (d, a, b) | _ -> None)
      o
  in
  let mo = Metrics.evaluate_query original (Display.doc_ids orig) in
  let mm = Metrics.evaluate_query original (Display.doc_ids modi) in
  let fmt_list l = if l = [] then Ansi.dim "-" else String.concat " " (List.map (describe original) l) in
  let d a b = let x = b -. a in
    let s = Printf.sprintf "%+.3f" x in
    if x > 1e-9 then Ansi.green s else if x < -1e-9 then Ansi.red s else Ansi.dim s in
  let text =
    Printf.sprintf
      "%s\n  original  top10: %s\n  modified  top10: %s\n  entered: %s\n  left:    %s\n  moved:   %s\n  P@10 %.2f -> %.2f (%s)   AP@all %.3f -> %.3f (%s)   NDCG@10 %.3f -> %.3f (%s)\n  %s\n"
      (Ansi.bold (Ansi.cyan (Env.model_name model)))
      (String.concat " " (List.map (describe original) o))
      (String.concat " " (List.map (describe original) m))
      (fmt_list entered) (fmt_list left)
      (if moved = [] then Ansi.dim "-"
       else String.concat "  " (List.map (fun (d, a, b) -> Printf.sprintf "%d: #%d->#%d" d a b) moved))
      mo.p10 mm.p10 (d mo.p10 mm.p10) mo.ap mm.ap (d mo.ap mm.ap) mo.ndcg10 mm.ndcg10 (d mo.ndcg10 mm.ndcg10)
      (Ansi.dim "(* = relevant)")
  in
  (text, orig, modi, mo, mm, o, m)

let run env ~models ~(raw : Query.query) ~text =
  let original = Env.query env raw.index in
  let modified = Env.with_text env original text in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (Display.query_header env ~raw original);
  Buffer.add_string buf
    (Printf.sprintf "  %s %s\n  processed: %s\n\n" (Ansi.bold "modified:") text
       (String.concat " " modified.text));
  let rows =
    List.concat_map
      (fun model ->
        let s, _, _, mo, mm, o, m = compare_one env ~model ~original ~modified in
        Buffer.add_string buf s;
        Buffer.add_char buf '\n';
        let name = Env.model_name model in
        [ { query = raw.index; cran_id = raw.original_id; model = name; version = "original"; text = Display.raw_text raw; metrics = mo; top10 = o };
          { query = raw.index; cran_id = raw.original_id; model = name; version = "modified"; text; metrics = mm; top10 = m } ])
      models
  in
  (Buffer.contents buf, rows)

let write_csv path rows =
  let oc = Out_channel.open_text path in
  Printf.fprintf oc "query,cran_id,model,version,P@10,R@10,F1@10,AP@all,AP@10,NDCG@10,top10,text\n";
  List.iter
    (fun r ->
      Printf.fprintf oc "%d,%d,%s,%s,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%s,\"%s\"\n" r.query r.cran_id
        (String.map (fun c -> if c = ',' then ';' else c) r.model)
        r.version r.metrics.p10 r.metrics.r10 r.metrics.f1_10 r.metrics.ap r.metrics.ap10 r.metrics.ndcg10
        (String.concat " " (List.map string_of_int r.top10)) r.text)
    rows;
  Out_channel.close oc

(* File format, like cran.qry: a ".I <id>" line (the id written in cran.qry, not the
   position) followed by ".T <modified text>", which may continue on the next
   lines until the next ".I". Blank lines and lines starting with # are skipped. *)
let read_file path =
  let fail_at n msg = failwith (Printf.sprintf "%s:%d: %s" path n msg) in
  let starts tag l =
    let n = String.length tag in
    String.length l >= n && String.sub l 0 n = tag && (String.length l = n || l.[n] = ' ' || l.[n] = '\t')
  in
  let rest tag l = String.trim (String.sub l (String.length tag) (String.length l - String.length tag)) in
  let entries = ref [] and current = ref None in
  let flush n =
    match !current with
    | None -> ()
    | Some (id, text) ->
        if text = [] then fail_at n (Printf.sprintf "query .I %d has no .T text" id);
        entries := (id, String.concat " " (List.rev text)) :: !entries;
        current := None
  in
  let lines = String.split_all ~sep:"\n" (In_channel.with_open_text path In_channel.input_all) in
  List.iteri
    (fun i l ->
      let n = i + 1 and l = String.trim l in
      if l = "" || l.[0] = '#' then ()
      else if starts ".I" l then begin
        flush n;
        match int_of_string_opt (rest ".I" l) with
        | Some id -> current := Some (id, [])
        | None -> fail_at n ("expected \".I <number>\", got: " ^ l)
      end
      else if starts ".T" l then begin
        match !current with
        | Some (id, []) -> current := Some (id, [ rest ".T" l ])
        | Some _ -> fail_at n "second .T for the same .I"
        | None -> fail_at n ".T before any .I"
      end
      else
        match !current with
        | Some (id, (_ :: _ as text)) -> current := Some (id, l :: text)
        | _ -> fail_at n ("expected \".I <number>\" or \".T <text>\", got: " ^ l))
    lines;
  flush (List.length lines);
  List.rev !entries
