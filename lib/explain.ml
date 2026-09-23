(* Term-by-term breakdown of a document's score, for both models. Mirrors the
   formulas in Model so the totals can be checked against them. *)

let distinct_terms (q : Query.query) =
  List.fold_left
    (fun acc t -> if List.mem_assoc t acc then acc else (t, List.length (List.filter (String.equal t) q.text)) :: acc)
    [] q.text
  |> List.rev

let bm25 (env : Env.t) ~(params : Model.bm25_params) ~(query : Query.query) doc_id =
  let stats = env.stats in
  let doc_len = Hashtbl.find stats.doc_lengths doc_id in
  let norm = 1.0 -. params.b +. (params.b *. doc_len /. stats.average_document_len) in
  let buf = Buffer.create 512 in
  Buffer.add_string buf
    (Printf.sprintf "%s\n  N=%.0f  avgdl=%.2f  |d|=%.0f  k1=%g  b=%g  length-norm = 1-b+b|d|/avgdl = %.4f\n"
       (Ansi.bold (Ansi.cyan (Printf.sprintf "BM25 breakdown for doc %d" doc_id)))
       stats.n stats.average_document_len doc_len params.k1 params.b norm);
  Buffer.add_string buf
    (Ansi.dim
       (Printf.sprintf "  %-16s %4s %5s %4s %8s %8s %10s\n" "term" "qtf" "df" "tf" "idf" "sat" "contrib"));
  let total = ref 0.0 in
  List.iter
    (fun (term, qtf) ->
      let df, tf = Model.posting_tf env.inverted_index term doc_id in
      if df = 0.0 then
        Buffer.add_string buf (Ansi.dim (Printf.sprintf "  %-16s %4d  (not in the collection)\n" term qtf))
      else begin
        let idf = Model.bm25_idf ~stats ~df in
        let sat = if tf = 0.0 then 0.0 else tf *. (params.k1 +. 1.0) /. (tf +. (params.k1 *. norm)) in
        let contrib = float_of_int qtf *. idf *. sat in
        total := !total +. contrib;
        let line =
          Printf.sprintf "  %-16s %4d %5.0f %4.0f %8.4f %8.4f %10.4f\n" term qtf df tf idf sat contrib
        in
        Buffer.add_string buf (if tf = 0.0 then Ansi.dim line else line)
      end)
    (distinct_terms query);
  let check = Model.bm25_score ~query ~doc_index:doc_id ~inverted_index:env.inverted_index ~params ~stats in
  Buffer.add_string buf
    (Printf.sprintf "  score = %s   (Model.bm25_score: %.4f)\n" (Ansi.bold (Printf.sprintf "%.4f" !total)) check);
  Buffer.contents buf

let vector (env : Env.t) ~(query : Query.query) doc_id =
  let buf = Buffer.create 512 in
  Buffer.add_string buf
    (Ansi.bold (Ansi.cyan (Printf.sprintf "Vector breakdown for doc %d\n" doc_id)));
  Buffer.add_string buf
    (Ansi.dim
       (Printf.sprintf "  %-16s %4s %8s %8s %4s %8s %10s\n" "term" "qtf" "idf" "w(t,q)" "tf" "w(t,d)" "product"));
  let dot = ref 0.0 and qsq = ref 0.0 in
  List.iter
    (fun (term, qtf) ->
      match Hashtbl.find_opt env.space.idf term with
      | None ->
          Buffer.add_string buf (Ansi.dim (Printf.sprintf "  %-16s %4d  (not in the collection)\n" term qtf))
      | Some idf ->
          let tf =
            match Hashtbl.find_opt env.inverted_index term with
            | Some p -> Option.value ~default:0 (Hashtbl.find_opt p doc_id)
            | None -> 0
          in
          let wq = Model.tf_weight (float_of_int qtf) *. idf in
          let wd = Model.tf_weight (float_of_int tf) *. idf in
          dot := !dot +. (wq *. wd);
          qsq := !qsq +. (wq *. wq);
          let line =
            Printf.sprintf "  %-16s %4d %8.4f %8.4f %4d %8.4f %10.4f\n" term qtf idf wq tf wd (wq *. wd)
          in
          Buffer.add_string buf (if tf = 0 then Ansi.dim line else line))
    (distinct_terms query);
  let qn = Float.sqrt !qsq in
  let dn = Option.value ~default:0.0 (Hashtbl.find_opt env.space.doc_norms doc_id) in
  let cos = if qn *. dn = 0.0 then 0.0 else !dot /. (qn *. dn) in
  Buffer.add_string buf
    (Printf.sprintf "  dot=%.4f  |q|=%.4f  |d|=%.4f  cosine = %s\n" !dot qn dn
       (Ansi.bold (Printf.sprintf "%.4f" cos)));
  Buffer.contents buf
