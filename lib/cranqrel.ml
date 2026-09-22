let relevant_documents_table filename =
  let tbl = Hashtbl.create 16 in
  let file = In_channel.open_text filename in
  let lines =
    In_channel.input_all file
    |> String.split_all ~sep:"\n"
    |> List.filter (fun l -> not (String.equal (String.trim l) ""))
    |> List.map (String.split_all ~sep:" ")
  in
  List.iter
    (fun line ->
      let query_idx = int_of_string (List.hd line) in
      let doc_idx = int_of_string (List.nth line 1) in
      let existing = Option.value ~default:[] (Hashtbl.find_opt tbl query_idx) in
      Hashtbl.replace tbl query_idx (doc_idx :: existing))
    lines;
  In_channel.close file;
  tbl