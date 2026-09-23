(* Each line of cranqrel is "<query> <doc> <grade>", where the query number is
   the 1-based position of the query in cran.qry (NOT the id in its ".I"
   line, which skips numbers). Grades: 1 (complete answer) .. 4 (minimum
   interest); -1 means not relevant. *)
let judgments_table filename =
  let tbl : (int, (int * int) list) Hashtbl.t = Hashtbl.create 256 in
  let file = In_channel.open_text filename in
  let lines =
    In_channel.input_all file
    |> String.split_all ~sep:"\n"
    |> List.map (fun l ->
           String.split_all ~sep:" " (String.trim l)
           |> List.filter (fun w -> not (String.equal w "")))
    |> List.filter (fun l -> l <> [])
  in
  List.iter
    (function
      | [ q; d; g ] ->
          let q = int_of_string q in
          let existing = Option.value ~default:[] (Hashtbl.find_opt tbl q) in
          Hashtbl.replace tbl q ((int_of_string d, int_of_string g) :: existing)
      | _ -> failwith "malformed qrel line")
    lines;
  In_channel.close file;
  tbl
