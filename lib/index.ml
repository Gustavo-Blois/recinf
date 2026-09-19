open Desserialize

let add_occurrence tbl word doc_id =
  match Hashtbl.find_opt tbl word with
  | Some internal_tbl ->
      begin match Hashtbl.find_opt internal_tbl doc_id with
      | Some tf -> Hashtbl.replace internal_tbl doc_id (tf + 1)
      | None -> Hashtbl.add internal_tbl doc_id 1
      end
  | None -> begin
      let htbl : (int, int) Hashtbl.t = Hashtbl.create 16 in
      Hashtbl.add tbl word htbl;
      Hashtbl.add htbl doc_id 1
    end

let build_inverse_index (documents : document list) =
  let inverse_index : (string, (int, int) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 16
  in
  List.iter
    (fun doc ->
      List.iter
        (fun word -> add_occurrence inverse_index word doc.index)
        doc.text)
    documents;
  inverse_index
