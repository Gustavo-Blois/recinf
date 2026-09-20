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

let build_inverted_index (documents : document list) =
  let inverted_index : (string, (int, int) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 16
  in
  List.iter
    (fun (doc:document) ->
      List.iter
        (fun word -> add_occurrence inverted_index word doc.index)
        doc.text)
    documents;
  inverted_index
