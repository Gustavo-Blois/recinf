open Recinf
let () =
  let words_of_documents = Utils.words_of_file "data/cran.all.1400" in
  let documents = Document.create_documents words_of_documents in
  print_endline @@ string_of_int @@ List.length documents
