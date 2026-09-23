let words_of_file filename =
  let file = In_channel.open_text filename in
  let lines =
    In_channel.input_all file |> String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c)
  in
  let words =
    String.split_all ~sep:" " lines
    |> List.filter (fun x -> not (String.is_empty x))
  in
  In_channel.close file;
  words

let take_in_between word_list first last =
  let rec go ~found = function
    | [] -> []
    | x :: rest ->
        if found then if String.equal x last then [] else x :: go ~found rest
        else if String.equal x first then go ~found:true rest
        else go ~found rest
  in
  go ~found:false word_list

let int_of_string_singleton = function
  | [ s ] -> int_of_string s
  | _ -> failwith "expected a singleton list"


(* Data and results paths are relative to the project root. Find it by walking
   up from the current directory (then from the executable's, for
   `dune exec` launched elsewhere) until data/cran.all.1400 shows up. *)
let find_project_root () =
  let marker = Filename.concat "data" "cran.all.1400" in
  let rec up dir =
    if Sys.file_exists (Filename.concat dir marker) then Some dir
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else up parent
  in
  let absolute p = if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p in
  match up (Sys.getcwd ()) with
  | Some _ as root -> root
  | None -> up (Filename.dirname (absolute Sys.executable_name))
