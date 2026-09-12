let words_of_file filename =
  let file = In_channel.open_text filename in
  let lines = In_channel.input_all file |> String.replace_all ~sub:"\n" ~by:" " in
  let words = String.split_all ~sep:" " lines |> List.filter (fun x -> not (String.is_empty x)) in
  In_channel.close file;
  words

let take_in_between word_list first last =
  let rec go ~found = function
    | [] -> []
    | x :: rest ->
      if found then
        if String.equal x last then []
        else x :: go ~found rest
      else if String.equal x first then
        go ~found:true rest
      else
        go ~found rest
  in
  go ~found:false word_list

let int_of_string_singleton = function
  | [s] -> int_of_string s
  | _ -> failwith "expected a singleton list"

let apply_twice f x = f (f x)