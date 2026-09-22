let punctuations = ",.;!?()'\"\\/{}[]@#$%:"
let match_punctuations c = String.exists (Char.equal c) punctuations

let filter_chars_in_string pred s =
  String.to_seq s |> Seq.filter (fun c -> not (pred c)) |> String.of_seq

let word_list str =
  String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c) str
  |> String.split_all ~sep:" "
  |> List.filter (fun c -> not @@ String.equal c "")


let tokenize text =
  String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c) text
  |> filter_chars_in_string match_punctuations
  |> String.split_all ~sep:" "
  |> List.filter (fun c -> not @@ String.equal c "")

let clean_tokens (words : string list) =
  List.map (filter_chars_in_string match_punctuations) words |>
  List.filter (fun c -> not @@ String.equal c "") 
