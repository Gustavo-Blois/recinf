let punctuations = ",.;!?()'\"\\/{}[]@#$%:\n\t\r"
let match_punctuations c = String.exists (Char.equal c) punctuations

let filter_chars_in_string pred s =
  String.to_seq s |> Seq.filter (fun c -> not (pred c)) |> String.of_seq

let tokenize t =
  filter_chars_in_string match_punctuations t
  |> String.split_all ~sep:" "
  |> List.filter (fun c -> not @@ String.equal c "")
