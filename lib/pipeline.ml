let pipeline t =
  let language =
    List.find
      (fun (l : Snowball.Language.t) -> String.equal (l :> string) "english")
      Snowball.languages
  in
  let stemmer = Snowball.create language in
  Token.tokenize t
  |> List.map String.lowercase_ascii
  |> Stopwords.remove_stop_words
  |> List.map (Snowball.stem stemmer)
