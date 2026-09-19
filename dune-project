(lang dune 3.24)

(name recinf)

(generate_opam_files true)

(source
 (github Gustavo-Blois/recinf))

(authors "Gustavo Blois <vlmblois@gmail.com>")

(maintainers "Gustavo Blois <vlmblois@gmail.com>")

(license MIT)

(package
 (name recinf)
 (synopsis "Information retrieval exercises over the Cranfield collection")
 (description
  "Tokenizer, stopword filter, Snowball stemmer pipeline and document deserializer for the cran.all.1400 corpus.")
 (depends
  (ocaml
   (>= 5.5.0))
  (dune
   (>= 3.24))
  stem
  base)
 (tags
  ("information-retrieval" "nlp" "cranfield")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
