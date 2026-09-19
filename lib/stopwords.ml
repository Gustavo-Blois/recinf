let stopwords =
  String.split_all ~sep:"\n"
    "a\n\
     and\n\
     are\n\
     as\n\
     at\n\
     be\n\
     but\n\
     by\n\
     for\n\
     if\n\
     in\n\
     into\n\
     is\n\
     it\n\
     no\n\
     not\n\
     of\n\
     on\n\
     or\n\
     s\n\
     such\n\
     t\n\
     that\n\
     the\n\
     their\n\
     then\n\
     there\n\
     these\n\
     they\n\
     this\n\
     to\n\
     was\n\
     will\n\
     with\n\
     www\n"

let match_stop_words str = List.exists (String.equal str) stopwords
let remove_stop_words l = List.filter (fun s -> not (match_stop_words s)) l
