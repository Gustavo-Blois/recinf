let stopwords =String.split_all ~sep:"\n" "a
and
are
as
at
be
but
by
for
if
in
into
is
it
no
not
of
on
or
s
such
t
that
the
their
then
there
these
they
this
to
was
will
with
www
"

let match_stop_words str = 
  List.exists (String.equal str) stopwords
let remove_stop_words l = List.filter (fun s -> not (match_stop_words s)) l 