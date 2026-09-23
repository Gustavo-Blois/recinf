let enabled =
  ref (Sys.getenv_opt "NO_COLOR" = None && Unix.isatty Unix.stdout)

let wrap code s = if !enabled then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s
let bold = wrap "1"
let dim = wrap "2"
let red = wrap "31"
let green = wrap "32"
let yellow = wrap "33"
let cyan = wrap "36"

(* Run [f] with colors off, e.g. to render text destined to a file. *)
let without_color f =
  let saved = !enabled in
  enabled := false;
  Fun.protect ~finally:(fun () -> enabled := saved) f
