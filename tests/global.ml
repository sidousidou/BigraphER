type text_style =
[ `bold
| `underline
| `black
| `red
| `green
| `yellow
| `blue
| `magenta
| `cyan
| `white ]

let colorise (c: text_style) s =
  let code = match c with
    | `bold      -> "01"
    | `underline -> "04"
    | `black     -> "30"
    | `red       -> "31"
    | `green     -> "32"
    | `yellow    -> "33"
    | `blue      -> "1;34"
    | `magenta   -> "35"
    | `cyan      -> "36"
    | `white     -> "37"
  in
  Printf.sprintf "\027[%sm%s\027[m" code s
