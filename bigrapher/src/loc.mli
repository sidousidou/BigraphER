type t = { lstart : Lexing.position; lend : Lexing.position }

val print_loc : Format.formatter -> t -> unit

val string_of_pos : t -> string

val curr : Lexing.lexbuf -> t

val loc : Lexing.position -> Lexing.position -> t

val dummy_loc : t
