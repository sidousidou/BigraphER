open Lexing
open Format

type t = { lstart : Lexing.position; lend : Lexing.position }

let print_loc fmt loc =
  let p0 = loc.lstart and p1 = loc.lend in
  let off0 = p0.pos_cnum - p0.pos_bol and off1 = p1.pos_cnum - p0.pos_bol in
  (* same line *)
  fprintf fmt "@[File '%s', line %i, characters %i-%i@]@," p0.pos_fname
    p0.pos_lnum off0 off1

let string_of_pos loc =
  let p0 = loc.lstart and p1 = loc.lend in
  let off0 = p0.pos_cnum - p0.pos_bol and off1 = p1.pos_cnum - p0.pos_bol in
  (* same line *)
  "line " ^ string_of_int p0.pos_lnum ^ ", characters " ^ string_of_int off0
  ^ "-" ^ string_of_int off1

let curr lexbuf = { lstart = lexbuf.lex_start_p; lend = lexbuf.lex_curr_p }

let loc lstart lend = { lstart; lend }

let dummy_loc = { lstart = Lexing.dummy_pos; lend = Lexing.dummy_pos }
