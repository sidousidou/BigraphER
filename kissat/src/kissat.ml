type t

type var = int (* positive integers *)

type lit = int (* non zero integers *)

type value = False | True | Unknown

type solution = SAT | UNSAT

external __create : unit -> t = "ocaml_kissat_new"

external __delete : t -> unit = "ocaml_kissat_delete"

external __solve : t -> int = "ocaml_kissat_solve"

external add_clause_empty : t -> unit = "ocaml_kissat_add_clause_empty"

external add_clause_unit : t -> lit -> unit = "ocaml_kissat_add_clause_unit"

external add_clause_binary : t -> lit -> lit -> unit
  = "ocaml_kissat_add_clause_binary"

external add_clause_ternary : t -> lit -> lit -> lit -> unit
  = "ocaml_kissat_add_clause_ternary"

external add_clause_quaternary : t -> lit -> lit -> lit -> lit -> unit
  = "ocaml_kissat_add_clause_quaternary"

external add_clause : t -> lit list -> unit = "ocaml_kissat_add_clause"

external __value : t -> var -> int = "ocaml_kissat_value"

external get_option : t -> string -> int = "ocaml_kissat_get_option"

external set_option : t -> string -> int -> int = "ocaml_kissat_set_option"

external print_stats : t -> unit = "ocaml_kissat_print_statistics"

let create () =
  let s = __create () in
  Gc.finalise __delete s;
  s

let solve s =
  match __solve s with
  | 10 -> Ok SAT
  | 20 -> Ok UNSAT
  | x ->
      assert (x = 0);
      Error x

let value_of s var =
  let v = __value s var in
  if v > 0 then True else if v < 0 then False else Unknown

let pos_lit var =
  assert (var > 0);
  var

let neg_lit var =
  assert (var > 0);
  -var

let negate lit = -lit

let string_of_value = function
  | False -> "false"
  | True -> "true"
  | Unknown -> "unknown"

let var_of_int x =
  assert (x > 0);
  x
