type t_raw

type t = { solver : t_raw; mutable vars : int }

type var = int (* positive integers *)

type lit = int (* non zero integers *)

type value = False | True

type solution = SAT | UNSAT

(* type stat = { v : int; c : int; mem : float; cpu : float } *)

external __create : unit -> t_raw = "ocaml_kissat_new"

external __delete : t_raw -> unit = "ocaml_kissat_delete"

external __solve : t_raw -> int = "ocaml_kissat_solve"

external __add : t_raw -> lit -> unit = "ocaml_kissat_add"

external __value : t_raw -> var -> int = "ocaml_kissat_value"

external __get_option : t_raw -> string -> int = "ocaml_kissat_get_option"

external __set_option : t_raw -> string -> int -> int
  = "ocaml_kissat_set_option"

external __print_stats : t_raw -> unit = "ocaml_kissat_print_statistics"

let create () =
  let s = __create () in
  Gc.finalise __delete s;
  { solver = s; vars = 0 }

let solve s =
  match __solve s.solver with
  | 10 -> Ok SAT
  | 20 -> Ok UNSAT
  | x ->
      assert (x = 0);
      Error x

let value_of s l =
  let v = __value s.solver l in
  if v > 0 then True
  else (
    assert (v < 0);
    False )

let new_var s =
  s.vars <- s.vars + 1;
  s.vars

let add_clause s c =
  List.iter (__add s.solver) c;
  __add s.solver 0

let pos_lit var =
  assert (var >= 0);
  var

let neg_lit var =
  assert (var >= 0);
  -var

let negate lit = -lit

let string_of_value = function False -> "false" | True -> "true"
