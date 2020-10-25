type t

type var = int

type lit = int

type value = False | True | Unknown

type solution = SAT | UNSAT

type stat = { v : int; c : int; mem : float; cpu : float }

external create : unit -> t = "ocaml_maple_new"

external set_verbosity : t -> int -> unit = "ocaml_maple_set_verbosity"

external new_var : t -> var = "ocaml_maple_new_var"

external pos_lit : var -> lit = "ocaml_maple_pos_lit"

external neg_lit : var -> lit = "ocaml_maple_neg_lit"

external negate : lit -> lit = "ocaml_maple_negate"

external __add_clause : t -> lit list -> bool = "ocaml_maple_add_clause"

external __simplify : t -> bool = "ocaml_maple_simplify"

external solve : t -> solution = "ocaml_maple_solve"

external __value_of : t -> var -> int = "ocaml_maple_value_of"

external n_vars : t -> int = "ocaml_maple_n_vars"

external n_clauses : t -> int = "ocaml_maple_n_clauses"

external __mem_used : unit -> float = "ocaml_maple_mem_used"

external __cpu_time : unit -> float = "ocaml_maple_cpu_time"

external __all_solutions_true : t -> var list -> var list list
  = "ocaml_maple_solve_all_true"

let add_clause solver l =
  match l with [] -> () | _ -> ignore (__add_clause solver l)

let simplify solver = ignore (__simplify solver)

let value_of solver v =
  try
    match __value_of solver v with
    | 0 -> False
    | 1 -> True
    | 2 -> Unknown
    | _ -> assert false
  with Failure _ -> invalid_arg "Maple.value_of"

let get_models ?(vars = []) solver = __all_solutions_true solver vars

let get_stats solver =
  {
    v = n_vars solver;
    c = n_clauses solver;
    mem = __mem_used ();
    cpu = __cpu_time ();
  }

let print_stats solver =
  let res = get_stats solver in
  Printf.(
    printf "Number of variables   : %-8d\n" res.v;
    printf "Number of clauses     : %-8d\n" res.c;
    printf "Memory used           : %.2f MB\n" res.mem;
    printf "CPU time              : %g s\n" res.cpu)

let string_of_value = function
  | False -> "false"
  | True -> "true"
  | Unknown -> "unknown"
