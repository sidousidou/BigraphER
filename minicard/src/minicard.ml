type t

type var = int

type lit = int

type value = False | True | Unknown

type solution = SAT | UNSAT

type stat = { v : int; c : int; mem : float; cpu : float }

external create : unit -> t = "ocaml_minicard_new"

external set_verbosity : t -> int -> unit = "ocaml_minicard_set_verbosity"

external set_phase_saving : t -> int -> unit
  = "ocaml_minicard_set_phase_saving"

external set_detect_clause : t -> bool -> unit
  = "ocaml_minicard_set_detect_clause"

external new_var : t -> var = "ocaml_minicard_new_var"

external pos_lit : var -> lit = "ocaml_minicard_pos_lit"

external neg_lit : var -> lit = "ocaml_minicard_neg_lit"

external negate : lit -> lit = "ocaml_minicard_negate"

external __add_clause_empty : t -> bool = "ocaml_minicard_add_caluse_empty"

external __add_clause_unit : t -> lit -> bool
  = "ocaml_minicard_add_clause_unit"

external __add_clause_binary : t -> lit -> lit -> bool
  = "ocaml_minicard_add_clause_binary"

external __add_clause_ternary : t -> lit -> lit -> lit -> bool
  = "ocaml_minicard_add_clasue_ternary"

external __add_clause_quaternary : t -> lit -> lit -> lit -> lit -> bool
  = "ocaml_minicard_add_clause_quaternary"

external __add_clause : t -> lit list -> bool = "ocaml_minicard_add_clause"

external __add_at_most : t -> lit list -> int -> bool
  = "ocaml_minicard_add_at_most"

external __simplify : t -> bool = "ocaml_minicard_simplify"

external solve : t -> solution = "ocaml_minicard_solve"

external __value_of : t -> var -> int = "ocaml_minicard_value_of"

external n_vars : t -> int = "ocaml_minicard_n_vars"

external n_clauses : t -> int = "ocaml_minicard_n_clauses"

external __mem_used : unit -> float = "ocaml_minicard_mem_used"

external __cpu_time : unit -> float = "ocaml_minicard_cpu_time"

external __all_solutions_true : t -> var list -> var list list
  = "ocaml_minicard_solve_all_true"

let add_clause_empty solver = ignore (__add_clause_empty solver)

let add_clause_unit solver l = ignore (__add_clause_unit solver l)

let add_clause_binary solver l0 l1 =
  ignore (__add_clause_binary solver l0 l1)

let add_clause_ternary solver l0 l1 l2 =
  ignore (__add_clause_ternary solver l0 l1 l2)

let add_clause_quaternary solver l0 l1 l2 l3 =
  ignore (__add_clause_quaternary solver l0 l1 l2 l3)

let add_clause solver = function
  | [] -> ignore (__add_clause_empty solver)
  | [ l ] -> add_clause_unit solver l
  | [ l0; l1 ] -> add_clause_binary solver l0 l1
  | [ l0; l1; l2 ] -> add_clause_ternary solver l0 l1 l2
  | [ l0; l1; l2; l3 ] -> add_clause_quaternary solver l0 l1 l2 l3
  | c -> ignore (__add_clause solver c)

let simplify solver = ignore (__simplify solver)

let add_at_most solver l k =
  match l with [] -> () | _ -> ignore (__add_at_most solver l k)

let add_at_least solver l k =
  add_at_most solver (List.rev_map negate l) (List.length l - k)

let value_of solver v =
  try
    match __value_of solver v with
    | 0 -> False
    | 1 -> True
    | 2 -> Unknown
    | _ -> assert false
  with Failure _ -> invalid_arg "Minicard.value_of"

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
