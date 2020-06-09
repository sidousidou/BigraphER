type t

type var = int

type lit = int

type value = False | True | Unknown

type solution = SAT | UNSAT

type stat = { v : int; c : int; mem : float; cpu : float }

external create : unit -> t = "ocaml_minisat_new"

external set_verbosity : t -> int -> unit = "ocaml_minisat_set_verbosity"

external new_var : t -> var = "ocaml_minisat_new_var"

external pos_lit : var -> lit = "ocaml_minisat_pos_lit"

external neg_lit : var -> lit = "ocaml_minisat_neg_lit"

external negate : lit -> lit = "ocaml_minisat_negate"

external __add_clause : t -> lit list -> bool = "ocaml_minisat_add_clause"

external simplify : t -> bool = "ocaml_minisat_simplify"

external solve : t -> solution = "ocaml_minisat_solve"

external __value_of : t -> var -> int = "ocaml_minisat_value_of"

external n_vars : t -> int = "ocaml_minisat_n_vars"

external n_clauses : t -> int = "ocaml_minisat_n_clauses"

external __mem_used : unit -> float = "ocaml_minisat_mem_used"

external __cpu_time : unit -> float = "ocaml_minisat_cpu_time"

class solver =
  object (self)
    val solver = create ()

    (* Verbosity level (0=silent, 1=some, 2=more). *)
    method set_verbosity verb = set_verbosity solver verb

    method new_var = new_var solver

    method add_clause l =
      match l with [] -> () | _ -> ignore (__add_clause solver l)

    method simplify = ignore (simplify solver)

    method solve = solve solver

    method value_of v =
      try
        match __value_of solver v with
        | 0 -> False
        | 1 -> True
        | 2 -> Unknown
        | _ -> assert false
      with Failure _ -> invalid_arg "Minisat.value_of"

    method get_stats =
      {
        v = n_vars solver;
        c = n_clauses solver;
        mem = __mem_used ();
        cpu = __cpu_time ();
      }

    method print_stats =
      let res = self#get_stats in
      Printf.(
        printf "Number of variables   : %-8d\n" res.v;
        printf "Number of clauses     : %-8d\n" res.c;
        printf "Memory used           : %.2f MB\n" res.mem;
        printf "CPU time              : %g s\n" res.cpu)
  end

let string_of_value = function
  | False -> "false"
  | True -> "true"
  | Unknown -> "unknown"
