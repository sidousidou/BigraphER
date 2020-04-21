type s

type var = int

type lit = int

type value = False | True | Unknown

type solution = SAT | UNSAT

type stat = { v : int; c : int; mem : float; cpu : float }

exception MINISAT of string

external create : unit -> s = "ocaml_minisat_new"

external new_var : s -> var = "ocaml_minisat_new_var" [@@noalloc]

external pos_lit : var -> lit = "ocaml_minisat_pos_lit" [@@noalloc]

external neg_lit : var -> lit = "ocaml_minisat_neg_lit" [@@noalloc]

external __add_clause : s -> lit list -> bool = "ocaml_minisat_add_clause"

external simplify : s -> bool = "ocaml_minisat_simplify"

external solve : s -> solution = "ocaml_minisat_solve"

external __value_of : s -> var -> int = "ocaml_minisat_value_of"

external n_vars : s -> int = "ocaml_minisat_n_vars" [@@noalloc]

external n_clauses : s -> int = "ocaml_minisat_n_clauses" [@@noalloc]

external __mem_used : unit -> float = "ocaml_minisat_mem_used" [@@noalloc]

external __cpu_time : unit -> float = "ocaml_minisat_cpu_time" [@@noalloc]

class solver =
  object (self)
    val solver = create ()

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
      with Failure _ -> raise (MINISAT "value_of")

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
