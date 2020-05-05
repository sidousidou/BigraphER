type t

type var = int

type lit = int

type value = False | True | Unknown

type solution = SAT | UNSAT

type stat = { v : int; c : int; mem : float; cpu : float }

external create : unit -> t = "ocaml_minicard_new"

external new_var : t -> var = "ocaml_minicard_new_var" [@@noalloc]

external pos_lit : var -> lit = "ocaml_minicard_pos_lit" [@@noalloc]

external neg_lit : var -> lit = "ocaml_minicard_neg_lit" [@@noalloc]

external negate : lit -> lit = "ocaml_minicard_negate" [@@noalloc]

external __add_clause : t -> lit list -> bool = "ocaml_minicard_add_clause"

external __add_at_most : t -> lit list -> int -> bool
  = "ocaml_minicard_add_at_most"

external simplify : t -> bool = "ocaml_minicard_simplify"

external solve : t -> solution = "ocaml_minicard_solve"

external __value_of : t -> var -> int = "ocaml_minicard_value_of"

external n_vars : t -> int = "ocaml_minicard_n_vars" [@@noalloc]

external n_clauses : t -> int = "ocaml_minicard_n_clauses" [@@noalloc]

external __mem_used : unit -> float = "ocaml_minicard_mem_used" [@@noalloc]

external __cpu_time : unit -> float = "ocaml_minicard_cpu_time" [@@noalloc]

class solver =
  object (self)
    val solver = create ()

    method new_var = new_var solver

    method add_clause l =
      match l with [] -> () | _ -> ignore (__add_clause solver l)

    method add_at_most l k =
      match l with [] -> () | _ -> ignore (__add_at_most solver l k)

    method add_at_least l k =
      self#add_at_most (List.map negate l) (List.length l - k)

    method simplify = ignore (simplify solver)

    method solve = solve solver

    method value_of v =
      try
        match __value_of solver v with
        | 0 -> False
        | 1 -> True
        | 2 -> Unknown
        | _ -> assert false
      with Failure _ -> invalid_arg "Minicard.value_of"

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
