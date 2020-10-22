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
(* Specific to MiniSat based on how it stores literals internally.
 *
 * Literal = var + var + (sign == -)
 *  E.g.+1 = 2
 *      -1 = 3
 *
 *  Remap to start from 1 since CNF format uses a 0 at end of line *)
let lit_to_varstr (l : lit) : string =
  let v = l / 2 in
  if l mod 2 == 0 then string_of_int (v + 1)
  else "-" ^ string_of_int (v + 1)

let should_dump_cnf =
  Option.is_some (Sys.getenv_opt "BIG_DUMPCNF")

(* Static counter to determine CNF filename *)
let cnf_file = ref 0

class solver =
  object (self)

    val mutable cnf_store = ( [] : string list )

    val solver =
      incr cnf_file;
      create ();

    (* Output CNF clauses in order of addition *)
    method dump_cnf =
      let fname =  ((string_of_int !cnf_file) ^ ".cnf") in
      let outf = open_out_gen [Open_append; Open_creat] 0o666 fname in
      Printf.fprintf outf "CNF_START\n";
      List.rev cnf_store |> List.iter (fun s -> Printf.fprintf outf "%s\n" s);
      Printf.fprintf outf "CNF_END\n";
      close_out outf;

    (* Verbosity level (0=silent, 1=some, 2=more). *)
    method set_verbosity verb = set_verbosity solver verb

    method new_var = new_var solver

    method add_clause l =
      match l with
        [] -> ()
      | _ -> begin
          if should_dump_cnf then(
            let clause = (List.map lit_to_varstr l |> String.concat " ") ^ " 0"
            in cnf_store <- clause :: cnf_store
          );
          ignore (__add_clause solver l);
        end

    method simplify = ignore (simplify solver)

    method solve =
      if should_dump_cnf then (
        self#dump_cnf
      );
      solve solver

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
