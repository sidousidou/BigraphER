open Printf
open Minisat

(*
 * Returns the string obtained from [str] by dropping the first
 * [n] characters. [n] must be smaller than or equal to the length
 * of the string [str].
 *)
let drop str n =
  let l = String.length str in
  String.sub str n (l - n)

(*
 * Returns the first [n] characters of the string [str] as a string.
 * [n] must be smaller than or equal to the length of the string [str].
 *)
let take str n = String.sub str 0 n

(*
 * Splits a string [str] whenever a character [ch] is encountered.
 * The returns value is a list of strings.
 *)
let split str ch =
  let rec split' str l =
    try
      let i = String.index str ch in
      let t = take str i in
      let str' = drop str (i + 1) in
      let l' = t :: l in
      split' str' l'
    with Not_found -> List.rev (str :: l)
  in
  split' str []

(*
 * Processes the content of [file], adds the variables and clauses to Minisat
 * and returns a mapping between names and Minisat variables.
 *)
let process_file solver file =
  (* Mapping between variable names and indices. *)
  let vars = Hashtbl.create 0 in

  (* Processes a line containing a variable definition. *)
  let process_var line =
    let l = String.length line in
    assert (l > 2);
    assert (line.[1] = ' ');
    let name = drop line 2 in
    let v = new_var solver in
    Hashtbl.add vars name v
  in

  (* Processes a line containing a clause. *)
  let process_clause line =
    let l = String.length line in
    assert (l > 2);
    assert (line.[1] = ' ');
    let lits =
      List.map
        (fun lit ->
          if lit.[0] = '-' then (false, drop lit 1) else (true, lit))
        (split (drop line 2) ' ')
    in
    let clause =
      List.map
        (fun (sign, name) ->
          let var = Hashtbl.find vars name in
          if sign then Minisat.pos_lit var else Minisat.neg_lit var)
        lits
    in
    add_clause solver clause
  in

  (* Read a new line and processes its content. *)
  let rec process_line () =
    try
      let line = input_line file in
      ( if line = "" then ()
      else
        match line.[0] with
        | 'v' -> process_var line
        | 'c' -> process_clause line
        | '#' -> ()
        | _ -> assert false );
      process_line ()
    with End_of_file -> ()
  in

  process_line ();
  vars

(* Reads a given file and solves the instance. *)
let solve file =
  let solver = create () in
  let vars = process_file solver file in
  simplify solver;
  match solve solver with
  | Minisat.UNSAT -> printf "unsat\n"
  | Minisat.SAT ->
      printf "sat\n";
      Hashtbl.iter
        (fun name v ->
          printf "  %s=%s\n" name
            (Minisat.string_of_value (value_of solver v)))
        vars

let solve_all xs file =
  let solver = create () in
  let vars = process_file solver file in
  let print_sols =
    List.iteri (fun i m ->
        printf "Solution %d:\n" i;
        Hashtbl.iter
          (fun name v ->
            printf "  %s=%s\n" name
              ( if List.mem v m then Minisat.string_of_value Minisat.True
              else Minisat.string_of_value Minisat.False ))
          vars)
  in
  simplify solver;
  let sols = get_models ~vars:(List.map (Hashtbl.find vars) xs) solver in
  match sols with [] -> printf "unsat\n" | _ -> print_sols sols

(* Solve the files given on the command line, or read one from standard input
   if none is given. Arguments --all computes all the solutions and --vars
   1,2,3 lists all non-auxiliary variables in the problem. --vars must follow
   --all. *)
let () =
  let scan_files solve_f i len =
    Array.iter
      (fun fname ->
        try
          printf "Solving %s...\n" fname;
          solve_f (open_in fname)
        with Sys_error msg -> printf "ERROR: %s\n" msg)
      (Array.sub Sys.argv i len)
  in
  let argc = Array.length Sys.argv in
  if argc = 1 then solve stdin
  else if String.equal "--all" Sys.argv.(1) then
    if argc = 2 then solve_all [] stdin
    else if String.equal "--vars" Sys.argv.(2) then
      let vars = String.split_on_char ',' Sys.argv.(3) in
      if argc = 4 then solve_all vars stdin
      else scan_files (solve_all vars) 4 (argc - 4)
    else scan_files (solve_all []) 2 (argc - 2)
  else scan_files solve 1 (argc - 1)
