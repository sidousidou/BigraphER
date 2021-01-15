open Bigraph
open Bigraph.Solver
open Printf

let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

type solver = SAT | PB | Clasp

let parse_solver s = match (String.lowercase_ascii s) with
  | "sat" -> SAT
  | "pb" -> PB
  | "clasp" -> PB
  | _ -> raise Exit


let print_res (res : Bigraph.Solver.occ list) =
  let out = List.map (fun (sol : Bigraph.Solver.occ) ->
      sprintf "mapping = %s -- %s" (Iso.to_string (sol.nodes)) (Iso.to_string (sol.edges))
    ) res in
  match out with
  | [] -> "{ }"
  | _ -> "{\n" ^ (String.concat "\n" out) ^ "\n}"

let print_sol res t =
  let str = [
      "solution_count = " ^ (string_of_int (List.length res));
      "search_time = " ^ (string_of_float (Mtime.Span.to_ms t)) ^ " ms";
      print_res res
    ]
  in
  print_endline (String.concat "\n" str)

module SAT_SOL = Solver.Make_SAT (Solver.MS)
module PB_SOL = Solver.Make_SAT (Solver.MC)
module CLASP_SOL = Solver.Make_SAT (Solver.MClasp)

let raw = ref false
let patternF = ref ""
let targetF = ref ""
let solverN = ref ""

let options = [
    ( "--raw",
      Arg.Set raw,
      "Don't filter iso" );
    ( "--solver",
      Arg.Set_string solverN,
      "Solver to use: sat | pb | clasp" );
    ( "--pattern",
      Arg.Set_string patternF,
      "Pattern file"
    );
    ( "--target",
      Arg.Set_string targetF,
      "Target file"
    )
  ]

let print_info () =
  let str = [
      "commandline = " ^ (String.concat " " (Array.to_list Sys.argv));
      "solver = " ^ !solverN;
      "pattern_file = " ^ !patternF;
      "target_file = " ^ !targetF;
      "raw matches = " ^ (string_of_bool !raw);
    ]
  in
  print_endline (String.concat "\n" str)

let () =
  Arg.parse options (fun _ -> ()) "Usage: matching_perftest.ml <options>: ";
  let () = print_info () in
  let p = Big.of_string (read_whole_file !patternF) in
  let t = Big.of_string (read_whole_file !targetF) in
  let solver = parse_solver !solverN in
  let s = Mtime_clock.now () in
  let (module S) =
    (match solver with
      | SAT -> (module SAT_SOL : Solver.M)
      | PB -> (module PB_SOL : Solver.M)
      | Clasp -> (module CLASP_SOL : Solver.M)
    )
  in
  let res = if !raw then
      S.occurrences_raw ~target:t ~pattern:p
    else
      S.occurrences ~target:t ~pattern:p in
  let e = Mtime_clock.now () in
  print_sol res (Mtime.span s e)
