open Bigraph
open Bigraph.Solver
open Printf

let rec read_lines out file =
  try read_lines ((input_line file) :: out) file
  with
  | End_of_file -> (close_in file; List.rev out)

let parse path =
  open_in path
  |> read_lines []
  |> String.concat "\n"

type solver = SAT | PB

let parse_solver s = match (String.lowercase_ascii s) with
  | "sat" -> SAT
  | "pb" -> PB
  | _ -> raise Exit

let print_info =
  let str = [
      "commandline = " ^ (String.concat " " (Array.to_list Sys.argv));
      "solver = " ^ Sys.argv.(1);
      "pattern_file = " ^ Sys.argv.(2);
      "target_file = " ^ Sys.argv.(3)
    ]
  in
  print_endline (String.concat "\n" str)


let print_res (res : Bigraph.Solver.occ list) =
  let out = List.map (fun (sol : Bigraph.Solver.occ) ->
      sprintf "mapping = %s" (Iso.to_string (sol.nodes)) (* (Iso.to_string j) *)
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

let () =
  let () = if Array.length Sys.argv != 4 then
    (print_endline "Usage: matcher <sat|pb> <pattern> <target>"; raise Exit;)
    else ()
  in
  let () = print_info in
  (* let p = Big.of_string (parse (Sys.argv.(2))) in
   * let t = Big.of_string (parse (Sys.argv.(3))) in *)
  let p = Big.parse (parse (Sys.argv.(2))) in
  let t = Big.parse (parse (Sys.argv.(3))) in
  let s = Mtime_clock.now () in
  let solver = parse_solver Sys.argv.(1) in
  let (module S) =
    (match solver with
      | SAT -> (module SAT_SOL : Solver.M)
      | PB -> (module PB_SOL : Solver.M)
    )
  in
  let res = S.occurrences ~target:t ~pattern:p in
  let e = Mtime_clock.now () in
  print_sol res (Mtime.span s e)
