open Bigraph
module Io = CI_utils.Io
module S = Solver.Make_SAT (Solver.KS)

let read_in file =
  Io.parse file |> String.concat "\n" |> fun x ->
  print_endline @@ "Parsing " ^ file;
  Big.of_string x

let () =
  let t = read_in "./files/T19.big" and p = read_in "./files/P28.big" in
  ( S.occurrences_raw ~target:t ~pattern:p |> fun xs ->
    Format.(fprintf std_formatter "@[<v>");
    List.iteri
      (fun i x ->
        Format.(
          fprintf std_formatter "Occurrence %d: @[%a@]@;" i Solver.pp_occ x))
      xs;
    Format.(fprintf std_formatter "@]") );
  exit 0
