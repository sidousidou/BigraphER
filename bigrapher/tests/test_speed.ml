module IO = CI_utils.Io
open Printf

(* bin is assumed to print out a float *)
let running_time ~bin ~args ~f =
  let chin = Unix.open_process_in (bin ^ String.concat " " args) in
  let t = f chin in
  ignore (Unix.close_process_in chin);
  t

let avg n_runs ~bin ~args ~f =
  assert (n_runs > 0);
  let rec aux n acc =
    if n = 0 then acc else aux (n - 1) (acc +. running_time ~bin ~args ~f)
  in
  aux n_runs 0.0 /. float n_runs

let benchmark =
  [
    ([], "../examples/rts_cts.big");
    ([ "-M 100" ], "../examples/hospital.big");
    ([], "../examples/conditional_turn_taking.big");
    ([ "-M 500" ], "../examples/savannah-general.big");
  ]

let () =
  assert (Array.length Sys.argv = 2);
  let n = 5 in
  printf "test_speed (%d runs):\n" n;
  let bin = Sys.argv.(1) ^ " full --running-time "
  and f x = input_line x |> float_of_string in
  List.iter
    (fun (args, model) ->
      printf "%s: %-3gs\n" model (avg n ~bin ~args:(args @ [ model ]) ~f))
    benchmark
