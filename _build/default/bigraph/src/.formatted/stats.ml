type t = { time : float; states : int; trans : int; occs : int }

let init ~t0 ~states ~trans ~occs =
  { time = Unix.gettimeofday () -. t0; states; trans; occs }

let descr stats =
  [
    ("Build time:", Printf.sprintf "%-3g" stats.time, true);
    ("States:", Printf.sprintf "%-8d" stats.states, false);
    ("Transitions:", Printf.sprintf "%-8d" stats.trans, false);
    ("Occurrences:", Printf.sprintf "%-8d" stats.occs, false);
  ]

let to_string stats =
  descr stats
  |> List.map (fun (d, v, _) -> d ^ "\t" ^ v)
  |> String.concat "\n"
