open Bigraph
open Big
module IO = CI_utils.Io
module S = Solver.Make_SAT (Solver.MS)
module BRS = Brs.Make (S)
module SBRS = Sbrs.Make (S)

let r_p = comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("B", [], 1))) one

let r =
  par
    (comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("B", [], 1))) one)
    (comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", [], 1))) one)

let s =
  close
    (Link.parse_face [ "x" ])
    (par
       (comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("B", [], 1))) one)
       (par_seq ~start:0 ~stop:29 (fun _ ->
            comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", [], 1))) one)))

let g =
  par_of_list
    [
      comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("B", [], 1))) one;
      comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", [], 1))) one;
      comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", [], 1))) one;
    ]

let reacts =
  BRS.
    [
      P_class
        [
          parse_react_unsafe ~name:"" ~lhs:r ~rhs:r_p () None;
          parse_react_unsafe ~name:"" ~lhs:g ~rhs:r () None;
        ];
    ]

let sreacts =
  SBRS.
    [
      P_class
        [
          parse_react_unsafe ~name:"" ~lhs:r ~rhs:r_p 2.0 None;
          parse_react_unsafe ~name:"" ~lhs:g ~rhs:r 4.0 None;
        ];
    ]

let () =
  let iter_f _ _ = ()
  and print_res s =
    Printf.printf "States: %d\nTransitions: %d\nOccurrences: %d\n"
      Rs.(s.states)
      Rs.(s.trans)
      Rs.(s.occs)
  in
  Printf.printf "brs\n";
  (try snd (BRS.bfs ~s0:s ~priorities:reacts ~predicates:[] ~max:1000 iter_f)
   with BRS.MAX (_, stats) -> stats)
  |> print_res;
  Printf.printf "sim_brs\n";
  (try
     snd
       (BRS.sim ~s0:s ~priorities:reacts ~predicates:[] ~stop:1000
          ~init_size:50 iter_f)
   with BRS.LIMIT (_, stats) | BRS.DEADLOCK (_, stats, _) -> stats)
  |> print_res;
  Printf.printf "sbrs\n";
  (try
     snd (SBRS.bfs ~s0:s ~priorities:sreacts ~predicates:[] ~max:1000 iter_f)
   with SBRS.MAX (_, stats) -> stats)
  |> print_res;
  Printf.printf "sim_sbrs\n";
  (try
     snd
       (SBRS.sim ~s0:s ~priorities:sreacts ~predicates:[] ~stop:5000.0
          ~init_size:50 iter_f)
   with SBRS.LIMIT (_, stats) | SBRS.DEADLOCK (_, stats, _) -> stats)
  |> print_res;
  exit 0
