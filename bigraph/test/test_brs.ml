open Bigraph
open Big
module ST = CI_utils.Shippable_test
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
  and states_reference = 30
  and reacts_reference = 29
  and occurs_reference = 4495
  and print_res l =
    ST.xml_block "system-out" []
      [
        List.filter (fun (_, _, flag) -> not flag) l
        |> List.map (fun (desc, value, _) -> desc ^ " " ^ value)
        |> String.concat "     ";
      ]
  in
  let failures l =
    List.map
      (fun (id, reference, out) -> ST.assert_eq_int id reference out)
      l
  and ass_list s r o =
    [
      ("States", states_reference, s);
      ("Transitions", reacts_reference, r);
      ("Occurrences", occurs_reference, o);
    ]
  in
  let testcases =
    [
      (let stats =
         try
           snd
             (BRS.bfs ~s0:s ~priorities:reacts ~predicates:[] ~max:1000
                iter_f)
         with BRS.MAX (_, stats) -> stats
       in
       ( "brs",
         __MODULE__,
         print_res @@ Rs.stats_descr stats,
         failures Rs.(ass_list stats.states stats.trans stats.occs) ));
      (let stats =
         try
           snd
             (BRS.sim ~s0:s ~priorities:reacts ~predicates:[] ~stop:1000
                ~init_size:50 iter_f)
         with BRS.LIMIT (_, stats) | BRS.DEADLOCK (_, stats, _) -> stats
       in
       ( "sim_brs",
         __MODULE__,
         print_res @@ Rs.stats_descr stats,
         failures
           [
             ("States", states_reference, Rs.(stats.states));
             ("Reactions", reacts_reference, Rs.(stats.trans));
           ] ));
      (let stats =
         try
           snd
             (SBRS.bfs ~s0:s ~priorities:sreacts ~predicates:[] ~max:1000
                iter_f)
         with SBRS.MAX (_, stats) -> stats
       in
       ( "sbrs",
         __MODULE__,
         print_res @@ Rs.stats_descr stats,
         failures Rs.(ass_list stats.states stats.trans stats.occs) ));
      (let stats =
         try
           snd
             (SBRS.sim ~s0:s ~priorities:sreacts ~predicates:[] ~stop:5000.0
                ~init_size:50 iter_f)
         with SBRS.LIMIT (_, stats) | SBRS.DEADLOCK (_, stats, _) -> stats
       in
       ( "sim_sbrs",
         __MODULE__,
         print_res @@ Rs.stats_descr stats,
         failures Rs.(ass_list stats.states stats.trans stats.occs) ));
    ]
  in
  print_endline "OK";
  IO.mkdir Sys.argv.(1);
  ST.(write_xml (testsuite "test_brs" testcases) Sys.argv.(1) Sys.argv.(2));
  print_endline "Done!";
  exit 0
