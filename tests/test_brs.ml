open Big
open Junit

let r_p =
  comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("B", 1))) one

let r =
  par (comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("B", 1))) one)
    (comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one)

let s =
  close (Link.parse_face [ "x" ])
    (par_of_list
       [ comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("B", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one ])

let g =
  par_of_list
    [ comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("B", 1))) one;
      comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one;
      comp (ion (Link.parse_face [ "x" ]) (Ctrl.C ("A", 1))) one ]

let reacts =
  [ Brs.P_class [ Brs.parse_react_unsafe ~lhs:r ~rhs:r_p None;
                  Brs.parse_react_unsafe ~lhs:g ~rhs:r None ] ]

let sreacts =
  [ Sbrs.P_class
      [ Sbrs.parse_react_unsafe ~lhs:r ~rhs:r_p 2.0 None;
        Sbrs.parse_react_unsafe ~lhs:g ~rhs:r 4.0 None ] ]

let () =
  print_endline "test_brs";
  let iter_f _ _ = ()
  and states_reference = 30
  and reacts_reference = 29
  and occurs_reference = 4495
  and print_res l =
    xml_block "system-out" []
      [List.filter (fun (_, _, flag) -> not flag) l
       |> List.map (fun (desc, value, _) -> desc ^ " " ^ value)
       |> String.concat "     "] in
  let failures l = List.map (fun (id, reference, out) ->
      assert_eq_int id reference out) l
  and ass_list s r o = [("States", states_reference, s);
                        ("Transitions", reacts_reference, r);
                        ("Occurrences", occurs_reference, o)] in
  let testcases =
    [  begin
      let stats =
        try
          snd (Brs.bfs ~s0:s
                 ~priorities:reacts
                 ~predicates:[]
                 ~max:1000
                 ~iter_f)
        with
        | Brs.MAX (_, stats) -> stats in
      ("brs",
       __MODULE__,
       print_res @@ Stats.descr stats,
       failures Stats.(ass_list stats.states stats.trans stats.occs))
    end;
       begin
         let stats =
           try
             snd (Brs.sim ~s0:s
                    ~priorities:reacts
                    ~predicates:[]
                    ~stop:1000
                    ~init_size:50
                    ~iter_f)
           with
           | Brs.LIMIT (_, stats)
           | Brs.DEADLOCK (_, stats, _) -> stats in
         ("sim_brs",
          __MODULE__,
          print_res @@ Stats.descr stats,
          failures [("States", states_reference, Stats.(stats.states));
                    ("Reactions", reacts_reference, Stats.(stats.trans));
                    (* ("Occurrences", 31, stats.Brs.occs) *) (* RANDOM *)
                   ])
       end;
       begin
         let stats =
           try
             snd (Sbrs.bfs ~s0:s
                    ~priorities:sreacts
                    ~predicates:[]
                    ~max:1000
                    ~iter_f)
           with
           | Sbrs.MAX (_, stats) -> stats in
         ("sbrs",
          __MODULE__,
          print_res @@ Stats.descr stats,
          failures Stats.(ass_list stats.states stats.trans stats.occs))
       end;
       begin
         let stats =
           try
             snd (Sbrs.sim ~s0:s
                    ~priorities:sreacts
                    ~predicates:[]
                    ~stop:5000.0
                    ~init_size:50
                    ~iter_f)
           with
           | Sbrs.LIMIT (_, stats)
           | Sbrs.DEADLOCK (_, stats, _) -> stats in
         ("sim_sbrs",
          __MODULE__,
          print_res @@ Stats.descr stats,
          failures Stats.(ass_list stats.states stats.trans stats.occs))
       end; ] in
  write_xml (testsuite "test_brs" testcases) Sys.argv.(1) Sys.argv.(2)
