open Base
open Big
open Printf
open Junit
       
let r_p = 
  comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("B", 1))) one
  
let r =
  par (comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("B", 1))) one)
    (comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one)
  
let s =
  close (Link.parse_face [ "x" ])
    (par_of_list
       [ comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("B", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
	 comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
	 comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
	 comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
	 comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
	 comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
	 comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
         comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one ])
  
let g =
  par_of_list
    [ comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("B", 1))) one;
      comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one;
      comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("A", 1))) one ]
  
let reacts =
  [ Brs.P_class [ { Brs.rdx = r; rct = r_p; }; { Brs.rdx = g; rct = r; } ] ]
  
let sreacts =
  [ Sbrs.P_class
      [ { Sbrs.rdx = r; rct = r_p; rate = 2.0; };
        { Sbrs.rdx = g; rct = r; rate = 4.0; } ] ]

let () =
  Random.self_init ();
  let iter_f _ _ = ()
  and states_reference = 30
  and reacts_reference = 29
  and occurs_reference = 4495
  and print_res s r o =
    xml_block "system-out" []
    	      [sprintf "States: %-8d     Reactions: %-8d     Occurrences: %-8d" s r o] in
  let failures l = List.map (fun (id, reference, out) ->
			     assert_eq_int id reference out) l
  and ass_list s r o = [("States", states_reference, s);
			("Reactions", reacts_reference, r);
			("Occurrences", occurs_reference, o)] in
  let testcases =
    [  begin
	let (_, stats) = 
	  Brs.bfs s reacts 1000 50 iter_f in
	("brs",
	 "test_brs.ml",
	 print_res stats.Brs.s stats.Brs.r stats.Brs.o,
	 failures (ass_list stats.Brs.s stats.Brs.r stats.Brs.o))
      end;
       begin
	 let (_, stats) = 
	   Brs.sim s reacts 1000 50 iter_f in
	 ("sim_brs",
	  "test_brs.ml",
	  print_res stats.Brs.s stats.Brs.r stats.Brs.o,
	  failures (ass_list stats.Brs.s stats.Brs.r stats.Brs.o))
       end;
       begin
	 let (_, stats) = 
	   Sbrs.bfs s sreacts 1000 50 iter_f in
	 ("sbrs",
	  "test_brs.ml",
	  print_res stats.Sbrs.s stats.Sbrs.r stats.Sbrs.o,
	  failures (ass_list stats.Sbrs.s stats.Sbrs.r stats.Sbrs.o))
       end;
       begin
	 let (_, stats) = 
	   Sbrs.sim s sreacts 5000.0 50 iter_f in 
	 ("sim_sbrs",
	  "test_brs.ml",
	  print_res stats.Sbrs.s stats.Sbrs.r stats.Sbrs.o,
	  failures (ass_list stats.Sbrs.s stats.Sbrs.r stats.Sbrs.o))
       end; ] in
  write_xml (testsuite "test_brs" testcases) Sys.argv.(1) Sys.argv.(2)

	      
