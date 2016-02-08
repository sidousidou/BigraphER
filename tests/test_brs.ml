open Big
open Printf
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
  [ Brs.P_class [ { Brs.rdx = r; rct = r_p; eta = None };
		  { Brs.rdx = g; rct = r; eta = None } ] ]
  
let sreacts =
  [ Sbrs.P_class
      [ { Sbrs.rdx = r; rct = r_p; rate = 2.0; eta = None };
        { Sbrs.rdx = g; rct = r; rate = 4.0; eta = None } ] ]

let () =
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
	 print_res stats.Brs.states stats.Brs.trans stats.Brs.occs,
	 failures (ass_list stats.Brs.states stats.Brs.trans stats.Brs.occs))
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
	  print_res stats.Brs.states stats.Brs.trans stats.Brs.occs,
	  failures [("States", states_reference, stats.Brs.states);
		    ("Reactions", reacts_reference, stats.Brs.trans);
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
	  print_res stats.Sbrs.states stats.Sbrs.trans stats.Sbrs.occs,
	  failures (ass_list stats.Sbrs.states stats.Sbrs.trans stats.Sbrs.occs))
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
	  print_res stats.Sbrs.states stats.Sbrs.trans stats.Sbrs.occs,
	  failures (ass_list stats.Sbrs.states stats.Sbrs.trans stats.Sbrs.occs))
       end; ] in
  write_xml (testsuite "test_brs" testcases) Sys.argv.(1) Sys.argv.(2)

	      
