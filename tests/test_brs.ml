open Base
open Big
open Format

let r_p = 
  comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("B", 1))) one

let r = 
  par 
    (comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("B", 1))) one) 
    (comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one)

let s = 
  close (Link.parse_face ["x"]) 
    (par_of_list [ comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("B", 1))) one; 
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		 ])

let g = 
  par_of_list [ comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("B", 1))) one; 
		comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
		comp (ion (Link.parse_face (["x"])) (Ctrl.Ctrl ("A", 1))) one;
	      ]

let reacts = 
  [ Brs.P_class [ { Brs.rdx = r; rct = r_p; };
		  { Brs.rdx = g; rct = r; };
		]
  ]

let sreacts =
  [ Sbrs.P_class [ { Sbrs.rdx = r; rct = r_p; rate = 2.0; };
		   { Sbrs.rdx = g; rct = r; rate = 4.0; };
		 ]
  ]
  
let _ =
  Export.check_graphviz ();
  let verb = 
    try match Sys.argv.(2) with
      | "v" -> true
      | a -> raise (Invalid_argument a) 
    with
    | _ -> false
  and path = 
    try Unix.access Sys.argv.(1) [Unix.W_OK; Unix.F_OK];
	Sys.argv.(1) 
    with
      | Unix.Unix_error (err, _, arg) -> 
	(eprintf "@[Error: cannot acccess %s: %s@]@." 
	   arg (Unix.error_message err); exit 1)
      | _ -> (eprintf "@[Usage: test_brs PATH [v]@]@."; exit 1) in 
  Random.self_init ();
  if Brs.is_valid_p_l reacts then begin
    let (ts, stats) = Brs.bfs s reacts 1000 50 verb in
    printf "@[%s@]@." (Brs.string_of_stats stats);
    Export.write_ts ts "ts" path verb;
    Brs.V.iter (fun (i, s) ->
      let name = sprintf "%d" i in
      Export.write_big s name path verb) ts.Brs.v;
    let (_, stats) = Brs.sim s reacts 1000 50 verb in
    printf "@[%s@]@." (Brs.string_of_stats stats)
  end else eprintf "@[Error: Invalid reactions.@]@.";
  if Sbrs.is_valid_p_l sreacts then begin
    let (ctmc, stats) = Sbrs.bfs s sreacts 1000 50 verb in
    printf "@[%s@]@." (Sbrs.string_of_stats stats);
    Export.write_ctmc ctmc "ctmc" path verb;
    let (_, stats) = Sbrs.sim s sreacts 5000.0 50 verb in
    printf "@[%s@]@." (Sbrs.string_of_stats_sim stats)
  end else eprintf "@[Error: Invalid stochastic reactions.@]@.";
  Export.wait_before_exit verb;
  Gc.full_major ();
    
