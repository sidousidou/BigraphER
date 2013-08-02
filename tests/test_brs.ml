open Base
open Big
open Brs
open Format
open Export

let s = 
  close (Link.parse_face ["x"]) 
    (par_of_list [ comp (ion (Link.parse_face (["x"])) (Ctrl ("B", 1))) one; 
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		   comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one
		 ])

let r = 
  par 
    (comp (ion (Link.parse_face (["x"])) (Ctrl ("B", 1))) one) 
    (comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one)

let r_p = 
  comp (ion (Link.parse_face (["x"])) (Ctrl ("B", 1))) one

let g = 
  par_of_list [ comp (ion (Link.parse_face (["x"])) (Ctrl ("B", 1))) one; 
		comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one;
		comp (ion (Link.parse_face (["x"])) (Ctrl ("A", 1))) one
	      ]

let reacts = 
  [ P_class [ { rdx = r; rct = r_p };
	      { rdx = g; rct = r }
	    ]
  ]
  
let _ =
  check_setup ();
  let verb = 
    try match Sys.argv.(2) with
      | "v" -> true
      | _ -> raise (Invalid_argument "") 
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
  if is_valid_p_l reacts then begin
    let ts, stats = bfs s reacts 10000 50 verb in
    printf "@[%s@]@." (string_of_stats stats);
    Export.write_svg (to_dot ts) "ts" path verb;
    V.iter (fun (i, s) ->
      let name = sprintf "%d" i in
      Export.write_svg (get_dot s name) name path verb) ts.v;
    let _, stats = sim s reacts 10000 50 verb in
    printf "@[%s@]@." (string_of_stats stats);
    wait_before_exit verb;
    Gc.full_major ();
  end else eprintf "@[Error: Invalid reactions.@]@."
