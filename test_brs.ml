open Base
open Big
open Brs
open Printf

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
  
let () =
  let verb = 
    try
      bool_of_string Sys.argv.(1)
    with
    | _ -> false in
  Random.self_init ();
  if is_valid_p reacts then
    begin
      let ts, stats = bfs s reacts 10000 50 verb in
      printf "%s" (string_of_stats stats);
      Export.write_svg (to_dot ts) "ts" "tests/brs/" verb;
      V.iter (fun (i, s) ->
	let name = sprintf "%d" i in
	Export.write_svg (get_dot s name) name "tests/brs/" verb) ts.v;
      let ts, stats = sim s reacts 10000 50 verb in
      printf "%s" (string_of_stats stats);
    end 
  else
    printf "Invalid reactions.\n"
