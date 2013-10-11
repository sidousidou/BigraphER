open Base
open Big
open Printf
open Global

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
  
let main ?(path="") mask v_flag =
  Random.self_init ();
  if Brs.is_valid_p_l reacts then begin
    if (mask land 0b1000) > 0 then begin
      printf "%s\n\n%!" (colorise `bold "Test for BRS");
      (try
	 let (ts, stats) = Brs.bfs s reacts 1000 50 v_flag in
	 printf "%s\n%!" (Brs.string_of_stats stats);
	 if path <> "" then begin
	   Export.write_ts ts "ts" path v_flag;
	   Brs.V.iter (fun (i, s) ->
	     let name = sprintf "%d" i in
	     Export.write_big s name path v_flag) ts.Brs.v
	 end;
       with
       | e -> (printf "%s" (colorise `red "Error: ");
	       printf "%s\n" (Printexc.to_string e))); 
      printf "----------------------------------------\n\n";
    end;
    if (mask land 0b0100) > 0 then begin
      printf "%s\n\n%!" (colorise `bold "Test for BRS simulation");
      (try
	 let (_, stats) = Brs.sim s reacts 1000 50 v_flag in
	 printf "%s\n%!" (Brs.string_of_stats stats);
       with
       | e -> (printf "%s" (colorise `red "Error: ");
	       printf "%s\n" (Printexc.to_string e))); 
      printf "----------------------------------------\n\n";
    end;
  end else assert false;
  if Sbrs.is_valid_p_l sreacts then begin
    if (mask land 0b0010) > 0 then begin
      printf "%s\n\n%!" (colorise `bold "Test for SBRS");
      (try
	 let (ctmc, stats) = Sbrs.bfs s sreacts 1000 50 v_flag in
	 printf "%s\n%!" (Sbrs.string_of_stats stats);
	 if path <> "" then Export.write_ctmc ctmc "ctmc" path v_flag;
       with
       | e -> (printf "%s" (colorise `red "Error: ");
	       printf "%s\n" (Printexc.to_string e))); 
      printf "----------------------------------------\n\n";
    end;
    if (mask land 0b0001) > 0 then begin
      printf "%s\n\n%!" (colorise `bold "Test for SBRS simulation");
      (try
	 let (_, stats) = Sbrs.sim s sreacts 5000.0 50 v_flag in
	 printf "%s\n%!" (Sbrs.string_of_stats_sim stats);
       with
       | e -> (printf "%s" (colorise `red "Error: ");
	       printf "%s\n" (Printexc.to_string e)));    
      printf "----------------------------------------\n\n";
    end;
  end else assert false;
    
