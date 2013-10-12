open Base
  
open Big
  
open Printf
  
open Global
  
let r_p = comp (ion (Link.parse_face [ "x" ]) (Ctrl.Ctrl ("B", 1))) one
  
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
  
let main ?(path = "") mask v_flag =
  (Random.self_init ();
   if Brs.is_valid_p_l reacts
   then
     (if (mask land 0b1000) > 0
      then
        (printf "%s\n\n%!" (colorise `bold "Test for BRS");
         (try
            let (ts, (t, s, r, o)) = 
	      Brs.bfs s reacts 1000 50 v_flag in
	    printf "States              : %s\n\
                    Reactions           : %s\n\
                    Occurrences         : %s\n\
                    Execution time (s)  : %s\n\n"
	      (colorise `bold (colorise `blue (sprintf "%-8d" s)))
	      (colorise `bold (colorise `blue (sprintf "%-8d" r)))
	      (colorise `bold (colorise `blue (sprintf "%-8d" o)))
	      (colorise `bold (colorise `blue (sprintf "%f" t)));
             if path <> ""
             then
               (Export.write_ts ts "ts" path v_flag;
                Brs.V.iter
                  (fun (i, s) ->
                    let name = sprintf "%d" i
                    in Export.write_big s name path v_flag)
                  ts.Brs.v)
             else ()
          with
          | e ->
              (printf "%s" (colorise `red "Error: ");
               printf "%s\n" (Printexc.to_string e)));
         printf "----------------------------------------\n\n")
      else ();
      if (mask land 0b0100) > 0
      then
        (printf "%s\n\n%!" (colorise `bold "Test for BRS simulation");
         (try
            let (_, (t, s, r, o)) = 
	      Brs.sim s reacts 1000 50 v_flag in
	    printf "States              : %s\n\
                    Reactions           : %s\n\
                    Occurrences         : %s\n\
                    Execution time (s)  : %s\n\n"
	      (colorise `bold (colorise `blue (sprintf "%-8d" s)))
	      (colorise `bold (colorise `blue (sprintf "%-8d" r)))
	      (colorise `bold (colorise `blue (sprintf "%-8d" o)))
	      (colorise `bold (colorise `blue (sprintf "%f" t)));
          with
          | e ->
              (printf "%s" (colorise `red "Error: ");
               printf "%s\n" (Printexc.to_string e)));
         printf "----------------------------------------\n\n")
      else ())
   else assert false;
   if Sbrs.is_valid_p_l sreacts
   then
     (if (mask land 0b0010) > 0
      then
        (printf "%s\n\n%!" (colorise `bold "Test for SBRS");
         (try
            let (ctmc, (t, s, r, o)) = 
	      Sbrs.bfs s sreacts 1000 50 v_flag in
            (printf "States              : %s\n\
                    Reactions           : %s\n\
                    Occurrences         : %s\n\
                    Execution time (s)  : %s\n\n"
	       (colorise `bold (colorise `blue (sprintf "%-8d" s)))
	       (colorise `bold (colorise `blue (sprintf "%-8d" r)))
	       (colorise `bold (colorise `blue (sprintf "%-8d" o)))
	       (colorise `bold (colorise `blue (sprintf "%f" t)));
             if path <> ""
             then Export.write_ctmc ctmc "ctmc" path v_flag
             else ())
          with
          | e ->
              (printf "%s" (colorise `red "Error: ");
               printf "%s\n" (Printexc.to_string e)));
         printf "----------------------------------------\n\n")
      else ();
      if (mask land 0b0001) > 0
      then
        (printf "%s\n\n%!" (colorise `bold "Test for SBRS simulation");
         (try
            let (_, (t, t_sim, s, r, o)) = 
	      Sbrs.sim s sreacts 5000.0 50 v_flag in 
	    printf "States              : %s\n\
                    Reactions           : %s\n\
                    Occurrences         : %s\n\
                    Execution time (s)  : %s\n\
                    Simulation time     : %s\n\n"
	      (colorise `bold (colorise `blue (sprintf "%-8d" s)))
	      (colorise `bold (colorise `blue (sprintf "%-8d" r)))
	      (colorise `bold (colorise `blue (sprintf "%-8d" o)))
	      (colorise `bold (colorise `blue (sprintf "%f" t)))
	      (colorise `bold (colorise `blue (sprintf "%f" t_sim)));
          with
          | e ->
              (printf "%s" (colorise `red "Error: ");
               printf "%s\n" (Printexc.to_string e)));
         printf "----------------------------------------\n\n")
      else ())
   else assert false)
  

