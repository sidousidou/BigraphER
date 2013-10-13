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
   let iter_f i _ = 
     if v_flag then printf "\r%3d states found%!" (i + 1)
     else () in
   if Brs.is_valid_p_l reacts
   then
     (if (mask land 0b1000) > 0
      then
        (printf "%s\n\n%!" (colorise `bold "Test for BRS");
         (try
            let (ts, stats) = 
	      Brs.bfs s reacts 1000 50 iter_f in
	    printf "%sStates              : %s\n\
                    Reactions           : %s\n\
                    Occurrences         : %s\n\
                    Execution time (s)  : %s\n\n"
	      (if v_flag then "\n" else "")
	      (colorise `bold (colorise `blue (sprintf "%-8d" stats.Brs.s)))
	      (colorise `bold (colorise `blue (sprintf "%-8d" stats.Brs.r)))
	      (colorise `bold (colorise `blue (sprintf "%-8d" stats.Brs.o)))
	      (colorise `bold (colorise `blue (sprintf "%f" stats.Brs.t)));
             if path <> ""
             then
               (Export.write_ts ts "ts" path v_flag;
                Brs.iter_states
                  (fun i s ->
                    let name = sprintf "%d" i
                    in Export.write_big s name path v_flag)
                  ts)
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
            let (_, stats) = 
	      Brs.sim s reacts 1000 50 iter_f in
	    printf "%sStates              : %s\n\
                    Reactions           : %s\n\
                    Occurrences         : %s\n\
                    Execution time (s)  : %s\n\n"
	      (if v_flag then "\n" else "")
	      (colorise `bold (colorise `blue (sprintf "%-8d" stats.Brs.s)))
	      (colorise `bold (colorise `blue (sprintf "%-8d" stats.Brs.r)))
	      (colorise `bold (colorise `blue (sprintf "%-8d" stats.Brs.o)))
	      (colorise `bold (colorise `blue (sprintf "%f" stats.Brs.t)));
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
            let (ctmc, stats) = 
	      Sbrs.bfs s sreacts 1000 50 iter_f in
            (printf "%sStates              : %s\n\
                    Reactions           : %s\n\
                    Occurrences         : %s\n\
                    Execution time (s)  : %s\n\n"
	       (if v_flag then "\n" else "")
	       (colorise `bold (colorise `blue (sprintf "%-8d" stats.Sbrs.s)))
	       (colorise `bold (colorise `blue (sprintf "%-8d" stats.Sbrs.r)))
	       (colorise `bold (colorise `blue (sprintf "%-8d" stats.Sbrs.o)))
	       (colorise `bold (colorise `blue (sprintf "%f" stats.Sbrs.t)));
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
            let (_, stats) = 
	      Sbrs.sim s sreacts 5000.0 50 iter_f in 
	    printf "%sStates              : %s\n\
                    Reactions           : %s\n\
                    Occurrences         : %s\n\
                    Execution time (s)  : %s\n\
                    Simulation time     : %s\n\n"
	      (if v_flag then "\n" else "")
	      (colorise `bold (colorise `blue (sprintf "%-8d" stats.Sbrs.s)))
	      (colorise `bold (colorise `blue (sprintf "%-8d" stats.Sbrs.r)))
	      (colorise `bold (colorise `blue (sprintf "%-8d" stats.Sbrs.o)))
	      (colorise `bold (colorise `blue (sprintf "%f" stats.Sbrs.t)))
	      (colorise `bold (colorise `blue (sprintf "%f" stats.Sbrs.sim)));
          with
          | e ->
              (printf "%s" (colorise `red "Error: ");
               printf "%s\n" (Printexc.to_string e)));
         printf "----------------------------------------\n\n")
      else ())
   else assert false)
  

