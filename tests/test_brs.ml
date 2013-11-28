open Base
open Big
open Printf
open Utils
  
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
  let iter_f i _ = () in
  match Sys.argv.(1) with
  | "brs" -> (
      let (ts, stats) = 
        Brs.bfs s reacts 1000 50 iter_f in
      printf "States              : %s\n\
              Reactions           : %s\n\
              Occurrences         : %s\n"
        (colorise `bold (colorise `blue (sprintf "%-8d" stats.Brs.s)))
        (colorise `bold (colorise `blue (sprintf "%-8d" stats.Brs.r)))
        (colorise `bold (colorise `blue (sprintf "%-8d" stats.Brs.o)));
    )
  | "sym_brs" -> (
      let (_, stats) = 
	Brs.sim s reacts 1000 50 iter_f in
      printf "States              : %s\n\
              Reactions           : %s\n\
              Occurrences         : %s\n"
        (colorise `bold (colorise `blue (sprintf "%-8d" stats.Brs.s)))
        (colorise `bold (colorise `blue (sprintf "%-8d" stats.Brs.r)))
        (colorise `bold (colorise `blue (sprintf "%-8d" stats.Brs.o)));
    )
  | "sbrs" -> (
      let (ctmc, stats) = 
	Sbrs.bfs s sreacts 1000 50 iter_f in
      printf "States              : %s\n\
              Reactions           : %s\n\
              Occurrences         : %s\n"
	(colorise `bold (colorise `blue (sprintf "%-8d" stats.Sbrs.s)))
        (colorise `bold (colorise `blue (sprintf "%-8d" stats.Sbrs.r)))
        (colorise `bold (colorise `blue (sprintf "%-8d" stats.Sbrs.o)));
    )
  | "sym_sbrs" -> (
      let (_, stats) = 
	Sbrs.sim s sreacts 5000.0 50 iter_f in 
      printf "States              : %s\n\
              Reactions           : %s\n\
              Occurrences         : %s\n"
	(colorise `bold (colorise `blue (sprintf "%-8d" stats.Sbrs.s)))
	(colorise `bold (colorise `blue (sprintf "%-8d" stats.Sbrs.r)))
	(colorise `bold (colorise `blue (sprintf "%-8d" stats.Sbrs.o)));
    )
  | _ -> exit 1

  

