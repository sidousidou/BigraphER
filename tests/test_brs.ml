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

let assert_eq_int name reference out =
  if out = reference then ""
  else  sprintf "<failure>%s: %-8d != %-8d</failure>" name out reference
    
let () =
  Random.self_init ();
  let iter_f _ _ = ()
  and states_reference = 30
  and reacts_reference = 29
  and occurs_reference = 4495
  and print_res s r o =
    printf "States              : %s\n\
            Reactions           : %s\n\
            Occurrences         : %s\n"
           (colorise `bold (colorise `blue (sprintf "%-8d" s)))
           (colorise `bold (colorise `blue (sprintf "%-8d" r)))
           (colorise `bold (colorise `blue (sprintf "%-8d" o))) in 
  match Sys.argv.(1) with
  | "brs" ->
     let (_, stats) = 
       Brs.bfs s reacts 1000 50 iter_f in
     print_res stats.Brs.s stats.Brs.r stats.Brs.o;
  | "sim_brs" -> 
     let (_, stats) = 
       Brs.sim s reacts 1000 50 iter_f in
     print_res stats.Brs.s stats.Brs.r stats.Brs.o;
  | "sbrs" ->
     let (_, stats) = 
       Sbrs.bfs s sreacts 1000 50 iter_f in
     print_res stats.Sbrs.s stats.Sbrs.r stats.Sbrs.o;
  | "sim_sbrs" ->
     let (_, stats) = 
       Sbrs.sim s sreacts 5000.0 50 iter_f in 
     print_res stats.Sbrs.s stats.Sbrs.r stats.Sbrs.o;
  | _ -> exit 1
	      
