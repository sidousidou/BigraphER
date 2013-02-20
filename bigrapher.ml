open Syntax
  
open Printf
  
open Pretty
  
let verbose_bool = ref false

let version_bool = ref false
  
let model_str = ref ""
  
let properties_str = ref ""
  
let priorities_str = ref ""
  
let export_trans_str = ref ""

let export_csl_str = ref ""
  
let export_trans_dot_str = ref ""
  
let export_store = ref ""
  
let export_states_bool = ref false
  
let sim_bool = ref false
  
let t_max = ref 100.0
  
let steps = ref 100
  
let usage =
  "Usage: bigmc [options] <model-file> [<properties-file>] [more-options]"
  
let speclist =
  [ ("-p", (Arg.String (fun s -> priorities_str := s)),
     "<file> Import rule priorities from a file");
    ("-v", (Arg.Unit (fun () -> verbose_bool := true)), " Verbose output");
    ("-V", (Arg.Unit (fun () -> version_bool := true)), " Program version");
    ("-t", (Arg.Float (fun t -> t_max := t)),
     "<float> Set the termination time of the simulation");
    ("-s", (Arg.Int (fun n -> steps := n)),
     "<int> Set the number of steps of the simulation");
    ("-sim", (Arg.Unit (fun () -> sim_bool := true)), " Simulate the model");
    ("-exporttrans", (Arg.String (fun s -> export_trans_str := s)),
     "<file> Export the transition system to a file");
    ("-exportdot", (Arg.String (fun s -> export_trans_dot_str := s)),
     "<path> Export the transition system to a dot file");
    ("-exportstates", (Arg.Unit (fun () -> export_states_bool := true)),
     " Export each state to a file");
    ("-exportcsl", (Arg.String (fun s -> export_csl_str := s)),
     "<file> Export the labelling function to a file"); 
    ("-exportstore", (Arg.String (fun s -> export_store := s)),
     "<path> Export each bigraph in the store to a dot file") ]
  
(* check if dot and bimatch are in the path *)
let check_cmd cmd a code =
  let (read, write) = Unix.pipe () in
  let _ = Unix.create_process cmd [| cmd; a |] Unix.stdin write write in
  let (_, status) = Unix.wait ()
  in
    (Unix.close read;
     Unix.close write;
     match status with
     | Unix.WEXITED i ->
         if i = code
         then ()
         else
           failwith
             (sprintf "Error: %s is not installed in the system\n" cmd)
     | _ ->
         failwith (sprintf "Error: %s is not installed in the system\n" cmd))

(* print version *)
let print_version () = 
  printf "BigraphER version 0.3\n"
  
(* bimatch does not return the correct exit code*)
let check_setup () = (check_cmd "dot" "-V" 0; check_cmd "bimatch" "-v" 1)

let init_lex file path = 
  let lexbuf = Lexing.from_channel file
    in
      lexbuf.Lexing.lex_curr_p <-
          {
            Lexing.pos_fname = Filename.basename path;
            Lexing.pos_lnum = 1;
            Lexing.pos_bol = 0;
            Lexing.pos_cnum = 0;
          };
       lexbuf          
  
let () =
  try
    (check_setup ();
     let t0 = Unix.gettimeofday ()
     in
       (Arg.parse (Arg.align speclist)
          (fun filename ->
             if Filename.check_suffix filename "big"
             then model_str := filename
             else
               if Filename.check_suffix filename "bilog"
               then properties_str := filename
               else raise (Arg.Bad ("Bad argument: " ^ filename))
           ) usage;
        if !version_bool
        then (print_version (); exit 0)
        else
        (* Parse model file *)
        let m =
          if !model_str = ""
          then raise (Arg.Bad ("Model file missing.\n" ^ usage))
          else
            (let file = open_in !model_str in
             let lexbuf = init_lex file !model_str in
                let out = Parser_mod.model Lexer_mod.lex lexbuf
                in (close_in file; out)) in
        let (store, s0, ctmc) = Env.eval_model m !model_str in
        (* Parse properties file *)
        let bilog =
          if !properties_str = ""
          then ([], B_null)
          else 
            (let file = open_in !properties_str in
             let lexbuf = init_lex file !properties_str in
                let out = Parser_pred.preds Lexer_pred.lex lexbuf
                in (close_in file; out)) in
        let preds = Env.eval_properties bilog !properties_str in
        (* Parse priorities file *)
        let pri =
          if !priorities_str = ""
          then []
          else
            (let file = open_in !priorities_str in
             let lexbuf = init_lex file !priorities_str in
                let out = Parser_pri.priorities Lexer_pri.lex lexbuf
                in (close_in file; out)) in
        let pri_queue = Env.eval_priorities pri store ctmc !priorities_str
        in
          (* Output for debug *)
          (if !verbose_bool
      			then
         			((*printf "%s\n" (string_of_model m !model_str);
          		printf "%s\n" (string_of_priorities pri !priorities_str);
          		Env.print_store store;
          		Env.print_pri pri_queue;
          		printf "%s\n" (string_of_preds (snd bilog) !properties_str);
          		Env.print_pred preds*)
          		)
      			else
       				());
          (* Export in svg the model *)
          (if !export_store = ""
            then ()
            else 
              (Export.export_dot_bilog preds !export_store !verbose_bool;
              Export.export_dot store !export_store !verbose_bool));
          (if !sim_bool
           (* Simulation *)
           then
             if ctmc
             (* Stochastic *)
             then
               (let (trace, lab_f) =
                Run.sim_s s0 pri_queue store !t_max preds !verbose_bool
                in
                  (* Export trace to text file *)    
                  (if !export_trans_str = ""
                   then ()
                   else
                    Export.trace_s_out trace !export_trans_str !verbose_bool);
                  (* Export trace to dot *)  
                  (if !export_trans_dot_str = ""
                   then ()
                   else
                    (Export.trace_s_out_dot trace !export_trans_dot_str
                       !export_states_bool !verbose_bool;
                    (* Export states to dot files *)
                    (if !export_states_bool
                     then
                      Export.trace_s_dot trace !export_trans_dot_str
                        !verbose_bool
                     else ())));
                  (* Export cls to file *)  
                  (if !export_csl_str = ""
                   then ()
                   else
                    Export.csl_out lab_f !export_csl_str !verbose_bool))
             (* Non deterministic *) 
             else
              (let (trace, lab_f) =
                Run.sim s0 pri_queue store !steps preds !verbose_bool
                in
                   (* Export trace to text file *)
                   (if !export_trans_str = ""
                   then ()
                   else
                     Export.trace_out trace !export_trans_str !verbose_bool);
                   (* Export trace to dot *)  
                   (if !export_trans_dot_str = ""
                   then ()
                   else
                     (Export.trace_out_dot trace !export_trans_dot_str
                       !export_states_bool !verbose_bool;
                      (* Export states to dot files *)
                      (if !export_states_bool
                       then
                         Export.trace_dot trace !export_trans_dot_str
                          !verbose_bool
                       else ())));
                   (* Export cls to file *)  
                  (if !export_csl_str = ""
                   then ()
                   else
                    Export.csl_out lab_f !export_csl_str !verbose_bool))
           (* Transition system *)            
           else
             if ctmc
             (* Stochastic *)
             then
              (let ((states, e, i), lab_f) = 
                  Run.exec_s s0 pri_queue store 1000000 preds !verbose_bool
               in 
                 (* Export CTMC to text file *)
                 (if !export_trans_str = ""
                 then ()
                 else
                   Export.markov_out (states, e) !export_trans_str
                    !verbose_bool);      
                 (* Export CTMC to dot *)  
                  (if !export_trans_dot_str = ""
                  then ()
                  else
                    (Export.markov_out_dot (states, e) !export_trans_dot_str
                      !export_states_bool !verbose_bool;
                     (* Export states to dot files *)
                    (if !export_states_bool
                     then Export.markov_dot states !export_trans_dot_str 
                      !verbose_bool
                     else ())));
                  (* Export cls to file *)  
                  (if !export_csl_str = ""
                   then ()
                   else
                    Export.csl_out lab_f !export_csl_str !verbose_bool)    
               )
             (* Non deterministic *) 
             else 
             (let ((states, e, i), lab_f) =
                Run.exec s0 pri_queue store 1000000 preds !verbose_bool
               in 
                 (* Export transition system to text file *)
                 (if !export_trans_str = ""
                 then ()
                 else
                   Export.ts_out (states, e) !export_trans_str
                   !verbose_bool);      
                 (* Export transition system to dot *)  
                  (if !export_trans_dot_str = ""
                  then ()
                  else
                    (Export.ts_out_dot (states, e) !export_trans_dot_str
                      !export_states_bool !verbose_bool;
                    (* Export states to dot files *)
                    (if !export_states_bool
                   then Export.ts_dot states !export_trans_dot_str 
                      !verbose_bool
                   else ())));
                  (* Export cls to file *)  
                  (if !export_csl_str = ""
                   then ()
                   else
                    Export.csl_out lab_f !export_csl_str !verbose_bool)    
                      )
                );
           printf "Finished in %f.\n" ((Unix.gettimeofday ()) -. t0);
           exit 0
           )
          )
  with | e -> failwith (Printexc.to_string e)
    
