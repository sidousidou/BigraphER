open Printf
open Utils

let version = 0.5
  
let verbose_bool = ref false

let version_bool = ref false
  
let model_str = ref ""

let consts_str = ref ""
  
let properties_str = ref ""
  
(*let priorities_str = ref ""*)
  
let export_trans_str = ref ""

let export_csl_str = ref ""
  
let export_trans_dot_str = ref ""
  
let export_store = ref ""
  
let export_states_bool = ref false
  
let sim_bool = ref false
  
let t_max = ref 100.0
  
let steps = ref 100
  
let usage =
  "Usage: bigrapher [options] <model-file> [<properties-file>] [more-options]"
  
let speclist =
  [ (*("-p", (Arg.String (fun s -> priorities_str := s)), 
     "<file> Import rule priorities from a file");*)
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
     "<path> Export each bigraph in the store to a dot file");
    ("-consts", (Arg.String (fun s -> consts_str := s)),
     "<ide=val, ...> Specify constants") ]
  
let print_version () = 
  printf "BigraphER version %f\n" version
  
let _ =
  try
    Export.check_setup ();
    Arg.parse (Arg.align speclist) (fun filename ->
      if Filename.check_suffix filename "big" then model_str := filename
      else if Filename.check_suffix filename "bilog" then properties_str := filename
      else raise (Arg.Bad ("Bad argument: " ^ filename))) usage;
    if !version_bool then print_version ()
    else begin 
      let decs, brs =
	if !model_str = "" then raise (Arg.Bad ("Error: Model file missing.\n" ^ usage))
	else (let lexbuf, file = open_lex !model_str in
	      let out = Parser_main.model Lexer_main.lex lexbuf in 
	      (close_in file; out)) in
      let env = Store.init_env decs in
      Store.parse_consts !consts_str env;
      Store.store_decs decs env;
      let s0, stochastic, p_classes =
	Store.store_brs brs env in
      printf "s0:\n%s\nReaction rules:\n" (Big.string_of_bg s0);
      List.iter (fun c -> 
	let _aux r = 
	  String.concat ", " (List.map (sprintf "%s") r) in
	match c with
	| Store.P_class_ide r -> printf "%s\n" (_aux r)	
	| Store.P_rclass_ide r -> printf "%s\n" (_aux r)) p_classes;
      printf "%d\n" (List.fold_left (fun acc c -> 
	match c with
	| Store.P_class_ide r -> acc + (List.length r)
	| Store.P_rclass_ide r -> acc + (List.length r)) 0 p_classes);
      if not stochastic then begin
	()
      end
      else begin
	()
      end;
      Export.wait_before_exit !verbose_bool
    end
  with
  | Arg.Bad m -> prerr_endline m; exit 1 
  | Utils.PARSE_ERROR -> prerr_endline "Parsing unsuccesful"; exit 1
  | Parsing.Parse_error -> prerr_endline "Parsing unsuccesful"; exit 1
  | _ -> Printexc.print_backtrace stderr; exit 1
  

(* Parse model file *)
        (*let (store, s0, ctmc) = Env.eval_model m !model_str in
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
        in*)
          (* Output for debug *)
    
