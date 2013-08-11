open Printf
open Utils

let version = "0.3.0"
  
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
  
let s_max = ref 10000
  
let usage =
  "Usage: bigrapher [options] <model-file> [<properties-file>] [more-options]"
  
let speclist =
  [ (*("-p", (Arg.String (fun s -> priorities_str := s)), 
     "<file> Import rule priorities from a file");*)
    ("-v", (Arg.Unit (fun () -> verbose_bool := true)), " Verbose output");
    ("-V", (Arg.Unit (fun () -> version_bool := true)), " Program version");
    ("-t", (Arg.Float (fun t -> t_max := t)),
     "<float> Set the termination time of the simulation");
    ("-max", (Arg.Int (fun n -> s_max := n)),
     "<int> Set the maximum number of states");
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
  printf "BigraphER %s\n" version

let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
 
let format_time () =
  let tm = Unix.localtime (Unix.time ()) in
  sprintf "%s %s %2d %02d:%02d:%02d %04d"
    days.(tm.Unix.tm_wday)
    months.(tm.Unix.tm_mon)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec
    (tm.Unix.tm_year + 1900)
    
let print_header () =
  printf "\nBigraphER: Bigraph Evaluator & Rewriting\n\
          ========================================\n\n\
          Version: %s\n\
          Date: %s\n\
          Hostname: %s\n\
          Operating system: %s\n\
          Command line: %s\n\n"
    version (format_time ()) (Unix.gethostname ()) Sys.os_type 
    (String.concat " " (Array.to_list Sys.argv))

let print_stats_store env stoch t0 =
  let t = (Unix.gettimeofday ()) -. t0 
  and ty = if stoch then "SBRS" else "BRS" in
  printf "Time for model construction: %-4g seconds.\n\n" t;
  printf "Type:     %s\n\
          Bindings: %d\n\n\
          --------------------------------------------------------------------------------\n\n%!" 
    ty (Hashtbl.length env)
let _ =
  try
    Export.check_graphviz ();
    Arg.parse (Arg.align speclist) (fun filename ->
      if Filename.check_suffix filename "big" then 
	model_str := filename
      else if Filename.check_suffix filename "bilog" then 
	properties_str := filename
      else raise (Arg.Bad ("Bad argument: " ^ filename))) usage;
    if !version_bool then print_version ()
    else begin 
      print_header ();
      let (decs, brs) =
	if !model_str = "" then 
	  raise (Arg.Bad ("Error: Model file missing.\n" ^ usage))
	else begin
	  printf "Parsing model file \"%s\"...\n\n" !model_str; 
	  let lexbuf, file = open_lex !model_str in
	  let out = Parser_main.model Lexer_main.lex lexbuf in 
	  close_in file; 
	  out
	end in
      let t0 = Unix.gettimeofday () in
      printf "--------------------------------------------------------------------------------\n\n%!\
              Building model...\n\n";
      let env = Store.init_env decs in
      Store.parse_consts !consts_str env;
      Store.store_decs decs env;
      let (s0, stoch, p_classes) =
	Store.store_brs brs env in
      if !export_store <> "" then
	Store.export decs env !export_store !verbose_bool;
      print_stats_store env stoch t0;
      (*(* Debug *)
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
      (****************************************************************)*)
      let p = Store.dummy_pos  
      and n = Hashtbl.length env in
      if not stoch then begin
	let brs_p_classes = List.map Store.p_to_brs p_classes 
	and get_f = Store.get_react p env in
	let (ts, stats) = 
	  if !sim_bool then
	    Brs.sim_ide s0 brs_p_classes get_f !s_max n !verbose_bool
	  else
	    Brs.bfs_ide s0 brs_p_classes get_f !s_max n !verbose_bool in
	printf "\n%s\n" (Sbrs.string_of_stats stats);
	if !export_trans_dot_str <> "" then
	  if !export_states_bool then
	    Brs.V.iter (fun (i, s) ->
	      Export.write_big s (string_of_int i) !export_trans_dot_str
		!verbose_bool) ts.Brs.v;
	Export.write_ts ts "ts" !export_trans_dot_str !verbose_bool;
	(* TODO *)
	if !export_trans_str <> "" then ();
	if !export_csl_str <> "" then ();
      end else begin
	let sbrs_p_classes = List.map Store.p_to_sbrs p_classes 
	and get_f = Store.get_sreact p env in
	let ctmc = 
	  if !sim_bool then
	    let (ctmc, stats) = 
	      Sbrs.sim_ide s0 sbrs_p_classes get_f !t_max n !verbose_bool in
	    printf "\n%s\n" (Sbrs.string_of_stats_sim stats);
	    ctmc
	  else
	    let (ctmc, stats) = 
	      Sbrs.bfs_ide s0 sbrs_p_classes get_f !s_max n !verbose_bool in
	    printf "\n%s\n" (Sbrs.string_of_stats stats);
	    ctmc in
	if !export_trans_dot_str <> "" then
	  if !export_states_bool then
	    Sbrs.V.iter (fun (i, s) ->
	      Export.write_big s (string_of_int i) !export_trans_dot_str
		!verbose_bool) ctmc.Sbrs.v;
	Export.write_ctmc ctmc "ctmc" !export_trans_dot_str !verbose_bool;
	(* TODO *)
	if !export_trans_str <> "" then ();
	if !export_csl_str <> "" then ();
      end;
      Export.wait_before_exit !verbose_bool
    end
  with
  | Arg.Bad m -> 
    prerr_endline m; exit 1 
  | Utils.PARSE_ERROR ->
    prerr_endline "Parsing unsuccesful"; exit 1
  | Parsing.Parse_error ->
    prerr_endline "Parsing unsuccesful"; exit 1
  | Store.INVALID_CONSTS | Store.INVALID_VAL | Store.WRONG_TYPE 
  | Store.NO_IDE | Store.INVALID_PRI -> 
    exit 1
  | e -> 
    prerr_endline (Printexc.to_string e); exit 1
  

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
    
