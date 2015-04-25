open Format
open Ast

(******** PRETTY PRINTING FUNCTIONS *********)

let max_width = 50
       
type val_type = [ `s of string | `i of int | `f of float ]
       
type row =
  { descr:   string * Utils.text_style;
    value:   val_type;
    pp_val:  formatter -> val_type -> unit;
    display: bool; }
       
let print_msg fmt c msg =
  if not Cmd.(defaults.debug) then
    fprintf fmt "@[%s@]@," (Utils.colorise c msg)
  else ()
	 
let print_descr fmt (d, c) =
  fprintf fmt "%s" (Utils.colorise c d)

let print_float fmt = function
  | `f f  -> fprintf fmt "@[<h>%-3gs@]" f
  | `i _
  | `s _ -> assert false
		   
let print_string fmt = function
  | `s s -> fprintf fmt "@[<h>%s@]" s
  | `i _
  | `f _ -> assert false
		   
let print_int fmt = function
  | `i i -> fprintf fmt "@[<h>%-8d@]" i
  | `f _
  | `s _ -> assert false

let print_table fmt (rows : row list) =
  let pp_row fmt r =
    pp_print_tab fmt ();
    fprintf fmt "%a" print_descr r.descr;
    pp_print_tab fmt ();
    fprintf fmt "%a" r.pp_val r.value in
  match List.filter (fun r -> r.display) rows with
  |  r :: rows ->
      (pp_open_tbox fmt ();
       (* First row *)
       pp_set_tab fmt ();
       fprintf fmt "@[<h>%s" (Utils.colorise (snd r.descr) (fst r.descr));
       print_break (15 - (String.length (fst r.descr))) 0;
       fprintf fmt "@]";
       pp_set_tab fmt ();
       fprintf fmt "%a" r.pp_val r.value; 
       List.iter (pp_row fmt) rows;
       pp_close_tbox fmt ();
       print_cut ())
  | _ -> assert false
		   
let print_header fmt () =
  if not Cmd.(defaults.debug) then
  (fprintf fmt "@[<v>@,%s@,%s@,"
	  (Utils.colorise `bold "BigraphER: Bigraph Evaluator & Rewriting")
	  "========================================";
  [{ descr = ("Version:", `magenta);
     value = `s (String.trim Version.version);
     pp_val = print_string;
     display = true; };
   { descr = ("Date:", `magenta);
     value = `s (Utils.format_time ());
     pp_val = print_string;
     display = true; };
   { descr = ("Hostname:", `magenta);
     value = `s (Unix.gethostname ());
     pp_val = print_string;
     display = true; };
   { descr = ("OS type:", `magenta);
     value = `s Sys.os_type;
     pp_val = print_string;
     display = true; };
   { descr = ("Command line:", `magenta);
     value = `s (String.concat " " (Array.to_list Sys.argv));
     pp_val = print_string;
     display = true; }]
  |> print_table fmt)
  else fprintf fmt "@[<v>"
	    
let print_stats_store fmt env stoch =
  let ty = if stoch then "Stochastic BRS" else "BRS" in
  [{ descr = ("Type:", `cyan);
     value = `s ty;
     pp_val = print_string;
     display = true; };
   { descr = ("Bindings:", `cyan);
     value = `i (Hashtbl.length env);
     pp_val = print_int;
     display = true; }]
  |> print_table fmt

let print_max fmt =
  [{ descr = ("Max # states:", `cyan);
     value = `i Cmd.(defaults.s_max);
     pp_val = print_int;
     display = true; }]
  |> print_table fmt;
  print_flush ();
  if Cmd.(defaults.debug) then () 
  else Pervasives.print_string "\n["
		   
let print_stats_brs fmt stats =
  [{ descr = ("Build time:", `green);
     value = `f Brs.(stats.t);
     pp_val = print_float;
     display =  not Cmd.(defaults.debug); };
   { descr = ("States:", `green);
     value = `i Brs.(stats.s);
     pp_val = print_int;
     display = true; };
   { descr = ("Transitions:", `green);
     value = `i Brs.(stats.r);
     pp_val = print_int;
     display = true; };
   { descr = ("Occurrences:", `green);
     value = `i Brs.(stats.o);
     pp_val = print_int;
     display = true; }]
  |> print_table fmt

let print_stats_sbrs fmt stats =
  [{ descr = ("Build time:", `green);
     value = `f Sbrs.(stats.t);
     pp_val = print_float;
     display =  not Cmd.(defaults.debug); };
   { descr = ("Sim time:", `green);
     value = `f Sbrs.(stats.sim);
     pp_val = print_float;
     display =  Cmd.(defaults.sim); };
   { descr = ("States:", `green);
     value = `i Sbrs.(stats.s);
     pp_val = print_int;
     display = true; };
   { descr = ("Transitions:", `green);
     value = `i Sbrs.(stats.r);
     pp_val = print_int;
     display = true; };
   { descr = ("Occurrences:", `green);
     value = `i Sbrs.(stats.o);
     pp_val = print_int;
     display = true; }]
  |> print_table fmt
		 
let print_loop i _ = 
  if Cmd.(defaults.debug) then () 
  else (let m =
	  if Cmd.(defaults.s_max) >= 1000 then
	    Cmd.(defaults.s_max) / 1000
	  else 1 in
	match (i + 1) mod (max_width * m) with
	| 0 -> (Pervasives.print_char '.';
				 Pervasives.print_newline ();
				 Pervasives.print_char ' ';
				 Pervasives.flush stdout)
	| i when i mod m = 0 -> (Pervasives.print_char '.';
				 Pervasives.flush stdout)
	| _ -> ())

(******** EXPORT FUNCTIONS *********)

let print_fun fmt c verb fname i =
  if verb then
    print_msg fmt c ((string_of_int i) ^ " bytes written to `" ^ fname ^ "'") 
  else ()
		      
let export_csl fmt label =
  match Cmd.(defaults.out_csl) with
  | None -> ()
  | Some file ->
     (print_msg fmt `yellow ("Exporting properties to " ^ file ^ " ...");
      Export.write_csl label (Filename.basename file) (Filename.dirname file)
     |> print_fun fmt `white Cmd.(defaults.verbose) file)

let export_ctmc_prism fmt ctmc =
  match Cmd.(defaults.out_prism) with
  | None -> ()
  | Some file ->
     (print_msg fmt `yellow ("Exporting CTMC in PRISM format to " ^ file ^ " ...");
      Export.write_ctmc_prism ctmc (Filename.basename file) (Filename.dirname file)
     |> print_fun fmt `white Cmd.(defaults.verbose) file)

let export_ts_prism fmt ts =
  match Cmd.(defaults.out_prism) with
  | None -> ()
  | Some file ->
     (print_msg fmt `yellow ("Exporting transition system in PRISM format to "
		     ^ file ^ " ...");
      Export.write_ts_prism ts (Filename.basename file) (Filename.dirname file)
     |> print_fun fmt `white Cmd.(defaults.verbose) file)

let export_ctmc_states fmt ctmc path =
  print_msg fmt `yellow ("Exporting states to " ^ path ^ " ...");
  Sbrs.iter_states (fun i s ->
		    let fname = (string_of_int i) ^ ".svg" in
		    Export.write_big s fname path
		    |> print_fun fmt `white Cmd.(defaults.verbose) (Filename.concat path fname))
		   ctmc

let export_ctmc_dot fmt ctmc =
  match Cmd.(defaults.out_dot) with
  | None -> ()
  | Some file ->
     (print_msg fmt `yellow ("Exporting CTMC to " ^ file ^ " ...");
      Export.write_ctmc ctmc (Filename.basename file) (Filename.dirname file)
      |> print_fun fmt `white Cmd.(defaults.verbose) file;
      if Cmd.(defaults.out_states) then 
        export_ctmc_states fmt ctmc (Filename.dirname file))

let export_ts_states fmt ts path =
  print_msg fmt `yellow ("Exporting states to " ^ path ^ " ...");
  Brs.iter_states (fun i s ->
		   let fname = (string_of_int i) ^ ".svg" in
		   Export.write_big s fname path
		   |> print_fun fmt `white Cmd.(defaults.verbose) (Filename.concat path fname))
		  ts

let export_ts_dot fmt ts =
  match Cmd.(defaults.out_dot) with
  | None -> ()
  | Some file ->
     (print_msg fmt `yellow ("Exporting transition system to " ^ file ^ " ...");
      Export.write_ts ts (Filename.basename file) (Filename.dirname file)
      |> print_fun fmt `white Cmd.(defaults.verbose) file;
      if Cmd.(defaults.out_states) then 
        export_ts_states fmt ts (Filename.dirname file))

let after_brs_aux fmt stats ts =
  print_stats_brs fmt stats;
  fprintf fmt "@]@?";
  fprintf fmt "@[<v>";
  export_ts_dot fmt ts;
  export_ts_prism fmt ts;
  export_csl fmt ts.Brs.l;
  fprintf fmt "@]@?";
  fprintf err_formatter "@]@?";
  exit 0

let close_progress_bar () =
  Pervasives.print_char ']';
  Pervasives.print_newline ();
  Pervasives.print_newline ()
  
let after_brs fmt stats ts =
  if Cmd.(defaults.debug) then ()
  else close_progress_bar ();
  fprintf fmt "@[<v>";
  after_brs_aux fmt stats ts

let after_sbrs_aux fmt stats ctmc =
  print_stats_sbrs fmt stats;
  fprintf fmt "@]@?";
  fprintf fmt "@[<v>";
  export_ctmc_dot fmt ctmc;
  export_ctmc_prism fmt ctmc;
  export_csl fmt ctmc.Sbrs.l;
  fprintf fmt "@]@?";
  fprintf err_formatter "@]@?";
  exit 0
		
let after_sbrs fmt stats ctmc =
  if Cmd.(defaults.debug) then ()
  else close_progress_bar ();
  fprintf fmt "@[<v>";
  after_sbrs_aux fmt stats ctmc
		 
(******** BIGRAPHER *********)
       
let open_lex path = 
  let file = open_in path in
  let lexbuf = Lexing.from_channel file in
  lexbuf.Lexing.lex_curr_p <-
    { Lexing.pos_fname = Filename.basename path;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0; };
  (lexbuf, file)          
       
let () =
  Printexc.record_backtrace true;
  let fmt = std_formatter in (* TEMPORARY *)
  fprintf err_formatter "@[<v>";
  try
    let iter_f = print_loop in
    Cmd.parse Sys.argv;
    print_header fmt ();
    print_msg fmt `yellow ("Parsing model file "
		   ^ (Cmd.(to_string defaults.model))
		   ^ " ..."); 
    let (lexbuf, file) = open_lex Cmd.(to_string defaults.model) in
    try
      let m = Parser.model Lexer.token lexbuf in 
      close_in file; 
      let env = Store.init_env fmt Cmd.(defaults.consts) in
      let (s0, prs, env_t) = Store.eval_model fmt m env in
      let n = Store.bindings env in
      let stoch =
	match prs with
	| Store.P _ -> false
	| Store.S _ -> true in
      (match Cmd.(defaults.out_store) with
       | None -> ()
       | Some path ->
	  (print_msg fmt `yellow ("Exporting declarations to "
			  ^ path ^ " ...");
	   Store.export m.model_decs env env_t path
			(print_fun fmt `white Cmd.(defaults.verbose))));
      print_stats_store fmt env stoch;
      match prs with
      | Store.P p_classes ->
	 (******** BRS *********)
	 (let (ts, stats) =
            if Cmd.(defaults.sim) then
	      (print_msg fmt `yellow "Starting simulation ...";
	       print_max fmt;
	       Brs.sim s0 p_classes Cmd.(defaults.s_max) n iter_f)
	    else
	      (print_msg fmt `yellow "Computing transition system ...";
	       print_max fmt;
	       Brs.bfs s0 p_classes Cmd.(defaults.s_max) n iter_f) in
	  after_brs fmt stats ts)
      | Store.S p_classes ->
	 (******** SBRS *********)
	  (let (ctmc, stats) = 
            if Cmd.(defaults.sim) then
	      (print_msg fmt `yellow "Starting stochastic simulation ...";
	       [{ descr = ("Max sim time:", `cyan);
		  value = `f Cmd.(defaults.t_max);
		  pp_val = print_float;
		  display = true; }]
	       |> print_table fmt;
	       print_flush ();
	       if Cmd.(defaults.debug) then ()
	       else Pervasives.print_string "\n[";
	       Sbrs.sim s0 p_classes Cmd.(defaults.t_max) n iter_f)
            else
	      (print_msg fmt `yellow "Computing CTMC ...";
	       print_max fmt;
	       Sbrs.bfs s0 p_classes Cmd.(defaults.s_max) n iter_f) in
	  after_sbrs fmt stats ctmc)
    with
    | Sbrs.LIMIT (ctmc, stats) ->
       (  if Cmd.(defaults.debug) then () else close_progress_bar ();
	  fprintf fmt "@[<v>";
	  print_msg fmt `yellow "Maximum number of states reached.";
	  after_sbrs_aux fmt stats ctmc) 
    | Brs.LIMIT (ts, stats) ->
       (  if Cmd.(defaults.debug) then () else close_progress_bar ();
	  fprintf fmt "@[<v>";
	  print_msg fmt `yellow "Maximum number of states reached.";
	  after_brs_aux fmt stats ts)
    | Export.ERROR e ->
       (fprintf fmt "@]@?";
	Export.report_error e
	|> fprintf err_formatter "@[%s: %s@]@," Utils.err;
	fprintf err_formatter "@]@?";
	exit 1)
    | Store.ERROR (e, p) ->
       (fprintf fmt "@]@?";
	Loc.print_loc err_formatter p;
	Store.report_error err_formatter e;
	fprintf err_formatter "@]@?";
	exit 1)
    | Parser.Error ->
       (fprintf fmt "@]@?";
	Loc.print_loc err_formatter Loc.{lstart = Lexing.(lexbuf.lex_start_p);
					 lend = Lexing.(lexbuf.lex_curr_p)};
	fprintf err_formatter "@[%s: Syntax error near token `%s'@]@,"
		Utils.err (Lexing.lexeme lexbuf);
	fprintf err_formatter "@]@?";
	exit 1)
  with
  | Lexer.ERROR (e, p) ->
     (fprintf fmt "@]@?";
      Loc.print_loc err_formatter p;
      Lexer.report_error err_formatter e;
      fprintf err_formatter "@]@?";
      exit 1)
  | Sys_error s ->
     (fprintf fmt "@]@?";
      fprintf err_formatter "@[%s: %s@]@," Utils.err s;
      fprintf err_formatter "@]@?";
      exit 1)
  | e -> 
     (fprintf fmt "@]@?";
      fprintf err_formatter "@[%s@,%s@]@," (Printexc.to_string e) (Printexc.get_backtrace ());
      fprintf err_formatter "@]@?";
      exit 1)
