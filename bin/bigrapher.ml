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
    fprintf fmt "@?@[%s@]@." (Utils.colorise c msg)
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
       Format.print_newline ())
  | _ -> assert false
		   
let print_header fmt () =
  if not Cmd.(defaults.debug) then
  (fprintf fmt "@[<v>@,%s@,%s@,"
	  (Utils.colorise `bold "BigraphER: Bigraph Evaluator & Rewriting")
	  "========================================";
  [{ descr = ("Version:", `blue);
     value = `s (String.trim Version.version);
     pp_val = print_string;
     display = true; };
   { descr = ("Date:", `blue);
     value = `s (Utils.format_time ());
     pp_val = print_string;
     display = true; };
   { descr = ("Hostname:", `blue);
     value = `s (Unix.gethostname ());
     pp_val = print_string;
     display = true; };
   { descr = ("OS type:", `blue);
     value = `s Sys.os_type;
     pp_val = print_string;
     display = true; };
   { descr = ("Command line:", `blue);
     value = `s (String.concat " " (Array.to_list Sys.argv));
     pp_val = print_string;
     display = true; }]
  |> print_table fmt)
  else ()
	    
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
     value = `i Cmd.(defaults.max_states);
     pp_val = print_int;
     display = true; }]
  |> print_table fmt;
  if Cmd.(defaults.debug) then () 
  else Pervasives.print_string "\n["
		   
let print_stats fmt t s r o =
  [{ descr = ("Build time:", `green);
     value = `f t;
     pp_val = print_float;
     display =  not Cmd.(defaults.debug); };
   { descr = ("States:", `green);
     value = `i s;
     pp_val = print_int;
     display = true; };
   { descr = ("Transitions:", `green);
     value = `i r;
     pp_val = print_int;
     display = true; };
   { descr = ("Occurrences:", `green);
     value = `i o;
     pp_val = print_int;
     display = true; }]
  |> print_table fmt
		 
let print_loop i _ = 
  if Cmd.(defaults.debug) then () 
  else (let m =
	  if Cmd.(defaults.max_states) >= 1000 then
	    Cmd.(defaults.max_states) / 1000
	  else 1 in
	match (i + 1) mod (max_width * m) with
	| 0 -> (Pervasives.print_char '.';
		Pervasives.print_string "  ";
		Pervasives.print_int (i + 1);
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
	 
let export_prism fmt msg f =
  match Cmd.(defaults.export_prism) with
  | None -> ()
  | Some file ->
     (print_msg fmt `yellow (msg ^ file ^ " ...");
      try
	f ~name:(Filename.basename file)
	  ~path:(Filename.dirname file)
	|> print_fun fmt `white Cmd.(defaults.verb) file
      with
      | Export.ERROR e ->
	 (pp_print_flush fmt ();
	  fprintf err_formatter "@[<v>";
	  Export.report_error e
	  |> fprintf err_formatter "@[%s: %s@]@." Utils.err))
	 
let export_ctmc_prism fmt ctmc =
  export_prism fmt
	       "Exporting CTMC in PRISM format to "
	       (Sbrs.write_prism ctmc)

let export_ts_prism fmt ts =
  export_prism fmt
	       "Exporting transition system in PRISM format to "
	       (Brs.write_prism ts)

let export_csl fmt f =
  match Cmd.(defaults.export_lab) with
  | None -> ()
  | Some file ->
     (print_msg fmt `yellow ("Exporting properties to " ^ file ^ " ...");
      try
	f ~name:(Filename.basename file) ~path:(Filename.dirname file)
	|> print_fun fmt `white Cmd.(defaults.verb) file
      with
      | Export.ERROR e ->
	 (pp_print_flush fmt ();
	  fprintf err_formatter "@[<v>";
	  Export.report_error e
	  |> fprintf err_formatter "@[%s: %s@]@." Utils.err))

let export_ctmc_csl fmt ctmc =
  export_csl fmt (Sbrs.write_lab ctmc)
	     
let export_ts_csl fmt ts =
  export_csl fmt (Brs.write_lab ts)

let export_states fmt f path =
  print_msg fmt `yellow ("Exporting states to " ^ path ^ " ...");
    f ~f:(fun i s ->
	  let fname = (string_of_int i) ^ ".svg" in
	  try
	    Big.write_svg s ~name:fname ~path
	    |> print_fun fmt
			 `white
			 Cmd.(defaults.verb)
			 (Filename.concat path fname)
	  with
	  | Export.ERROR e ->
	     (pp_print_flush fmt ();
	      fprintf err_formatter "@[<v>";
	      Export.report_error e
	      |> fprintf err_formatter "@[%s: %s@]@." Utils.err))
	     
let export_ctmc_states ctmc fmt path =
  export_states fmt Sbrs.iter_states path ctmc

let export_ts_states ts fmt path =
  export_states fmt Brs.iter_states path ts
		
let export_ts fmt msg f f_iter =
  match Cmd.(defaults.export_graph) with
  | None -> ()
  | Some file ->
     (print_msg fmt `yellow (msg ^ file ^ " ...");
      (try
	  f ~name:(Filename.basename file)
	    ~path:(Filename.dirname file)
	  |> print_fun fmt `white Cmd.(defaults.verb) file
	with
	| Export.ERROR e ->
	   (pp_print_flush fmt ();
	    fprintf err_formatter "@[<v>";
	    Export.report_error e
	    |> fprintf err_formatter "@[%s: %s@]@." Utils.err)))
       
let close_progress_bar () =
  Pervasives.print_char ']';
  Pervasives.print_newline ();
  Pervasives.print_newline ()
			   
let after_brs_aux fmt stats ts =
  print_stats fmt
	      stats.Brs.time
	      stats.Brs.states
	      stats.Brs.trans
	      stats.Brs.occs;
  export_ts fmt
	    "Exporting transition system to "
	    (Brs.write_svg ts)
	    (export_ts_states ts);
  export_ts_prism fmt ts;
  export_ts_csl fmt ts;
  pp_print_flush err_formatter ();
  exit 0

let after_brs fmt (ts,stats) =
  if Cmd.(defaults.debug) then ()
  else close_progress_bar ();
  after_brs_aux fmt stats ts

let after_sbrs_aux fmt stats ctmc =
  print_stats fmt
	      stats.Sbrs.time
	      stats.Sbrs.states
	      stats.Sbrs.trans
	      stats.Sbrs.occs;
  export_ts fmt
	    "Exporting CTMC to "
	    (Sbrs.write_svg ctmc)
	    (export_ctmc_states ctmc);
  export_ctmc_prism fmt ctmc;
  export_ctmc_csl fmt ctmc;
  pp_print_flush err_formatter ();
  exit 0

let after_sbrs fmt (ctmc, stats) =
  if Cmd.(defaults.debug) then ()
  else close_progress_bar ();
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
    		     		   		 
let parse_cmd fmt argv =
  let lexbuf =
    Array.to_list argv
    |> List.tl
    |> String.concat " " 
    |> Lexing.from_string in
  (* Check environment variables *)
  Cmd.eval_env ();
  try
    match Parser.cmd Lexer.cmd lexbuf with
    | Cmd.StandAloneOpt Cmd.Config ->
       Cmd.eval_config std_formatter ()
    | Cmd.StandAloneOpt Cmd.Help_top_level ->
       Cmd.eval_help_top std_formatter ()
    | Cmd.StandAloneOpt Cmd.Version ->
       Cmd.eval_version std_formatter ()
    | Cmd.Check (options, model, pred) ->
       (Cmd.eval_chk fmt options model pred;
	`check) 
    | Cmd.Full (options, model, pred) ->
       (Cmd.eval_full fmt options model pred;
	`full) 
    | Cmd.Sim (options, model, pred) ->
       (Cmd.eval_sim fmt options model pred;
	`sim) 
  with
  | Cmd.ERROR e ->
     (pp_print_flush fmt ();
      Cmd.report_error err_formatter e;
      exit 1)
  | Parser.Error ->
     (pp_print_flush fmt ();
      Cmd.report_error err_formatter (Cmd.Parse (Lexing.lexeme lexbuf));
      Cmd.usage err_formatter ();
      exit 1)
  | Lexer.ERROR (e, p) ->
     (pp_print_flush fmt ();
      fprintf err_formatter "@[<v>";
      Lexer.report_error err_formatter e;
      Cmd.usage err_formatter ();
      exit 1)
       
let () =
  Printexc.record_backtrace true; (* Disable for releases *)
  let fmt = std_formatter in (* TEMPORARY *)
  try
    let iter_f = print_loop in
    let exec_type = parse_cmd fmt Sys.argv in
    print_header fmt ();
    print_msg fmt `yellow ("Parsing model file "
		   ^ Cmd.(defaults.model)
		   ^ " ..."); 
    let (lexbuf, file) = open_lex Cmd.(defaults.model) in
    try
      let m = Parser.model Lexer.token lexbuf in 
      close_in file; 
      let env = Store.init_env fmt Cmd.(defaults.consts) in
      let (s0, prs, env_t) = Store.eval_model fmt m env in
      let stoch =
	match prs with
	| Store.P _ -> false
	| Store.S _ -> true in
      (match Cmd.(defaults.export_decs) with
       | None -> ()
       | Some path ->
	  (print_msg fmt `yellow ("Exporting declarations to "
			  ^ path ^ " ...");
	   Store.export m.model_decs env env_t path
			(print_fun fmt `white Cmd.(defaults.verb))));
      print_stats_store fmt env stoch;
      match prs with
      | Store.P priorities ->
	 (******** BRS *********)
	 (match exec_type with
	  | `sim ->
	     (print_msg fmt `yellow "Starting simulation ...";
	      print_max fmt;
	      Brs.sim ~s0
		      ~priorities
		      ~init_size:Cmd.(defaults.max_states)
		      ~stop:Cmd.(defaults.steps)
		      ~iter_f
	      |> after_brs fmt)
	  | `full ->
	     (print_msg fmt `yellow "Computing transition system ...";
	      print_max fmt;
	      Brs.bfs ~s0
		      ~priorities
		      ~max:Cmd.(defaults.max_states)
		      ~iter_f
	      |> after_brs fmt)
	  | `check -> failwith "TO DO")
      | Store.S priorities ->
	 (******** SBRS *********)
	 (match exec_type with
	  | `sim ->
	     (print_msg fmt `yellow "Starting stochastic simulation ...";
	      [{ descr = ("Max sim time:", `cyan);
		 value = `f Cmd.(defaults.time);
		 pp_val = print_float;
		 display = true; }]
	      |> print_table fmt;
	      if Cmd.(defaults.debug) then ()
	      else Pervasives.print_string "\n[";
	      Sbrs.sim ~s0
		       ~priorities
		       ~init_size:Cmd.(defaults.max_states)
		       ~stop:Cmd.(defaults.time)
		       ~iter_f
	      |> after_sbrs fmt)
          | `full ->
	     (print_msg fmt `yellow "Computing CTMC ...";
	      print_max fmt;
	      Sbrs.bfs ~s0
		       ~priorities
		       ~max:Cmd.(defaults.max_states)
		       ~iter_f
	      |>  after_sbrs fmt)
	  | `check -> failwith "TO DO")
    with
    | Sbrs.MAX (ctmc, stats)
    | Sbrs.LIMIT (ctmc, stats) ->
       (if Cmd.(defaults.debug) then () else close_progress_bar ();
	print_msg fmt `yellow "Maximum number of states reached.";
	after_sbrs_aux fmt stats ctmc)
    | Sbrs.DEADLOCK (ctmc, stats, t) ->
       (if Cmd.(defaults.debug) then () else close_progress_bar ();
	print_msg fmt `yellow ("Deadlock state reached at time " ^ (string_of_float t) ^ ".");
	after_sbrs_aux fmt stats ctmc)
    | Brs.MAX (ts, stats)
    | Brs.LIMIT (ts, stats) ->
       (if Cmd.(defaults.debug) then () else close_progress_bar ();
	print_msg fmt `yellow "Maximum number of states reached.";
	after_brs_aux fmt stats ts)
    | Brs.DEADLOCK (ts, stats, t) ->
       (if Cmd.(defaults.debug) then () else close_progress_bar ();
	print_msg fmt `yellow ("Deadlock state reached at step " ^ (string_of_int t) ^ ".");
	after_brs_aux fmt stats ts)
    | Store.ERROR (e, p) ->
       (pp_print_flush fmt ();
	fprintf err_formatter "@[<v>";
	Loc.print_loc err_formatter p;
	Store.report_error err_formatter e;
	pp_print_flush err_formatter ();
	exit 1)
    | Parser.Error ->
       (pp_print_flush fmt ();
	fprintf err_formatter "@[<v>";
	Loc.print_loc err_formatter
		      Loc.{lstart = Lexing.(lexbuf.lex_start_p);
			   lend = Lexing.(lexbuf.lex_curr_p)};
	fprintf err_formatter "@[%s: Syntax error near token `%s'@]@."
		Utils.err (Lexing.lexeme lexbuf);
	exit 1)
  with
  | Lexer.ERROR (e, p) ->
     (pp_print_flush fmt ();
      fprintf err_formatter "@[<v>";
      Loc.print_loc err_formatter p;
      Lexer.report_error err_formatter e;
      pp_print_flush err_formatter ();
      exit 1)
  | Sys_error s ->
     (pp_print_flush fmt ();
      fprintf err_formatter "@[%s: %s@]@." Utils.err s;
      exit 1)
  | e -> 
     (pp_print_flush fmt ();
      fprintf err_formatter "@[%s@,%s@]@."
	      (Printexc.to_string e) (Printexc.get_backtrace ());
      exit 1)
