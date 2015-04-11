open Format
open Utils
open Ast
       
let print_header () =
  printf "\n\
          %s\n\
          ========================================\n\
          %s      %s\n\
          %s         %s\n\
          %s     %s\n\
          %s      %s\n\
          %s %s\n"
    (colorise `bold "BigraphER: Bigraph Evaluator & Rewriting")
    (colorise `magenta "Version:") (String.trim Version.version) 
    (colorise `magenta "Date:") (format_time ()) 
    (colorise `magenta "Hostname:") (Unix.gethostname ()) 
    (colorise `magenta "OS type:") Sys.os_type
    (colorise `magenta "Command line:")  (String.concat " " (Array.to_list Sys.argv))

let print_stats_store env stoch t0 =
  let t = (Unix.gettimeofday ()) -. t0 
  and ty = if stoch then "Stochastic BRS" else "BRS" in
  if Cmd.(defaults.debug) then () else 
    printf "%s   %-4gs\n" (colorise `cyan "Build time:") t;
  printf "%s         %s\n\
          %s     %d\n"
    (colorise `cyan "Type:") ty
    (colorise `cyan "Bindings:") (Hashtbl.length env)

let print_stats_brs stats lim =
  if Cmd.(defaults.debug) then () else printf " in %.3gs.\n" stats.Brs.t;
  printf
    "%s       %s\n\
     %s    %-8d\n\
     %s  %-8d\n"
    (colorise `green "States:") (let s = sprintf "%-8d" stats.Brs.s in 
                                 if lim then colorise `red s else s)
    (colorise `green "Reactions:") stats.Brs.r 
    (colorise `green "Occurrences:") stats.Brs.o   

let print_stats_sbrs stats t_lim s_lim =
  if Cmd.(defaults.debug) then () else printf " in %.3gs.\n" stats.Sbrs.t;
  if Cmd.(defaults.sim) then
    printf  
      "%s     %s\n"
      (colorise `green "Sim time:") (let s = sprintf "%.3g" stats.Sbrs.sim in 
                                     if t_lim then colorise `red s else s);
  printf
    "%s       %s\n\
     %s    %-8d\n\
     %s  %-8d\n"
    (colorise `green "States:") (let s = sprintf "%-8d" stats.Sbrs.s in 
                                 if s_lim then colorise `red s else s)
    (colorise `green "Reactions:") stats.Sbrs.r 
    (colorise `green "Occurrences:") stats.Sbrs.o

let print_loop v_flag i _ = 
  if Cmd.(defaults.debug) then () 
  else if v_flag then printf "\r%3d states found%!" (i + 1)
  else () 

(* Raise Sys_error if the file could not be opened *)
let open_lex path = 
  let file = open_in path in
  let lexbuf = Lexing.from_channel file in
  lexbuf.Lexing.lex_curr_p <-
    { Lexing.pos_fname = Filename.basename path;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0;
    };
  (lexbuf, file)          

let export_csl label =
  match Cmd.(defaults.out_csl) with
  | None -> ()
  | Some file -> (
      print_endline (
        (colorise `yellow "Exporting properties to ") ^ 
        (colorise `yellow (colorise `bold file)) ^ 
        (colorise `yellow " ...")
      );
      Export.write_csl label (Filename.basename file)
		       (Filename.dirname file) Cmd.(defaults.verbose)
    )

let export_ctmc_prism ctmc =
  match Cmd.(defaults.out_prism) with
  | None -> ()
  | Some file -> (
      print_endline (
        (colorise `yellow "Exporting CTMC in PRISM format to ") ^ 
        (colorise `yellow (colorise `bold file)) ^ 
        (colorise `yellow " ...")
      );
      Export.write_ctmc_prism ctmc (Filename.basename file)
			      (Filename.dirname file) Cmd.(defaults.verbose)
    )

let export_ts_prism ts =
  match Cmd.(defaults.out_prism) with
  | None -> ()
  | Some file -> (
      print_endline (
        (colorise `yellow "Exporting transition system in PRISM format to ") ^ 
        (colorise `yellow (colorise `bold file)) ^ 
        (colorise `yellow " ...")
      );
      Export.write_ts_prism ts (Filename.basename file)
			    (Filename.dirname file) Cmd.(defaults.verbose)
    )

let export_ctmc_states ctmc path =
  print_endline (
    (colorise `yellow "Exporting states to ") ^ 
    (colorise `yellow (colorise `bold path)) ^ 
    (colorise `yellow " ...")
  );
  Sbrs.iter_states (fun i s ->
      Export.write_big s ((string_of_int i) ^ ".svg") path Cmd.(defaults.verbose)
    ) ctmc

let export_ctmc_dot ctmc =
  match Cmd.(defaults.out_dot) with
  | None -> ()
  | Some file -> (
      print_endline (
        (colorise `yellow "Exporting CTMC to ") ^ 
        (colorise `yellow (colorise `bold file)) ^ 
        (colorise `yellow " ...")
      );
      Export.write_ctmc ctmc (Filename.basename file)
			(Filename.dirname file) Cmd.(defaults.verbose);
      if Cmd.(defaults.out_states) then 
        export_ctmc_states ctmc (Filename.dirname file);
    )

let export_ts_states ts path =
  print_endline (
    (colorise `yellow "Exporting states to ") ^ 
    (colorise `yellow (colorise `bold path)) ^ 
    (colorise `yellow " ...")
  );
  Brs.iter_states (fun i s ->
      Export.write_big s ((string_of_int i) ^ ".svg") path Cmd.(defaults.verbose)
    ) ts

let export_ts_dot ts =
  match Cmd.(defaults.out_dot) with
  | None -> ()
  | Some file -> (
      print_endline (
        (colorise `yellow "Exporting transition system to ") ^ 
        (colorise `yellow (colorise `bold file)) ^ 
        (colorise `yellow " ...")
      );
      Export.write_ts ts (Filename.basename file)
		      (Filename.dirname file) Cmd.(defaults.verbose);
      if Cmd.(defaults.out_states) then 
        export_ts_states ts (Filename.dirname file);
    )

let after_brs stats ts lim =
  print_stats_brs stats lim; 
  export_ts_dot ts;
  export_ts_prism ts;
  export_csl ts.Brs.l;
  exit 0

let after_sbrs stats ctmc t_lim s_lim =
  print_stats_sbrs stats t_lim s_lim; 
  export_ctmc_dot ctmc;
  export_ctmc_prism ctmc;
  export_csl ctmc.Sbrs.l;
  exit 0

let () =
  Printexc.record_backtrace true;
  try
    let iter_f = print_loop true in
    Cmd.parse Sys.argv;
    if Cmd.(defaults.debug) then () else print_header ();
    if Cmd.(defaults.debug) then ()
    else print_endline
	   ((colorise `yellow "Parsing model file ")
	    ^ (colorise `bold (colorise `yellow (Cmd.(to_string defaults.model))))
	    ^ (colorise `yellow " ...")); 
    let (lexbuf, file) = open_lex Cmd.(to_string defaults.model) in
    try
      let m = Parser.model Lexer.token lexbuf in 
      close_in file; 
      let fmt = Format.std_formatter in (* TEMPORARY *)
      let t0 = Unix.gettimeofday () in
      let env = Store.init_env fmt Cmd.(defaults.consts) in
      let (s0, prs, env_t) = Store.eval_model fmt m env in
      let n = Store.bindings env in
      let stoch =
	match prs with
	| Store.P _ -> false
	| Store.S _ -> true in
      (match Cmd.(defaults.out_store) with
       | None -> ()
       | Some path -> (
	 print_endline (
             (colorise `yellow "Exporting declarations to ") ^ 
	       (colorise `yellow (colorise `bold path)) ^ 
		 (colorise `yellow " ...")
           );
	 Store.export m.model_decs env env_t path Cmd.(defaults.verbose)));
      print_stats_store env stoch t0;
      match prs with
      | Store.P p_classes ->
	 (******** BRS *********)
	 (let (ts, stats) = 
            if Cmd.(defaults.sim) then
	      (print_endline
		 ((colorise `yellow "Starting simulation ...\n")
		  ^ (colorise `cyan "Max # states: ")
		  ^ (string_of_int Cmd.(defaults.s_max)));
	       Brs.sim s0 p_classes Cmd.(defaults.s_max) n iter_f)
	    else
	      (print_endline
		 ((colorise `yellow "Starting transition system construction ...\n")
		  ^ (colorise `cyan "Max # states: ")
		  ^ (string_of_int Cmd.(defaults.s_max)));
	       Brs.bfs s0 p_classes Cmd.(defaults.s_max) n iter_f) in
	  after_brs stats ts false)
      | Store.S p_classes ->
	 (******** SBRS *********)
	 (let (ctmc, stats) = 
            if Cmd.(defaults.sim) then
	      (print_endline
		 ((colorise `yellow "Starting stochastic simulation ...\n")
		  ^ (colorise `cyan "Max sim time: ")
		  ^ (string_of_float Cmd.(defaults.t_max)));
	       Sbrs.sim s0 p_classes Cmd.(defaults.t_max) n iter_f)
            else
	      (print_endline
		 ((colorise `yellow "Starting CTMC construction ...\n")
		  ^ (colorise `cyan "Max # states: ")
		  ^ (string_of_int Cmd.(defaults.s_max)));
	       Sbrs.bfs s0 p_classes Cmd.(defaults.s_max) n iter_f) in
          after_sbrs stats ctmc false false)
    with
    | Sbrs.LIMIT (ctmc, stats) ->
       (if Cmd.(defaults.sim) then after_sbrs stats ctmc true false
	else after_sbrs stats ctmc false true) 
    | Brs.LIMIT (ts, stats) -> after_brs stats ts true 
    | Store.ERROR (e, p) ->
       (Loc.print_loc Format.err_formatter p;
	Store.report_error Format.err_formatter e;
	exit 1)
    | Parser.Error ->
       (Loc.print_loc Format.err_formatter Loc.{lstart = Lexing.(lexbuf.lex_curr_p);
						lend = Lexing.(lexbuf.lex_start_p)};
	prerr_endline ((colorise `red "Error: ") ^ "Syntax error near token \""
		       ^ (Lexing.lexeme lexbuf) ^ "\""); 
	exit 1)
  with
  | Lexer.ERROR (e,p) ->
     (Loc.print_loc Format.err_formatter p;
      Lexer.report_error Format.err_formatter e;
      exit 1)
  | Sys_error s ->
     (prerr_endline ((colorise `red "Error: ") ^ s); 
      exit 1)
  | e -> 
     eprintf "%s\n%s" (Printexc.to_string e) (Printexc.get_backtrace ()); exit 1

  (* | Link.FACES_MISMATCH (inner, outer) -> *)
  (*   eprintf "%sImpossible to compose over faces:\n\ *)
  (*           \       %s\n\ *)
  (*           \       %s\n"  *)
  (*     (colorise `red "Error: ") *)
  (*     (Link.string_of_face inner) (Link.string_of_face outer);  *)
  (*   exit 1 *)
  (* | Link.NAMES_ALREADY_DEFINED (inner, outer) -> *)
  (*   eprintf "%sDuplicate inner names: %s\n\ *)
  (*           \       Duplicate outer names: %s\n" *)
  (*     (colorise `red "Error: ") *)
  (*     (Link.string_of_face inner) (Link.string_of_face outer);  *)
  (*   exit 1 *)
  (* | Place.COMP_ERROR (sites, roots) -> *)
  (*   eprintf "%sImpossible to compose with %d sites and %d regions\n" *)
  (*     (colorise `red "Error: ") *)
  (*     sites roots; *)
  (*   exit 1 *)
  (* | Big.SHARING_ERROR ->  *)
  (*   eprintf "%sInvalid sharing expression\n"     *)
  (*     (colorise `red "Error: ");  *)
  (*   exit 1 *)
  (* | Big.CTRL_ERROR (n, face) -> *)
  (*   eprintf "%sImpossible to use face %s with a control of arity %d\n"  *)
  (*     (colorise `red "Error: ") *)
  (*     (Link.string_of_face face) n; *)
  (*   exit 1 *)
  (* | Big.ISO_ERROR (n, n_dom) -> *)
  (*   eprintf "%sIsomorphism is not total: (%d < %d)\n" *)
  (*     (colorise `red "Error: ") *)
  (*     n_dom n; *)
  (*   exit 1 *)
