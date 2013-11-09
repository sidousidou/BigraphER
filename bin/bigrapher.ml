open Printf
open Utils
open Syntax
open Filename
open Cmd

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
    (colorise `magenta "Version:") (String.trim version) 
    (colorise `magenta "Date:") (format_time ()) 
    (colorise `magenta "Hostname:") (Unix.gethostname ()) 
    (colorise `magenta "OS type:") Sys.os_type
    (colorise `magenta "Command line:")  (String.concat " " (Array.to_list Sys.argv))

let print_stats_store env stoch t0 =
  let t = (Unix.gettimeofday ()) -. t0 
  and ty = if stoch then "Stochastic BRS" else "BRS" in
  printf "%s   %-4gs\n\
          %s         %s\n\
          %s     %d\n"
    (colorise `cyan "Build time:") t
    (colorise `cyan "Type:") ty
    (colorise `cyan "Bindings:") (Hashtbl.length env)

let print_stats_brs stats lim =
  printf " in %.3gs.\n" stats.Brs.t;
  printf
    "%s       %s\n\
     %s    %-8d\n\
     %s  %-8d\n"
    (colorise `green "States:") (let s = sprintf "%-8d" stats.Brs.s in 
                                 if lim then colorise `red s else s)
    (colorise `green "Reactions:") stats.Brs.r 
    (colorise `green "Occurrences:") stats.Brs.o   

let print_stats_sbrs stats t_lim s_lim =
  printf " in %.3gs.\n" stats.Sbrs.t;
  if defaults.sim then
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
  if v_flag then printf "\r%3d states found%!" (i + 1)
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
 match defaults.out_csl with
   | None -> ()
   | Some file -> (
       print_endline (
         (colorise `yellow "Exporting properties to ") ^ 
           (colorise `yellow (colorise `bold file)) ^ 
           (colorise `yellow " ...")
       );
       Export.write_csl label (basename file) (dirname file) defaults.verbose
     )

let export_ctmc_prism ctmc =
  match defaults.out_prism with
  | None -> ()
  | Some file -> (
      print_endline (
        (colorise `yellow "Exporting CTMC in PRISM format to ") ^ 
          (colorise `yellow (colorise `bold file)) ^ 
          (colorise `yellow " ...")
      );
      Export.write_ctmc_prism ctmc (basename file) (dirname file) defaults.verbose
  )

let export_ts_prism ts =
  match defaults.out_prism with
  | None -> ()
  | Some file -> (
      print_endline (
        (colorise `yellow "Exporting transition system in PRISM format to ") ^ 
          (colorise `yellow (colorise `bold file)) ^ 
          (colorise `yellow " ...")
      );
      Export.write_ts_prism ts (basename file) (dirname file) defaults.verbose
    )

let export_ctmc_states ctmc path =
  print_endline (
    (colorise `yellow "Exporting states to ") ^ 
      (colorise `yellow (colorise `bold path)) ^ 
      (colorise `yellow " ...")
  );
  Sbrs.iter_states (fun i s ->
      Export.write_big s (string_of_int i) path defaults.verbose
    ) ctmc
    
let export_ctmc_dot ctmc =
  match defaults.out_dot with
  | None -> ()
  | Some file -> (
      print_endline (
        (colorise `yellow "Exporting CTMC to ") ^ 
          (colorise `yellow (colorise `bold file)) ^ 
          (colorise `yellow " ...")
      );
      Export.write_ctmc ctmc (basename file) (dirname file) defaults.verbose;
      if defaults.out_states then 
        export_ctmc_states ctmc (dirname file);
    )
    
let export_ts_states ts path =
  print_endline (
    (colorise `yellow "Exporting states to ") ^ 
      (colorise `yellow (colorise `bold path)) ^ 
      (colorise `yellow " ...")
  );
  Brs.iter_states (fun i s ->
      Export.write_big s (string_of_int i) path defaults.verbose
    ) ts

let export_ts_dot ts =
  match defaults.out_dot with
  | None -> ()
  | Some file -> (
      print_endline (
        (colorise `yellow "Exporting transition system to ") ^ 
          (colorise `yellow (colorise `bold file)) ^ 
          (colorise `yellow " ...")
      );
      Export.write_ts ts (basename file) (dirname file) defaults.verbose;
      if defaults.out_states then 
        export_ts_states ts (dirname file);
    )

let after_brs stats ts lim =
  print_stats_brs stats lim; 
  export_ts_dot ts;
  export_ts_prism ts;
  export_csl ts.Brs.l

let after_sbrs stats ctmc t_lim s_lim =
  print_stats_sbrs stats t_lim s_lim; 
  export_ctmc_dot ctmc;
  export_ctmc_prism ctmc;
  export_csl ctmc.Sbrs.l

let _ =
  try
    let iter_f = print_loop true in
    parse Sys.argv;
    print_header ();
    let (decs, brs) =
      print_endline (
        (colorise `yellow "Parsing model file ") ^ 
          (colorise `bold (colorise `yellow (_to_string defaults.model))) ^
          (colorise `yellow " ...")
      ); 
      let (lexbuf, file) = 
        open_lex (_to_string defaults.model) in
      let out = Parser_main.model Lexer_main.lex lexbuf in 
      close_in file; 
      out
    in
    let t0 = Unix.gettimeofday () in
    let env = Store.init_env decs in
    Store.parse_consts defaults.consts env;
    Store.store_decs decs env;
    let (s0, stoch, p_classes) =
      Store.store_brs brs env in (
      match defaults.out_store with
      | None -> ()
      | Some path -> (
          print_endline (
            (colorise `yellow "Exporting declarations to ") ^ 
              (colorise `yellow (colorise `bold path)) ^ 
              (colorise `yellow " ...")
          );
	  Store.export decs env path defaults.verbose
        )
    );
    print_stats_store env stoch t0;
    let p = Store.dummy_pos  
    and n = Hashtbl.length env in
    try
      if not stoch then (
	let brs_p_classes = List.map Store.p_to_brs p_classes 
        and get_f = Store.get_react p env in
        let (ts, stats) = 
          if defaults.sim then (
            print_endline (
              (colorise `yellow "Starting simulation ...\n") ^
                (colorise `cyan "Max # states: ") ^
                (string_of_int defaults.s_max)
            );
            Brs.sim_ide s0 brs_p_classes get_f defaults.s_max n iter_f
          )
          else (
            print_endline (
              (colorise `yellow "Starting transition system construction ...\n") ^
                (colorise `cyan "Max # states: ") ^
                (string_of_int defaults.s_max)
            );
            Brs.bfs_ide s0 brs_p_classes get_f defaults.s_max n iter_f 
          ) in
        after_brs stats ts false 
      ) else (
	let sbrs_p_classes = List.map Store.p_to_sbrs p_classes 
        and get_f = Store.get_sreact p env in
        let (ctmc, stats) = 
          if defaults.sim then (
            print_endline (
              (colorise `yellow "Starting stochastic simulation ...\n") ^
                (colorise `cyan "Max sim time: ") ^
                (string_of_float defaults.t_max)
            );
            Sbrs.sim_ide s0 sbrs_p_classes get_f defaults.t_max n iter_f
          )
          else (
            print_endline (
              (colorise `yellow "Starting CTMC construction ...\n") ^
                (colorise `cyan "Max # states: ") ^
                (string_of_int defaults.s_max)
              );
            Sbrs.bfs_ide s0 sbrs_p_classes get_f defaults.s_max n iter_f 
          ) in
          after_sbrs stats ctmc false false
      )
    with
    | Sbrs.LIMIT (ctmc, stats) -> (
        if defaults.sim then after_sbrs stats ctmc true false
        else after_sbrs stats ctmc false true
      ) 
    | Brs.LIMIT (ts, stats) -> after_brs stats ts true 
  with
  | Arg.Bad m -> 
    prerr_endline m; exit 1 
  | Parsing.Parse_error ->
    prerr_endline (colorise `red "Parsing unsuccesful"); 
    exit 1
  | Store.INVALID_CONSTS | Store.INVALID_VAL | Store.WRONG_TYPE 
  | Store.NO_IDE | Store.INVALID_PRI -> 
    exit 1
  | Link.FACES_MISMATCH (inner, outer) ->
    eprintf "%sImpossible to compose over faces:\n\
            \       %s\n\
            \       %s\n" 
      (colorise `red "Error: ")
      (Link.string_of_face inner) (Link.string_of_face outer); 
    exit 1
  | Link.NAMES_ALREADY_DEFINED (inner, outer) ->
    eprintf "%sDuplicate inner names: %s\n\
            \       Duplicate outer names: %s\n"
      (colorise `red "Error: ")
      (Link.string_of_face inner) (Link.string_of_face outer); 
    exit 1
  | Place.COMP_ERROR (sites, roots) ->
    eprintf "%sImpossible to compose with %d sites and %d regions\n"
      (colorise `red "Error: ")
      sites roots;
    exit 1
  | Big.SHARING_ERROR -> 
    eprintf "%sInvalid sharing expression\n"    
      (colorise `red "Error: "); 
    exit 1
  | Big.CTRL_ERROR (n, face) ->
    eprintf "%sImpossible to use face %s with a control of arity %d\n" 
      (colorise `red "Error: ")
      (Link.string_of_face face) n;
    exit 1
  | Big.ISO_ERROR (n, n_dom) ->
    eprintf "%sIsomorphism is not total: (%d < %d)\n"
      (colorise `red "Error: ")
      n_dom n;
    exit 1
  | e -> 
    eprintf "%s\n%s" (Printexc.to_string e) (Printexc.get_backtrace ()); exit 1
