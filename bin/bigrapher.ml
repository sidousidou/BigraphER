open Format
open Ast

(******** PRETTY PRINTING FUNCTIONS *********)

let colorise c msg =
  if Cmd.(defaults.colors)
  then Utils.colorise c msg
  else msg

let max_width = 50

type val_type = [ `s of string | `i of int | `f of float ]

type row =
  { descr:   string * Utils.text_style;
    value:   val_type;
    pp_val:  formatter -> val_type -> unit;
    display: bool; }

let print_msg fmt c msg =
  if not Cmd.(defaults.debug) then
    fprintf fmt "@?@[%s@]@." (colorise c msg)
  else ()

let print_descr fmt (d, c) =
  fprintf fmt "%s" (colorise c d)

let print_float unit fmt = function
  | `f f  -> fprintf fmt "@[<h>%-3g%s@]" f unit
  | `i _
  | `s _ -> assert false (*BISECT-IGNORE*)

let print_string fmt = function
  | `s s -> fprintf fmt "@[<h>%s@]" s
  | `i _
  | `f _ -> assert false (*BISECT-IGNORE*)

let print_int fmt = function
  | `i i -> fprintf fmt "@[<h>%-8d@]" i
  | `f _
  | `s _ -> assert false (*BISECT-IGNORE*)

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
     fprintf fmt "@[<h>%s" (colorise (snd r.descr) (fst r.descr));
     pp_print_break fmt (15 - (String.length (fst r.descr))) 0;
     fprintf fmt "@]";
     pp_set_tab fmt ();
     fprintf fmt "%a" r.pp_val r.value; 
     List.iter (pp_row fmt) rows;
     pp_close_tbox fmt ();
     Format.pp_print_newline fmt ())
  | _ -> assert false (*BISECT-IGNORE*)

let print_header fmt () =
  if not Cmd.(defaults.debug) then
    (fprintf fmt "@[<v>@,%s@,%s@,"
       (colorise `bold "BigraphER: Bigraph Evaluator & Rewriting")
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

let print_stats_store fmt env n stoch =
  let ty = if stoch then "Stochastic BRS" else "BRS" in
  [{ descr = ("Type:", `cyan);
     value = `s ty;
     pp_val = print_string;
     display = true; };
   { descr = ("Bindings:", `cyan);
     value = `i (Store.Hashtbl.length env);
     pp_val = print_int;
     display = true; };
   { descr = ("# of rules:", `cyan);
     value = `i n;
     pp_val = print_int;
     display = true; }]
  |> print_table fmt

let print_max fmt =
  [{ descr = ("Max # states:", `cyan);
     value = `i Cmd.(defaults.max_states);
     pp_val = print_int;
     display = true; }]
  |> print_table fmt

let print_stats fmt t s r o =
  [{ descr = ("Build time:", `green);
     value = `f t;
     pp_val = print_float "s";
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

let open_progress_bar () =
  if Cmd.(defaults.debug) || Cmd.(defaults.quiet) then () 
  else Pervasives.print_string "\n["

let print_loop i _ = 
  if Cmd.(defaults.debug) || Cmd.(defaults.quiet) then () 
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

let close_progress_bar () =
  if Cmd.(defaults.debug) || Cmd.(defaults.quiet) then ()
  else Pervasives.print_string "]\n\n"

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

let export_states fmt f g =
  if Cmd.(defaults.export_states_flag) then
    (match Cmd.(defaults.export_states) with
     | None -> assert false (*BISECT-IGNORE*)
     | Some path ->
       (print_msg fmt `yellow ("Exporting states to " ^ path ^ " ...");
        f ~f:(fun i s ->
            let aux i s f ext =
              let fname = (string_of_int i) ^ ext in
              try
                f s ~name:fname ~path
                |> print_fun fmt
                  `white
                  Cmd.(defaults.verb)
                  (Filename.concat path fname)
              with
              | Export.ERROR e ->
                (pp_print_flush fmt ();
                 fprintf err_formatter "@[<v>";
                 Export.report_error e
                 |> fprintf err_formatter "@[%s: %s@]@." Utils.err) in
            List.iter (function 
                | Cmd.Svg -> aux i s Big.write_svg ".svg"
                | Cmd.Dot -> aux i s Big.write_dot ".dot")
              Cmd.(defaults.out_format))
          g))
  else ()

let export_ctmc_states fmt =
  export_states fmt Sbrs.iter_states

let export_ts_states fmt =
  export_states fmt Brs.iter_states

let export_ts fmt msg formats =
  match Cmd.(defaults.export_graph) with
  | None -> ()
  | Some file ->
    (let name =
       let n = Filename.basename file in
       try Filename.chop_extension n with
       | Invalid_argument _ -> n
     and path = Filename.dirname file in
     List.iter (fun (f, ext) ->
         try
           let file' =
             Filename.concat path (name ^ ext) in
           print_msg fmt `yellow (msg ^ file' ^ " ...");
           f ~name:(name ^ ext) ~path
           |> print_fun fmt `white Cmd.(defaults.verb) file
         with
         | Export.ERROR e ->
           (pp_print_flush fmt ();
            fprintf err_formatter "@[<v>";
            Export.report_error e
            |> fprintf err_formatter "@[%s: %s@]@." Utils.err))
       formats)

let after_brs_aux fmt stats ts =
  let format_map = function
    | Cmd.Svg -> (Brs.write_svg ts, ".svg")
    | Cmd.Dot -> (Brs.write_dot ts, ".dot") in
  print_stats fmt
    stats.Brs.time
    stats.Brs.states
    stats.Brs.trans
    stats.Brs.occs;
  export_ts fmt
    "Exporting transition system to "
    (List.map format_map Cmd.(defaults.out_format));
  export_ts_states fmt ts;
  export_ts_prism fmt ts;
  export_ts_csl fmt ts;
  pp_print_flush err_formatter ();
  exit 0

let after_brs fmt (ts,stats) =
  close_progress_bar ();
  after_brs_aux fmt stats ts

let after_sbrs_aux fmt stats ctmc =
  let format_map = function
    | Cmd.Svg -> (Sbrs.write_svg ctmc, ".svg")
    | Cmd.Dot -> (Sbrs.write_dot ctmc, ".dot") in
  print_stats fmt
    stats.Sbrs.time
    stats.Sbrs.states
    stats.Sbrs.trans
    stats.Sbrs.occs;
  export_ts fmt
    "Exporting CTMC to "
    (List.map format_map Cmd.(defaults.out_format));
  export_ctmc_states fmt ctmc;
  export_ctmc_prism fmt ctmc;
  export_ctmc_csl fmt ctmc;
  pp_print_flush err_formatter ();
  exit 0

let after_sbrs fmt (ctmc, stats) =
  close_progress_bar ();
  after_sbrs_aux fmt stats ctmc

let check fmt =
  print_msg fmt `yellow "Model file parsed correctly";
  pp_print_flush err_formatter ();
  exit 0

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

let parse_cmd argv =
  let lexbuf = Array.to_list argv
               |> List.tl
               |> String.concat "\n" 
               |> Lexing.from_string in
  (* Check environment variables *)
  Cmd.eval_env ();
  try
    Parser.cmd Lexer.cmd lexbuf
  with
  | Cmd.ERROR e ->
    (Cmd.report_error err_formatter e;
     Cmd.eval_help_top err_formatter ();
     exit 1)
  | Parser.Error ->
    (Cmd.report_error err_formatter (Cmd.Parse (Lexing.lexeme lexbuf));
     Cmd.eval_help_top err_formatter ();
     exit 1)
  | Lexer.ERROR (e, _) ->
    (Lexer.report_error err_formatter e;
     pp_print_newline err_formatter ();    
     Cmd.eval_help_top err_formatter ();
     exit 1)

let set_output_ch () =
  if Cmd.(defaults.quiet) then
    (Cmd.(defaults.verb <- false); (* Ignore verbose flag *)
     str_formatter)
  else
    std_formatter

let () =
  Printexc.record_backtrace true; (* Disable for releases *)
  try
    let iter_f = print_loop in
    let exec_type = parse_cmd Sys.argv in
    let fmt = set_output_ch () in
    print_header fmt ();
    print_msg fmt `yellow ("Parsing model file "
                           ^ Cmd.(defaults.model)
                           ^ " ..."); 
    let (lexbuf, file) = open_lex Cmd.(defaults.model) in
    try
      let m = Parser.model Lexer.token lexbuf in 
      close_in file; 
      let env = Store.init_env fmt Cmd.(defaults.consts) in
      let (s0, prs, preds, env_t) = Store.eval_model fmt m env in
      (* STATS *)
      (match prs with
       | Store.P priorities ->
         (Cmd.check_brs_opt ();
          print_stats_store fmt env (Brs.cardinal priorities) false;)
       | Store.S priorities ->
         (Cmd.check_sbrs_opt ();
          print_stats_store fmt env (Sbrs.cardinal priorities) true;));
      (* DECLARATIONS *)
      (match Cmd.(defaults.export_decs) with
       | None -> ()
       | Some path ->
         (let format_map = function
             | Cmd.Svg -> (Big.write_svg, ".svg")
             | Cmd.Dot -> (Big.write_dot, ".dot") in
          print_msg fmt `yellow ("Exporting declarations to "
                                 ^ path ^ " ...");
          Store.export m.model_decs
            env
            env_t
            path
            (List.map format_map Cmd.(defaults.out_format))
            fmt
            (print_fun fmt `white Cmd.(defaults.verb))));
      (match Cmd.(defaults.export_ml) with
       | None -> ()
       | Some path ->
         (print_msg fmt `yellow ("Exporting OCaml declarations to "
                                 ^ path ^ " ...");
          try
            Export.write_string (Store.ml_of_model m Cmd.(defaults.model))
              ~name:(Filename.basename path)
              ~path:(Filename.dirname path)
            |> print_fun fmt
              `white
              Cmd.(defaults.verb)
              path
          with
          | Export.ERROR e ->
            (pp_print_flush fmt ();
             fprintf err_formatter "@[<v>";
             Export.report_error e
             |> fprintf err_formatter "@[%s: %s@]@." Utils.err)));
      match prs with
      | Store.P priorities ->
        (******** BRS *********)
        (match exec_type with
         | `sim ->
           (print_msg fmt `yellow "Starting simulation ...";
            print_max fmt;
            open_progress_bar ();
            Brs.sim ~s0
              ~priorities
              ~predicates:preds
              ~init_size:Cmd.(defaults.max_states)
              ~stop:Cmd.(defaults.steps)
              ~iter_f
            |> after_brs fmt)
         | `full ->
           (print_msg fmt `yellow "Computing transition system ...";
            print_max fmt;
            open_progress_bar ();
            Brs.bfs ~s0
              ~priorities
              ~predicates:preds
              ~max:Cmd.(defaults.max_states)
              ~iter_f
            |> after_brs fmt)
         | `check -> check fmt)
      | Store.S priorities ->
        (******** SBRS *********)
        (match exec_type with
         | `sim ->
           (print_msg fmt `yellow "Starting stochastic simulation ...";
            [{ descr = ("Max sim time:", `cyan);
               value = `f Cmd.(defaults.time);
               pp_val = print_float "";
               display = true; }]
            |> print_table fmt;
            open_progress_bar ();
            Sbrs.sim ~s0
              ~priorities
              ~predicates:preds
              ~init_size:Cmd.(defaults.max_states)
              ~stop:Cmd.(defaults.time)
              ~iter_f
            |> after_sbrs fmt)
         | `full ->
           (print_msg fmt `yellow "Computing CTMC ...";
            print_max fmt;
            open_progress_bar ();
            Sbrs.bfs ~s0
              ~priorities
              ~predicates:preds
              ~max:Cmd.(defaults.max_states)
              ~iter_f
            |>  after_sbrs fmt)
         | `check -> check fmt)
    with
    | Place.NOT_PRIME ->
      (close_progress_bar ();
       fprintf err_formatter "@[<v>@[%s: The parameter of a reaction rule is not prime.@]@."
         Utils.err;
       exit 1)
    | Sbrs.MAX (ctmc, stats)
    | Sbrs.LIMIT (ctmc, stats) ->
      (close_progress_bar ();
       print_msg fmt `yellow "Maximum number of states reached.";
       after_sbrs_aux fmt stats ctmc)
    | Sbrs.DEADLOCK (ctmc, stats, t) ->
      (close_progress_bar ();
       print_msg fmt `yellow ("Deadlock state reached at time " ^ (string_of_float t) ^ ".");
       after_sbrs_aux fmt stats ctmc)
    | Brs.MAX (ts, stats)
    | Brs.LIMIT (ts, stats) ->
      (close_progress_bar ();
       print_msg fmt `yellow "Maximum number of states reached.";
       after_brs_aux fmt stats ts)
    | Brs.DEADLOCK (ts, stats, t) ->
      (close_progress_bar ();
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
    (fprintf err_formatter "@[<v>";
     Loc.print_loc err_formatter p;
     Lexer.report_error err_formatter e;
     pp_print_newline err_formatter ();
     exit 1)
  | Sys_error s ->
    (fprintf err_formatter "@[%s: %s@]@." Utils.err s;
     exit 1)
  | e -> 
    (fprintf err_formatter "@[%s@,%s@]@."
       (Printexc.to_string e) (Printexc.get_backtrace ());
     exit 1)
