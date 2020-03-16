open Ast
open Bigraph

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
    pp_val:  Format.formatter -> val_type -> unit;
    display: bool; }

let print_msg fmt c msg =
  if not Cmd.(defaults.debug) then
    Format.fprintf fmt "@?@[%s@]@." (colorise c msg)

let print_descr fmt (d, c) =
  Format.fprintf fmt "%s" (colorise c d)

let pp_float unit fmt = function
  | `f f  -> Format.fprintf fmt "@[<h>%-3g%s@]" f unit
  | `i _
  | `s _ -> assert false (*BISECT-IGNORE*)

let pp_string fmt = function
  | `s s -> Format.fprintf fmt "@[<h>%s@]" s
  | `i _
  | `f _ -> assert false (*BISECT-IGNORE*)

let pp_int fmt = function
  | `i i -> Format.fprintf fmt "@[<h>%-8d@]" i
  | `f _
  | `s _ -> assert false (*BISECT-IGNORE*)

let print_table fmt (rows : row list) =
  let pp_row fmt r =
    Format.(
        pp_print_tab fmt ();
        fprintf fmt "%a" print_descr r.descr;
        pp_print_tab fmt ();
        fprintf fmt "%a" r.pp_val r.value) in
  match List.filter (fun r -> r.display) rows with
  |  r :: rows ->
      Format.(pp_open_tbox fmt ();
              (* First row *)
              pp_set_tab fmt ();
              fprintf fmt "@[<h>%s" (colorise (snd r.descr) (fst r.descr));
              pp_print_break fmt (15 - (String.length (fst r.descr))) 0;
              fprintf fmt "@]";
              pp_set_tab fmt ();
              fprintf fmt "%a" r.pp_val r.value;
              List.iter (pp_row fmt) rows;
              pp_close_tbox fmt ();
              pp_print_newline fmt ())
  | _ -> assert false (*BISECT-IGNORE*)

let print_header fmt () =
  if not Cmd.(defaults.debug) then
    begin
      Format.fprintf fmt "@[<v>@,%s@,%s@,"
        (colorise `bold "BigraphER: Bigraph Evaluator & Rewriting")
        "========================================";
      [{ descr = ("Version:", `blue);
         value = `s (String.trim Version.version);
         pp_val = pp_string;
         display = true; };
       { descr = ("Date:", `blue);
         value = `s (Utils.format_time ());
         pp_val = pp_string;
         display = true; };
       { descr = ("Hostname:", `blue);
         value = `s (Unix.gethostname ());
         pp_val = pp_string;
         display = true; };
       { descr = ("OS type:", `blue);
         value = `s Sys.os_type;
         pp_val = pp_string;
         display = true; };
       { descr = ("Command line:", `blue);
         value = `s (String.concat " " (Array.to_list Sys.argv));
         pp_val = pp_string;
         display = true; }]
      |> print_table fmt
    end

let print_max fmt =
  print_table fmt
    [{ descr = ("Max # states:", `cyan);
       value = `i Cmd.(defaults.max_states);
       pp_val = pp_int;
       display = true; }]

let print_max_sim fmt = function
  | Rs.BRS | Rs.PBRS | Rs.NBRS ->
    print_table fmt
      [{ descr = ("Max sim steps:", `cyan);
         value = `i Cmd.(defaults.steps);
         pp_val = pp_int;
         display = true; }]
  | Rs.SBRS ->
    print_table fmt
      [{ descr = ("Max sim time:", `cyan);
         value = `f Cmd.(defaults.time);
         pp_val = pp_float "";
         display = true; }]

let open_progress_bar () =
  if not (Cmd.(defaults.debug) || Cmd.(defaults.quiet)) then
    print_string "\n["

let print_loop i _ =
  if not (Cmd.(defaults.debug) || Cmd.(defaults.quiet)) then
    begin
      let m =
        if Cmd.(defaults.max_states) >= 1000 then
          Cmd.(defaults.max_states) / 1000
        else 1 in
      match (i + 1) mod (max_width * m) with
      | 0 -> (print_char '.';
              print_string "  ";
              print_int (i + 1);
              print_newline ();
              print_char ' ';
              flush stdout)
      | i when i mod m = 0 -> (print_char '.';
                               flush stdout)
      | _ -> ()
    end

let close_progress_bar () =
  if not (Cmd.(defaults.debug) || Cmd.(defaults.quiet)) then
    print_string "]\n\n"

(******** EXPORT FUNCTIONS *********)

let print_fun fmt c verb fname i =
  if verb then
    print_msg fmt c ((string_of_int i) ^ " bytes written to `" ^ fname ^ "'")

let format_map = function
  | Cmd.Svg -> (Export.B.write_svg, ".svg")
  | Cmd.Dot -> (Export.B.write_dot, ".dot")
  | Cmd.Json -> (Export.B.write_json, ".json")
  | Cmd.Txt -> (Export.B.write_txt, ".txt")

let export_prism argument fmt msg f =
  match argument with
  | None -> ()
  | Some file ->
    (print_msg fmt `yellow (msg ^ file ^ " ...");
     try
       f ~name:(Filename.basename file)
         ~path:(Filename.dirname file)
       |> print_fun fmt `white Cmd.(defaults.verb) file
     with
     | Rs.EXPORT_ERROR e ->
       Format.(pp_print_flush fmt ();
               fprintf err_formatter "@[<v>@[%s: %s@]@."
                 (Utils.err_opt Cmd.(defaults.colors))
                 e))

let export_csl fmt f =
  match Cmd.(defaults.export_lab) with
  | None -> ()
  | Some file ->
    (print_msg fmt `yellow ("Exporting properties to " ^ file ^ " ...");
     try
       f ~name:(Filename.basename file) ~path:(Filename.dirname file)
       |> print_fun fmt `white Cmd.(defaults.verb) file
     with
     | Rs.EXPORT_ERROR e ->
       Format.(pp_print_flush fmt ();
               fprintf err_formatter "@[<v>@[%s: %s@]@."
                 (Utils.err_opt Cmd.(defaults.colors))
                 e))

let export_states fmt f g =
  if Cmd.(defaults.export_states_flag) then
    begin
      match Cmd.(defaults.export_states) with
      | None -> assert false (*BISECT-IGNORE*)
      | Some path ->
        (print_msg fmt `yellow ("Exporting states to " ^ path ^ " ...");
         f (fun i s ->
             let aux i s f ext =
               let fname = (string_of_int i) ^ ext in
               try
                 f s ~name:fname ~path
                 |> print_fun fmt
                   `white
                   Cmd.(defaults.verb)
                   (Filename.concat path fname)
               with
               | Failure msg ->
                 Format.(pp_print_flush fmt ();
                         fprintf err_formatter "@[<v>@[%s: %s@]@."
                           (Utils.err_opt Cmd.(defaults.colors))
                           msg) in
             Cmd.(defaults.out_format)
             |> List.map format_map
             |> List.iter (fun (f, ext) ->  aux i s f ext))
           g)
    end

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
         | Rs.EXPORT_ERROR e ->
            Format.(pp_print_flush fmt ();
                    fprintf err_formatter "@[<v>@[%s: %s@]@."
                      (Utils.err_opt Cmd.(defaults.colors))
                      e))
       formats)

let check fmt =
  print_msg fmt `yellow "Model file parsed correctly";
  Format.(pp_print_flush err_formatter ());
  exit 0

module Run
    (T: TsType.RS)
    (L: sig
       val stop : T.limit
     end)
    (P: sig
       val parse_react : string -> Big.t -> Big.t ->
         [ `E of unit | `F of float | `P of string * int * float ]
         -> Fun.t option -> T.react option
    end)
    (J: sig
       val f: ?minify:bool -> T.graph -> String.t
     end) = struct

  module S = Store.Make (T) (P)

  module E = Export.T (T) (J)
  
  let export_decs fmt path m env env_t =
    print_msg fmt `yellow ("Exporting declarations to "
                           ^ path ^ " ...");
    S.export m.model_decs
      env
      env_t
      path
      (List.map format_map Cmd.(defaults.out_format))
      fmt
      Cmd.(defaults.colors)
      (print_fun fmt `white Cmd.(defaults.verb))

  let export_ml fmt path m =
    print_msg fmt `yellow ("Exporting OCaml declarations to "
                           ^ path ^ " ...");
    try
      Export.write_string
        (match Cmd.(defaults.model) with
         | None -> "stdin.big"
         | Some s -> s
        |> S.ml_of_model m)
        ~name:(Filename.basename path)
        ~path:(Filename.dirname path)
      |> print_fun fmt
        `white
        Cmd.(defaults.verb)
        path
    with
    | Export.ERROR e ->
      Format.(pp_print_flush fmt ();
              fprintf err_formatter "@[<v>@[%s: %s@]@."
                (Utils.err_opt Cmd.(defaults.colors))
                (Export.report_error e))

  let export_model fmt m env env_t =
    (* DECLARATIONS *)
    (match Cmd.(defaults.export_decs) with
     | None -> ()
     | Some path -> export_decs fmt path m env env_t);
    (* Export model to OCaml *)
    (match Cmd.(defaults.export_ml) with
     | None -> ()
     | Some path -> export_ml fmt path m)

  let print_stats_store fmt env priorities =
    [{ descr = ("Type:", `cyan);
       value = `s (Rs.to_string T.typ);
       pp_val = pp_string;
       display = true; };
     { descr = ("Bindings:", `cyan);
       value = `i (Base.H_string.length env);
       pp_val = pp_int;
       display = true; };
     { descr = ("# of rules:", `cyan);
       value = `i (T.cardinal priorities);
       pp_val = pp_int;
       display = true; }]
    |> print_table fmt

  let print_stats fmt stats =
    Stats.descr stats
    |> List.map (fun (descr, value, flag) ->
        { descr = (descr, `green);
          value = `s value;
          pp_val = pp_string;
          display =  (not Cmd.(defaults.debug) || not flag); })
    |> print_table fmt

  let after fmt f (graph, stats) =
    f ();
    let format_map = function
      | Cmd.Svg -> (E.write_svg graph, ".svg")
      | Cmd.Json -> (E.write_json graph, ".json")
      | Cmd.Dot -> (E.write_dot graph, ".dot")
      | Cmd.Txt -> (E.write_prism graph, ".txt") in
    print_stats fmt stats;
    export_ts fmt
      ("Exporting " ^ (Rs.to_string T.typ) ^ " to ")
      (List.map format_map Cmd.(defaults.out_format));
    export_states fmt T.iter_states graph;
    export_prism Cmd.(defaults.export_prism) fmt
      ("Exporting " ^ (Rs.to_string T.typ) ^ " in PRISM format to ")
      (E.write_prism graph);
    export_prism Cmd.(defaults.export_state_rewards) fmt
      ("Exporting the state rewards of "
       ^ (Rs.to_string T.typ)
       ^ " in PRISM format to ")
      (E.write_state_rewards graph);
    export_prism Cmd.(defaults.export_transition_rewards) fmt
      ("Exporting the transition rewards of "
       ^ (Rs.to_string T.typ)
       ^ " in PRISM format to ")
      (E.write_transition_rewards graph);
    export_csl fmt (E.write_lab graph);
    Format.(pp_print_flush err_formatter ());
    exit 0

  let sim fmt s0 priorities preds =
    print_msg fmt `yellow ("Starting " ^ (Rs.sim_type T.typ) ^ " ...");
    print_max_sim fmt T.typ;
    open_progress_bar ();
    T.sim ~s0
      ~priorities
      ~predicates:preds
      ~init_size:Cmd.(defaults.max_states)
      ~stop:L.stop
      ~iter_f:print_loop
    |> after fmt close_progress_bar

  let full fmt s0 priorities preds =
    print_msg fmt `yellow ("Computing " ^ (Rs.ts_type T.typ) ^ " ...");
    print_max fmt;
    open_progress_bar ();
    T.bfs ~s0
      ~priorities
      ~predicates:preds
      ~max:Cmd.(defaults.max_states)
      ~iter_f:print_loop
    |> after fmt close_progress_bar

  let set_trap fmt  =
    Sys.set_signal Sys.sigint
      (Sys.Signal_handle (fun _ ->
           close_progress_bar ();
           print_msg fmt `yellow "Execution interrupted by the user.";
           Format.(pp_print_flush err_formatter ());
           exit 0))

  let run_aux fmt s0 priorities preds = function
    | `sim ->
      begin
        set_trap fmt;
        try sim fmt s0 priorities preds with
        | T.LIMIT (graph, stats) ->
          after fmt
            (fun () ->
               close_progress_bar ();
               print_msg fmt `yellow
                 ("Maximum " ^ (Rs.limit_msg T.typ) ^ " reached."))
            (graph, stats)
        | T.DEADLOCK (graph, stats, limit) ->
          after fmt
            (fun () ->
               close_progress_bar ();
               print_msg fmt `yellow
                 ("Deadlock state reached at "
                  ^ (Rs.limit_type T.typ) ^ " "
                  ^ (T.string_of_limit limit) ^ "."))
            (graph, stats)
      end
    | `full ->
      begin
        set_trap fmt;
        try full fmt s0 priorities preds with
        | T.MAX (graph, stats) ->
          after fmt
            (fun () ->
               close_progress_bar ();
               print_msg fmt `yellow
                 "Maximum number of states reached.")
            (graph, stats)
      end
    | `check -> check fmt
    | `exit -> ()

  let run fmt c m exec_type =
    try
      let env = S.init_env fmt c
          Cmd.(defaults.consts) in
      let (s0, pri, preds, env_t) =
        S.eval_model fmt c m env in
      print_stats_store fmt env pri;
      export_model fmt m env env_t;
      run_aux fmt s0 pri preds exec_type
    with
    | S.ERROR (e, p) ->
       Format.(pp_print_flush fmt ();
               fprintf err_formatter "@[<v>";
               Loc.print_loc err_formatter p;
               S.report_error err_formatter c e;
               pp_print_flush err_formatter ();
               exit 1)

end

(******** BIGRAPHER *********)

let open_lex model =
  let (fname, file) =
    match model with
    | None -> ("stdin", stdin)
    | Some path -> (Filename.basename path, open_in path) in
  let lexbuf = Lexing.from_channel file in
  lexbuf.Lexing.lex_curr_p <-
    { Lexing.pos_fname = fname;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0; };
  (lexbuf, file)
  
let set_output_ch () =
  if Cmd.(defaults.quiet) then begin
    Cmd.(defaults.verb <- false); (* Ignore verbose flag *)
    Format.str_formatter
  end else
    Format.std_formatter

let () =
  let exec_type = Cmd.parse_cmds in
  (* Allow early exit, e.g. when the config is to be printed *)
  match exec_type with
  | `exit -> exit 1
  | _ -> ();
  try
    let fmt = set_output_ch () in
    print_header fmt ();
    print_msg fmt `yellow ("Parsing model file "
                           ^ (match Cmd.(defaults.model) with
                               | None -> "stdin"
                               | Some name -> name)
                           ^ " ...");
    let (lexbuf, file) = open_lex Cmd.(defaults.model) in
    try
      let m = Parser.model Lexer.token lexbuf in
      close_in file;
      (match m.model_rs.dbrs_type with
       | Rs.BRS ->
         begin
           let module R = Run
               (struct

                 include Brs

                 let parse_react_unsafe ~name ~lhs ~rhs _ eta =
                   parse_react_unsafe ~name ~lhs ~rhs eta
                
                 let parse_react ~name ~lhs ~rhs _ eta =
                   parse_react ~name ~lhs ~rhs eta
                   
               end)
               (struct
                 let stop = Cmd.(defaults.steps)
               end)
               (struct
                 let parse_react name lhs rhs _ eta =
                   Brs.parse_react ~name ~lhs ~rhs eta
               end)
               (struct
                  let f = Big_json.ts_to_json
                end) in
           R.run fmt Cmd.(defaults.colors) m exec_type
         end
       | Rs.PBRS ->
         begin
           let module R = Run
               (Pbrs)
               (struct
                 let stop = Cmd.(defaults.steps)
               end)
               (struct
                 let parse_react name lhs rhs l eta =
                   match l with
                   | `F f -> Pbrs.parse_react ~name ~lhs ~rhs f eta
                   | _ -> assert false  (*BISECT-IGNORE*)
               end)
               (struct
                 let f = Big_json.dtmc_to_json
               end) in
           R.run fmt Cmd.(defaults.colors) m exec_type
         end
       | Rs.SBRS ->
         begin
           let module R = Run
               (Sbrs)
               (struct
                 let stop = Cmd.(defaults.time)
               end)
               (struct
                 let parse_react name lhs rhs l eta =
                   match l with
                   | `F f -> Sbrs.parse_react ~name ~lhs ~rhs f eta
                   |  _ -> assert false  (*BISECT-IGNORE*)
               end)
               (struct
                 let f = Big_json.ctmc_to_json
                end) in
           R.run fmt Cmd.(defaults.colors) m exec_type
         end
       | Rs.NBRS ->
         begin
           let module R = Run
               (Nbrs)
               (struct
                 let stop = Cmd.(defaults.steps)
               end)
               (struct
                 let parse_react name lhs rhs l eta =
                   match l with
                   | `P p -> Nbrs.parse_react ~name ~lhs ~rhs p eta
                   | _ -> assert false  (*BISECT-IGNORE*)
               end)
               (struct
                 let f = Big_json.mdp_to_json
               end) in
           R.run fmt Cmd.(defaults.colors) m exec_type
         end);
    with
    | Place.NOT_PRIME ->
      (close_progress_bar ();
       Format.(fprintf
                 err_formatter
                 "@[<v>@[%s: The parameter of a reaction rule is not prime.@]@."
                 (Utils.err_opt Cmd.(defaults.colors)));
       exit 1)
    | Parser.Error ->
       Format.(pp_print_flush fmt ();
               fprintf err_formatter "@[<v>";
               Loc.print_loc err_formatter
                 Loc.{lstart = Lexing.(lexbuf.lex_start_p);
                      lend = Lexing.(lexbuf.lex_curr_p)};
               fprintf
                 err_formatter
                 "@[%s: Syntax error near token `%s'@]@."
                 (Utils.err_opt Cmd.(defaults.colors))
                 (Lexing.lexeme lexbuf);
               exit 1)
  with
  | Lexer.ERROR (e, p) ->
     Format.(fprintf err_formatter "@[<v>";
             Loc.print_loc err_formatter p;
             Lexer.report_error err_formatter
               (Utils.err_opt Cmd.(defaults.colors))
               e;
             pp_print_newline err_formatter ();
             exit 1)
  | Sys_error s ->
     Format.(fprintf err_formatter "@[%s: %s@]@."
               (Utils.err_opt Cmd.(defaults.colors))
               s;
             exit 1)
  | e ->
     Format.(fprintf err_formatter "@[%s@,%s@]@."
               (Printexc.to_string e) (Printexc.get_backtrace ());
             exit 1)
