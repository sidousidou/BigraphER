open Format
open Utils
open Cmdliner

type path = string

type file = string

type format_op = Dot | Svg | Txt | Json

type settings = {
  mutable consts : Ast.const list;
  mutable debug : bool;
  mutable dot_installed : bool;
  mutable export_graph : file option;
  mutable export_lab : file option;
  mutable export_prism : file option;
  mutable export_states : path option;
  mutable export_states_flag : bool;
  mutable export_state_rewards : path option;
  mutable export_transition_rewards : path option;
  mutable help : bool;
  mutable max_states : int;
  mutable model : string option;
  mutable out_format : format_op list;
  mutable quiet : bool;
  mutable seed : int option;
  mutable steps : int;
  mutable steps_flag : bool;
  mutable time : float;
  mutable time_flag : bool;
  mutable verb : bool;
  mutable colors : bool;
  mutable running_time : bool;
  mutable solver : Bigraph.Solver.solver_t;
}

let default_formats = [ Dot ]

let defaults =
  {
    consts = [];
    debug = false;
    dot_installed = dot_installed ();
    export_graph = None;
    export_lab = None;
    export_prism = None;
    export_states = None;
    export_states_flag = false;
    export_state_rewards = None;
    export_transition_rewards = None;
    help = false;
    max_states = 1000;
    model = None;
    out_format = default_formats;
    quiet = false;
    seed = None;
    steps = 1000;
    steps_flag = false;
    time = 1000.0;
    time_flag = false;
    verb = false;
    colors = true;
    running_time = false;
    solver = Bigraph.Solver.MSAT (* MiniSAT *);
  }

type cmd_t = [ `check | `full | `sim | `exit ]

(* Stand alone options *)
let print_table fmt rows ?(offset = 0) f_l f_r =
  (* Find longest row *)
  let l_max =
    List.fold_left
      (fun max r ->
        let l = String.length (f_l r) in
        if l > max then l else max)
      0 rows
  in
  let pp_row fmt row =
    pp_print_tab fmt ();
    pp_print_string fmt (f_l row);
    pp_print_tab fmt ();
    fprintf fmt "%a" f_r row
  in
  match rows with
  | first :: rows ->
      pp_open_tbox fmt ();
      pp_set_tab fmt ();
      let l = f_l first in
      fprintf fmt "@[<h>%s" l;
      pp_print_break fmt (l_max - offset) 0;
      fprintf fmt "@]";
      pp_set_tab fmt ();
      fprintf fmt "%a" f_r first;
      List.iter (pp_row fmt) rows;
      pp_close_tbox fmt ()
  | _ -> assert false

let string_of_format f =
  List.map
    (function Svg -> "svg" | Json -> "json" | Dot -> "dot" | Txt -> "txt")
    f
  |> String.concat ","

let string_of_solver_type = function
  | Bigraph.Solver.MSAT -> "MiniSAT"
  | Bigraph.Solver.MCARD -> "MiniCARD"

let string_of_file = function None -> "-" (* Not set *) | Some f -> f

let parse_formats s =
  String.split_on_char ',' s
  |> List.fold_left
       (fun acc s ->
         match s with
         | "dot" -> Dot :: acc
         | "svg" -> Svg :: acc
         | "txt" -> Txt :: acc
         | "json" -> Json :: acc
         | _ -> acc)
       []
  |> function
  | [] -> default_formats
  | x -> x

let parse_solver_type = function
  | "MCARD" -> Bigraph.Solver.MCARD
  | "MSAT" -> Bigraph.Solver.MSAT
  | _ -> Bigraph.Solver.MSAT

(* Defaults to MiniSAT *)

let eval_config fmt () =
  let config_str fmt () =
    let conf =
      [
        ( "colors",
          fun fmt () ->
            ( match Sys.getenv_opt "BIGNOCOLORS" with
            | None -> ()
            | Some _ -> defaults.colors <- false );
            fprintf fmt "@[<hov>%b@]" defaults.colors );
        ( "consts",
          fun fmt () ->
            fprintf fmt "@[<hov>%s@]"
              ( match Ast.string_of_consts defaults.consts with
              | "" -> "-"
              | s -> s ) );
        ("debug", fun fmt () -> fprintf fmt "@[<hov>%b@]" defaults.debug);
        ( "export_graph",
          fun fmt () ->
            fprintf fmt "@[<hov>%s@]" (string_of_file defaults.export_graph)
        );
        ( "export_lab",
          fun fmt () ->
            fprintf fmt "@[<hov>%s@]" (string_of_file defaults.export_lab) );
        ( "export_prism",
          fun fmt () ->
            fprintf fmt "@[<hov>%s@]" (string_of_file defaults.export_prism)
        );
        ( "export_states",
          fun fmt () ->
            fprintf fmt "@[<hov>%s@]" (string_of_file defaults.export_states)
        );
        ( "export_states_flag",
          fun fmt () -> fprintf fmt "@[<hov>%b@]" defaults.export_states_flag
        );
        ( "export_state_rewards",
          fun fmt () ->
            fprintf fmt "@[<hov>%s@]"
              (string_of_file defaults.export_state_rewards) );
        ( "export_transition_rewards",
          fun fmt () ->
            fprintf fmt "@[<hov>%s@]"
              (string_of_file defaults.export_transition_rewards) );
        ("help", fun fmt () -> fprintf fmt "@[<hov>%b@]" defaults.help);
        ( "max_states",
          fun fmt () -> fprintf fmt "@[<hov>%d@]" defaults.max_states );
        ( "out_format",
          fun fmt () ->
            ( match Sys.getenv_opt "BIGFORMAT" with
            | None -> ()
            | Some x -> defaults.out_format <- parse_formats x );
            fprintf fmt "@[<hov>%s@]" (string_of_format defaults.out_format)
        );
        ( "quiet",
          fun fmt () ->
            ( match Sys.getenv_opt "BIGQUIET" with
            | None -> ()
            | Some _ -> defaults.quiet <- true );
            fprintf fmt "@[<hov>%b@]" defaults.quiet );
        ( "running_time",
          fun fmt () -> fprintf fmt "@[<hov>%b@]" defaults.running_time );
        ( "seed",
          fun fmt () ->
            match defaults.seed with
            | None -> fprintf fmt "@[<hov>%s@]" "-"
            | Some x -> fprintf fmt "@[<hov>%d@]" x );
        ( "solver",
          fun fmt () ->
            ( match Sys.getenv_opt "BIGSOLVER" with
            | None -> ()
            | Some x -> defaults.solver <- parse_solver_type x );
            fprintf fmt "@[<hov>%s@]" (string_of_solver_type defaults.solver)
        );
        ("steps", fun fmt () -> fprintf fmt "@[<hov>%d@]" defaults.steps);
        ("time", fun fmt () -> fprintf fmt "@[<hov>%g@]" defaults.time);
        ( "verb",
          fun fmt () ->
            ( match Sys.getenv_opt "BIGVERBOSE" with
            | None -> ()
            | Some _ -> defaults.verb <- true );
            fprintf fmt "@[<hov>%b@]" defaults.verb );
      ]
    in
    print_table fmt conf (fun (x, _) -> x) (fun fmt (_, f) -> f fmt ())
  in
  fprintf fmt "@[<v 2>CONFIGURATION:@,%a@]@." config_str ()

let check_states () =
  if defaults.export_states_flag then
    match (defaults.export_states, defaults.export_graph) with
    | None, Some f ->
        defaults.export_states <- Some (Filename.dirname f);
        true
    | None, None ->
        fprintf err_formatter
          "@[A path must be specified for@ option@ \"-s\"@ unless it@ used@ \
           with@ option@ \"-t\" @]";
        false
    | Some _, None | Some _, Some _ -> true
  else true

let check_dot () =
  if not defaults.dot_installed then
    if List.mem Svg defaults.out_format then (
      (* reset flag *)
      let dot_msg =
        "`dot' is not installed on this system.\n\
        \                        Reverting to dot output format."
      in
      defaults.out_format <- default_formats;
      fprintf err_formatter "@[<v>%s Warning: %s@]"
        (warn_opt defaults.colors)
        dot_msg )

let const_conv =
  let lexer = Genlex.make_lexer [ "=" ] in
  let parse (s : string) =
    let toks = lexer (Stream.of_string s) in
    let varid = Stream.next toks in
    let _eq = Stream.next toks in
    let v = Stream.next toks in
    match varid with
    | Ident i -> (
        match v with
        | Int v ->
            Ok
              (Ast.Cint
                 {
                   d_id = i;
                   d_exp = ENum (Num_int_val (v, Loc.dummy_loc));
                   d_loc = Loc.dummy_loc;
                 })
        | Float v ->
            Ok
              (Ast.Cfloat
                 {
                   d_id = i;
                   d_exp = ENum (Num_float_val (v, Loc.dummy_loc));
                   d_loc = Loc.dummy_loc;
                 })
        | String v ->
            Ok
              (Ast.Cstr
                 {
                   d_id = i;
                   d_exp = EStr (Str_val (v, Loc.dummy_loc));
                   d_loc = Loc.dummy_loc;
                 })
        | _ -> Error (`Msg "Could not parse assignment") )
    | _ -> Error (`Msg "Could not parse assignment")
  in
  let print_format pf = function
    | Ast.Cint i -> pp_print_string pf (i.d_id ^ "=<exp>")
    | Ast.Cfloat f -> pp_print_string pf (f.d_id ^ "=<exp>")
    | Ast.Cstr s -> pp_print_string pf (s.d_id ^ "=<exp>")
  in
  (parse, print_format)

let fconv =
  let parse (s : string) =
    match s with
    | "dot" -> Ok Dot
    | "svg" -> Ok Svg
    | "txt" -> Ok Txt
    | "json" -> Ok Json
    | _ -> Error (`Msg "Could not parse format")
  in
  let print_format pf = function
    | Dot -> pp_print_string pf "dot"
    | Svg -> pp_print_string pf "svg"
    | Txt -> pp_print_string pf "txt"
    | Json -> pp_print_string pf "json"
  in
  (parse, print_format)

(* Option parsing *)
let opt_if = function Some _ -> true | None -> false

let empty_to_none = function
  | Some "" -> None
  | Some _ as s -> s
  | None -> None

let copts consts debug ext graph lbls prism quiet states srew trew verbose
    nocols rtime solver =
  defaults.consts <- consts;
  defaults.debug <- debug;
  defaults.out_format <- ext;
  defaults.export_graph <- graph;
  defaults.export_lab <- lbls;
  defaults.export_prism <- prism;
  defaults.quiet <- quiet;
  (* States have extra handling to ensure that the directory is inferred
   * from export-ts if required *)
  defaults.export_states <- empty_to_none states;
  defaults.export_states_flag <- opt_if states;
  defaults.export_state_rewards <- empty_to_none srew;
  defaults.export_transition_rewards <- empty_to_none trew;
  defaults.verb <- verbose;
  defaults.colors <- not nocols;
  defaults.running_time <- rtime;
  defaults.solver <- parse_solver_type solver

let copts_t =
  let opt_str = Arg.opt (Arg.some Arg.string) None in
  let vopt_str = Arg.opt ~vopt:(Some "") (Arg.some Arg.string) None in
  let debug = Arg.(value & flag & info [ "debug" ]) in
  let rtime = Arg.(value & flag & info [ "running-time" ]) in
  let consts =
    let doc =
      "Specify a comma-separated list of variable assignments.\n\
      \               Example: `x=4,t=.56'."
    in
    Arg.(
      value
      & opt (list (conv const_conv)) []
      & info [ "c"; "const" ] ~docv:"ASSIGNMENT" ~doc)
  in
  let ext =
    let doc =
      "A comma-separated list of output formats.\n\
      \               Supported formats are `dot', `json', `svg' and `txt'."
    in
    let env = Arg.env_var "BIGFORMAT" ~doc in
    Arg.(
      value
      & opt (list (conv fconv)) [ Dot ]
      & info [ "f"; "format" ] ~docv:"FORMAT" ~doc ~env)
  in
  let graph =
    let doc = "Export the transition system to $(docv)." in
    Arg.(value & opt_str & info [ "t"; "export-ts" ] ~docv:"FILE" ~doc)
  in
  let lbls =
    let doc =
      "Export the labelling function in PRISM csl format to $(docv)."
    in
    Arg.(value & opt_str & info [ "l"; "export-labels" ] ~docv:"FILE" ~doc)
  in
  let prism =
    let doc =
      "Export the transition system in PRISM tra format to $(docv)."
    in
    Arg.(value & opt_str & info [ "p"; "export-prism" ] ~docv:"FILE" ~doc)
  in
  let srew =
    let doc = "Export state rewards in PRISM srew format to $(docv)." in
    Arg.(
      value & opt_str
      & info [ "r"; "export-state-rewards" ] ~docv:"FILE" ~doc)
  in
  let trew =
    let doc = "Export transition rewards in PRISM srew format to $(docv)." in
    Arg.(
      value & opt_str
      & info [ "R"; "export-transition-rewards" ] ~docv:"FILE" ~doc)
  in
  let quiet =
    let doc = "Disable progress indicator." in
    let env = Arg.env_var "BIGQUIET" ~doc in
    Arg.(value & flag & info [ "q"; "quiet" ] ~doc ~env)
  in
  let states =
    let doc =
      "Export each state to a file in $(docv).\n\
      \               State indices are used as file names.\n\
      \               When $(docv) is omitted, it is inferred from\n\
      \               option \"export-ts\"."
    in
    Arg.(value & vopt_str & info [ "s"; "export-states" ] ~docv:"DIR" ~doc)
  in
  let verbose =
    let doc = "Be more verbose." in
    let env = Arg.env_var "BIGVERBOSE" ~doc in
    Arg.(value & flag & info [ "v"; "verbose" ] ~doc ~env)
  in
  let nocols =
    let doc = "Disable colored output." in
    let env = Arg.env_var "BIGNOCOLORS" ~doc in
    Arg.(value & flag & info [ "n"; "no-colors" ] ~doc ~env)
  in
  let solver =
    let doc =
      "Select solver for matching engine.\n\
      \               Supported solvers are `MSAT' (MiniSAT) and `MCARD' \
       (MiniCARD)."
    in
    let env = Arg.env_var "BIGSOLVER" ~doc in
    Arg.(value & opt string "MSAT" & info [ "solver" ] ~doc ~env)
  in
  Term.(
    const copts $ consts $ debug $ ext $ graph $ lbls $ prism $ quiet
    $ states $ srew $ trew $ verbose $ nocols $ rtime $ solver)

(* Sim options *)
let sim_opts time steps seed =
  defaults.seed <- seed;
  match time with
  | Some t ->
      defaults.time_flag <- true;
      defaults.time <- t
  | None -> (
      ();
      match steps with
      | Some s ->
          defaults.steps_flag <- true;
          defaults.steps <- s
      | None -> () )

let sim_opts_t =
  let time =
    let doc =
      "Set the maximum simulation time.\n\
      \               This option is only valid for stochastic models"
    in
    Arg.(
      value
      & opt (some float) None
      & info [ "T"; "simulation-time" ] ~docv:"FLOAT" ~doc)
  in
  let steps =
    let doc =
      "Set the maximum number of simulation steps. This option is valid\n\
      \               only for deterministic and probabilistic models."
    in
    Arg.(
      value
      & opt (some int) None
      & info [ "S"; "simulation-steps" ] ~docv:"INT" ~doc)
  in
  let seed =
    let doc =
      "Initialise the pseudo-random number generator using $(docv) as seed."
    in
    Arg.(value & opt (some int) None & info [ "seed" ] ~docv:"INT" ~doc)
  in
  Term.(const sim_opts $ time $ steps $ seed)

(* Full options *)
let full_opts states = defaults.max_states <- states

let full_opts_t =
  let states =
    let doc = "Set the maximum number of states" in
    Arg.(value & opt int 1000 & info [ "M"; "max-states" ] ~docv:"INT" ~doc)
  in
  Term.(const full_opts $ states)

(* Commandline *)
let run f typ =
  check_dot ();
  if check_states () then (
    defaults.model <- f;
    typ )
  else `exit

let run_sim _copts _sopts f = run f `sim

let run_check _copts f = run f `check

let run_full _copts _fopts f = run f `full

let mdl_file = Arg.(value & pos 0 (some file) None & info [] ~docv:"FILE")

let sim_cmd =
  let doc = "Simulate a model" in
  ( Term.(const run_sim $ copts_t $ sim_opts_t $ mdl_file),
    Term.info "sim" ~doc ~exits:Term.default_exits ~man:[] )

let check_cmd =
  let doc = "Parse a model and check its validity" in
  ( Term.(const run_check $ copts_t $ mdl_file),
    Term.info "validate" ~doc ~exits:Term.default_exits ~man:[] )

let full_cmd =
  let doc = "Compute the transition system of a model" in
  ( Term.(const run_full $ copts_t $ full_opts_t $ mdl_file),
    Term.info "full" ~doc ~exits:Term.default_exits ~man:[] )

let run_default cfg =
  match cfg with
  | true ->
      eval_config Format.std_formatter ();
      `Ok `exit
  | false -> `Help (`Pager, None)

let default_cmd =
  let cfg =
    let doc = "Print a summary of your configuration" in
    Arg.(value & flag & info [ "C"; "config" ] ~doc)
  in
  let doc =
    "An implementation of Bigraphical Reactive System (BRS)\n\
    \             that supports bigraphs with sharing, stochastic reaction \
     rules,\n\
    \             rule priorities and functional rules."
  in
  ( Term.(ret (const run_default $ cfg)),
    Term.info "bigrapher" ~version:Version.version ~doc
      ~exits:Term.default_exits ~man:[] )

let cmds = [ check_cmd; sim_cmd; full_cmd ]

let parse_cmds =
  let res = Term.(eval_choice default_cmd cmds) in
  match res with
  | `Ok e -> e
  | _ ->
      Term.exit res;
      `check

(* This check is never reached *)
