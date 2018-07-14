open Format
open Utils

type error =
  | Malformed_env of string
  | Malformed_states
  | Parse of string

exception ERROR of error

type big_file = string

type path = string

type file = string

type stand_alone_opt =
  | Config
  | Help_top_level
  | Version

let string_of_stand_alone_opt = function
  | Config -> "-C, --config"
  | Help_top_level -> "-h, --help"
  | Version -> "-V, --version"

type format_op =
  | Dot
  | Svg
  | Txt
  | Json

type opt =
  | Const of Ast.const list
  | Debug
  | Decs of path
  | Ext of format_op list
  | Graph of file
  | Help
  | Labels of file
  | Max of int
  | Prism of file
  | Ml of file
  | Quiet
  | States of path option
  | Steps of int
  | Time of float
  | Verb
  | No_colors

type settings = {
  mutable consts : Ast.const list;
  mutable debug : bool;
  mutable dot_installed : bool;
  mutable export_decs : path option;
  mutable export_graph : file option;
  mutable export_ml : file option;
  mutable export_lab : file option;
  mutable export_prism : file option;
  mutable export_states : path option;
  mutable export_states_flag : bool;
  mutable help : bool;
  mutable max_states : int;
  mutable model : string option;
  mutable out_format : format_op list;
  mutable quiet : bool;
  mutable steps : int;
  mutable steps_flag : bool;
  mutable time : float;
  mutable time_flag : bool;
  mutable verb : bool;
  mutable colors : bool;
}

let default_formats = [Dot]

let defaults = {
  consts = [];
  debug = false;
  dot_installed = dot_installed ();
  export_decs = None;
  export_graph = None;
  export_ml = None;
  export_lab = None;
  export_prism = None;
  export_states = None;
  export_states_flag = false;
  help = false;
  max_states = 1000;
  model = None;
  out_format = default_formats;
  quiet = false;
  steps = 1000;
  steps_flag = false;
  time  = 1000.0;
  time_flag = false;
  verb = false;
  colors = true;
}

let colorise c msg =
  if defaults.colors
  then Utils.colorise c msg
  else msg

let string_of_opt sep ?(man = false) opt =
  (match opt with
   | Const _ ->
     (if man then
        let s = "<" ^ (colorise `underline "ASSIGNMENT ...") ^ ">" in
        ["-c " ^ s; "--const " ^ s]
      else ["-c"; "--const"])
   | Debug -> []
   | Decs _ ->
     (if man then
        let s = "<" ^ (colorise `underline "DIR") ^ ">" in
        ["-d " ^ s; "--export-decs " ^ s]
      else ["-d"; "--export-decs"])
   | Ext _ ->
     (if man then
        let s = "<" ^ (colorise `underline "FORMAT") ^ ">" in
        ["-f " ^ s; "--format " ^ s]
      else ["-f"; "--format"])
   | Graph _ ->
     (if man then
        let s = "<" ^ (colorise `underline "FILE") ^ ">" in
        ["-t " ^ s; "--export-ts " ^ s]
      else ["-t"; "--export-ts"])
   | Help -> ["-h"; "--help"]
   | Labels _ ->
     (if man then
        let s = "<" ^ (colorise `underline "FILE") ^ ">" in
        ["-l " ^ s; "--export-labels " ^ s]
      else ["-l"; "--export-labels"])
   | Max _ ->
     (if man then
        let s = "<" ^ (colorise `underline "INT") ^ ">" in
        ["-M " ^ s; "--max-states " ^ s]
      else ["-M"; "--max-states"])
   | Prism _ ->
     (if man then
        let s = "<" ^ (colorise `underline "FILE") ^ ">" in
        ["-p " ^ s; "--export-prism " ^ s]
      else ["-p"; "--export-prism"])
   | Ml _ ->
     (if man then
        let s = "<" ^ (colorise `underline "FILE") ^ ">" in
        ["-m " ^ s; "--export-ml " ^ s]
      else ["-m"; "--export-ml"])
   | Quiet -> ["-q"; "--quiet"]
   | States _ ->
     (if man then
        let s = "[" ^ (colorise `underline "DIR") ^ "]" in
        ["-s " ^ s; "--export-states " ^ s]
      else ["-s"; "--export-states"])
   | Steps _ ->
     (if man then
        let s = "[" ^ (colorise `underline "INT") ^ "]" in
        ["-S " ^ s; "--simulation-steps " ^ s]
      else ["-S"; "--simulation-steps"])
   | Time _ ->
     (if man then
        let s = "[" ^ (colorise `underline "FLOAT") ^ "]" in
        ["-T " ^ s; "--simulation-time " ^ s]
      else  ["-T"; "--simulation-time"])
   | Verb -> ["-v"; "--verbose"]
   | No_colors -> ["-n"; "--no-colors"])
  |> String.concat sep

let report_error_aux fmt = function
  | Malformed_env s ->
    fprintf fmt "@[`%s' is not a valid format@]" s
  | Malformed_states ->
    fprintf fmt "@[<hov -16>Specify a path for@ option@ `%s'@ or@ \
                 use@ it@ in@ conjunction@ with@ option@ `%s'@]"
      (string_of_opt "|" (States None))
      (string_of_opt "|" (Graph ""))
  | Parse s ->
    fprintf fmt "@[Invalid argument `%s'@]" s

type t =
  | Check
  | Full
  | Sim
  | StandAloneOpt of stand_alone_opt

type cmd_t =
  [ `check | `full | `sim ]

let string_of_t = function
  | Check -> "validate"
  | Full -> "full"
  | Sim -> "sim"
  | StandAloneOpt x -> string_of_stand_alone_opt x

(* Update defaults with environment variables *)
let eval_env () =
  let parse_format s =
    Str.split (Str.regexp_string ",") s
    |> List.map (function
        | "svg" -> Svg
        | "dot" -> Dot
        (*   | "json" -> Json *)
        | "txt" -> Txt
        | s -> raise (ERROR (Malformed_env s))) in
  (try
     defaults.verb <- (ignore (Sys.getenv "BIGVERBOSE"); true);
   with
   | Not_found -> ());
  (try
     defaults.quiet <- (ignore (Sys.getenv "BIGQUIET"); true);
   with
   | Not_found -> ());
  (try
     defaults.colors <- (ignore (Sys.getenv "BIGNOCOLORS"); false);
   with
   | Not_found -> ());
  (try
     defaults.out_format <-
       parse_format (Sys.getenv "BIGFORMAT")
   with
   | Not_found -> ())

(* Stand alone options *)
let msg_so_opt fmt = function
  | Config -> fprintf fmt "@[<hov>Print a summary of your configuration.@]"
  | Help_top_level -> fprintf fmt "@[<hov>Show this help.@]"
  | Version -> fprintf fmt "@[<hov>Show version information.@]"

let msg_opt fmt = function
  | Const _ -> fprintf fmt "@[<hov>Specify a comma-separated list of@ \
                            variable@ assignments.@,\
                            Example: `x=4,t=.56'.@]"
  | Debug -> assert false (* Undocumented *) (*BISECT-IGNORE*)
  | Decs _ -> fprintf fmt "@[<hov>Export each declaration@ in@ the@ model@ \
                           (bigraphs@ and@ reaction@ rules)@ to@ a distinct \
                           file@ in@ %s.@ Dummy@ values@ are@ used@ to@ \
                           instantiate@ functional@ values.@]"
                (colorise `underline "DIR")
  | Ext _ ->  fprintf fmt "@[<hov>Specify a comma-separated list@ of@ output@ \
                           formats@ for@ options@ `%s',@ `%s'@ and@ `%s'.@ \
                           Supported@ formats@ are@ `dot',@ `json',@ `svg'@ and@ \
                           `txt'.@ This@ is@ equivalent@ to@ setting@ \
                           $BIGFORMAT@ to@ %s.@]"
                (string_of_opt "|" (Decs ""))
                (string_of_opt "|" (Graph ""))
                (string_of_opt "|" (States None))
                (colorise `underline "FORMAT")
  | Graph _ -> fprintf fmt "@[<hov>Export the transition system to %s.@]"
                 (colorise `underline "FILE")
  | Help -> fprintf fmt "@[<hov>Show this help.@]"
  | Labels _ -> fprintf fmt "@[<hov>Export the labelling function in PRISM \
                             csl@ format@ to@ %s.@]"
                  (colorise `underline "FILE")
  | Max _ -> fprintf fmt "@[<hov>Set the maximum number of states.@]"
  | No_colors -> fprintf fmt "@[<hov>Disable colored output.@ This is@ \
                              equivalent to@ setting@ $BIGNOCOLORS@ to@ a@ \
                              non-empty@ value.@]"
  | Prism _ -> fprintf fmt "@[<hov>Export the transition system in@ PRISM@ \
                            tra@ format@ to@ %s.@]"
                 (colorise `underline "FILE")
  | Ml _ -> fprintf fmt "@[<hov>Export the model in@ OCaml@ format@ \
                         to@ %s.@]" (colorise `underline "FILE")
  | Quiet -> fprintf fmt "@[<hov>Disable progress indicator.@ This is@ \
                          equivalent to@ setting@ $BIGQUIET@ to a@ non-empty@ \
                          value.@]"
  | States _ -> fprintf fmt "@[<hov>Export each state@ to@ a file@ in %s.@ \
                             State@ indices@ are@ used@ as@ file@ names.@ \
                             When %s@ is@ omitted,@ it@ is@ inferred@ from@ \
                             option `%s'.@]"
                  (colorise `underline "DIR")
                  (colorise `underline "DIR")
                  (string_of_opt "|" (Graph ""))
  | Steps _ -> fprintf fmt "@[<hov>Set the maximum number of simulation steps.@ This option@ is@ \
                            valid@ only@ for@ deterministic@ and@ probabilistic@ models.@]"
  | Time _ -> fprintf fmt "@[<hov>Set the maximum simulation time.@ This option@ is@ valid@ \
                           only@ for@ stochastic@ models.@]"
  | Verb -> fprintf fmt "@[<hov>Be more verbose.@ This is@ equivalent to@ setting@ \
                         $BIGVERBOSE@ to@ a@ non-empty@ value.@]"

let msg_cmd fmt = function
  | Check -> fprintf fmt "@[<hov>Parse a model and check its validity.@]"
  | Full -> fprintf fmt "@[<hov>Compute the transition system of a model.@]"
  | Sim -> fprintf fmt "@[<hov>Simulate a model.@]"
  | StandAloneOpt x -> msg_so_opt fmt x

let usage_str fmt () =
  fprintf fmt "@[<v 2>USAGE:@,\
               bigrapher <OPTION>@,\
               bigrapher <COMMAND> <ARGS>@]@,"

let print_table fmt rows ?(offset = 0) f_l f_r =
  (* Find longest row *)
  let l_max =
    List.fold_left (fun max r ->
        let l = String.length (f_l r) in
        if l > max then l else max)
      0 rows in
  let pp_row fmt row =
    pp_print_tab fmt ();
    pp_print_string fmt (f_l row);
    pp_print_tab fmt ();
    fprintf fmt "%a" f_r row in
  match rows with
  | first :: rows ->
    (pp_open_tbox fmt ();
     pp_set_tab fmt ();
     let l = f_l first in
     fprintf fmt "@[<h>%s" l;
     pp_print_break fmt (l_max - offset) 0;
     fprintf fmt "@]";
     pp_set_tab fmt ();
     fprintf fmt "%a" f_r first;
     List.iter (pp_row fmt) rows;
     pp_close_tbox fmt ())
  | _ -> assert false (* Assumed always non-empty *) (*BISECT-IGNORE*)

let eval_help_top fmt () =
  let commands fmt () =
    print_table fmt [ Full; Sim; Check ]
      ~offset:(-3)
      string_of_t
      msg_cmd
  and opts fmt () =
    print_table fmt [ Config;
                      Help_top_level;
                      Version ]
      ~offset:6
      string_of_stand_alone_opt
      msg_so_opt in
  fprintf fmt "@[<v>%a@,\
               @[<v 2>COMMANDS:@,%a@]@,@,\
               @[<v 2>OPTIONS:@,%a@]@,@,\
               See `bigrapher <COMMAND> -h' for more information on a specific \
               subcommand.@]@."
    usage_str () commands () opts ()

let help_fun fmt l cmd =
  let print_man fmt l =
    let n = (List.length l) - 1 in
    List.iteri (fun i row ->
        fprintf fmt "@[<v 2>@[%s@]@,@[%a@]@]"
          (string_of_opt ", " ~man:true row)
          msg_opt row;
        if i < n then
          pp_print_cut fmt ())
      l in
  fprintf fmt "@[<v 2>USAGE:@,\
               bigrapher %s [OPTIONS] <MODEL.big>@]@.@."
    cmd;
  fprintf fmt "@[<v 2>OPTIONS:@,%a@]@."
    print_man l;
  exit 0

let eval_help_check fmt () =
  let opt_chk = [ Const [];
                  Decs "";
                  Ext [];
                  Help;
                  Ml "";
                  No_colors;
                  Quiet;
                  Verb ] in
  help_fun fmt opt_chk "validate"

let eval_help_full fmt () =
  let opt_full = [ Const [];
                   Decs "";
                   Ext [];
                   Help;
                   Labels "";
                   Ml "";
                   Max 0;
                   No_colors;
                   Prism "";
                   Quiet;
                   States None;
                   Graph "";
                   Verb ] in
  help_fun fmt opt_full "full"

let eval_help_sim fmt () =
  let opt_sim  = [ Const [];
                   Decs "";
                   Ext [];
                   Help;
                   Labels "";
                   Ml "";
                   No_colors;
                   Prism "";
                   Quiet;
                   States None;
                   Steps 0;
                   Graph "";
                   Time 0.0;
                   Verb ] in
  help_fun fmt opt_sim "sim"

let eval_version fmt () =
  fprintf fmt "@[%s@]@." Version.version

let string_of_format f =
  List.map (function
      | Svg -> "svg"
      | Json -> "json"
      | Dot -> "dot"
      | Txt -> "txt") f
  |> String.concat ","

let string_of_file = function
  | None -> "-" (* Not set *)
  | Some f -> f

let eval_config fmt () =
  let config_str fmt () =
    let conf =
      [("colors",
        fun fmt () ->
          fprintf fmt "@[<hov>%b@]" defaults.colors);
       ("consts",
        fun fmt () ->
          fprintf fmt "@[<hov>%s@]"
            (match Ast.string_of_consts defaults.consts with
             | "" -> "-"
             | s -> s));
       ("debug",
        fun fmt () ->
          fprintf fmt "@[<hov>%b@]" defaults.debug);
       ("export_decs",
        fun fmt () ->
          fprintf fmt "@[<hov>%s@]" (string_of_file defaults.export_decs));
       ("export_graph",
        fun fmt () ->
          fprintf fmt "@[<hov>%s@]" (string_of_file defaults.export_graph));
       ("export_lab",
        fun fmt () ->
          fprintf fmt "@[<hov>%s@]" (string_of_file defaults.export_lab));
       ("export_ml",
        fun fmt () ->
          fprintf fmt "@[<hov>%s@]" (string_of_file defaults.export_ml));
       ("export_prism",
        fun fmt () ->
          fprintf fmt "@[<hov>%s@]" (string_of_file defaults.export_prism));
       ("export_states",
        fun fmt () ->
          fprintf fmt "@[<hov>%s@]" (string_of_file defaults.export_states));
       ("export_states_flag",
        fun fmt () ->
          fprintf fmt "@[<hov>%b@]" defaults.export_states_flag);
       ("help",
        fun fmt () ->
          fprintf fmt "@[<hov>%b@]" defaults.help);
       ("max_states",
        fun fmt () ->
          fprintf fmt "@[<hov>%d@]" defaults.max_states);
       ("out_format",
        fun fmt () ->
          fprintf fmt "@[<hov>%s@]" (string_of_format defaults.out_format));
       ("quiet",
        fun fmt () ->
          fprintf fmt "@[<hov>%b@]" defaults.quiet);
       ("steps",
        fun fmt () ->
          fprintf fmt "@[<hov>%d@]" defaults.steps);
       ("time",
        fun fmt () ->
          fprintf fmt "@[<hov>%g@]" defaults.time);
       ("verb",
        fun fmt () ->
          fprintf fmt "@[<hov>%b@]" defaults.verb)] in
    print_table fmt conf
      (fun (x, _) -> x) (fun fmt (_, f) -> f fmt ()) in
  fprintf fmt "@[<v 2>CONFIGURATION:@,%a@]@." config_str ()

let dot_msg = "`dot' is not installed on this system. "

let report_warning fmt msg opt =
  fprintf fmt "@[<v>%s: %sIgnoring option `%s'@]@."
    (warn_opt defaults.colors) msg opt

let usage fmt () =
  fprintf fmt "@[<v>%a@,@[Try `bigrapher --help' for more information.@]@]"
    usage_str ()

let usage_sub fmt cmd =
  fprintf fmt "@[<v>USAGE: bigrapher %s [OPTIONS] <MODEL.big>@,\
               Try `bigrapher %s --help' for more information.@]@." cmd cmd

let report_error fmt e =
  fprintf fmt "@[%s: %a@]@." (err_opt defaults.colors) report_error_aux e

let check_states () =
  if defaults.export_states_flag then
    match (defaults.export_states, defaults.export_graph) with
    | (None, Some f) -> defaults.export_states <- Some (Filename.dirname f)
    | (None, None) -> raise (ERROR Malformed_states)
    | (Some _, None )
    | (Some _, Some _) -> ()

let check_dot () =
  if not defaults.dot_installed then
    if List.mem Svg defaults.out_format then
      (* reset flag *)
      begin
        defaults.out_format <- default_formats;
        report_warning err_formatter
          dot_msg
          (string_of_opt "|" (Ext []))
      end

let check_brs_opt () =
  if defaults.time_flag then
    report_warning err_formatter
      ""
      (string_of_opt "|" (Time 0.0))

let check_sbrs_opt () =
  if defaults.steps_flag then
    report_warning err_formatter
      ""
      (string_of_opt "|" (Steps 0))

let check_pbrs_opt = check_brs_opt

let check_nbrs_opt = check_brs_opt