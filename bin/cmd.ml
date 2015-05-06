open Format
open Utils

type error =
  | Malformed_env of string
  | Malformed_states
  | Parse of string
	       
exception ERROR of error

let report_error_aux fmt = function
  | Malformed_env s ->
     fprintf fmt "`%s'@ is@ not@ a valid@ format" s
  | Malformed_states ->
     fprintf fmt "Specify a@ path@ or for option `-s|--export-states'@ \
		  or@ use it@ in conjunction@ with option@ \
		  `-t|--export-transition-system'"			
  | Parse s ->
     fprintf fmt "Syntax error near token `%s'" s 
		     
type big_file = string

type bilog_file = string option		  

type path = string
	      
type file = string
	      	    
type stand_alone_opt =
  | Config
  | Help_top_level
  | Version

let string_of_stand_alone_opt = function
  | Config -> "-c, --config"
  | Help_top_level -> "-h, --help"
  | Version -> "-V, --version"
      
type format_op =
  | Dot
  | Svg
		            
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
  | Quiet
  | States of path option
  | Steps of int
  | Time of float
  | Verb
      
let string_of_opt sep opt =
  (match opt with
   | Const _ -> ["-c"; "--const"]
   | Debug -> []
   | Decs _ -> ["-d"; "--export-decs"]
   | Ext _ -> ["-f"; "--format"]
   | Graph _ -> ["-t"; "--export-transition-system"]
   | Help -> ["-h"; "--help"]
   | Labels _ -> ["-l"; "--export-labels"]
   | Max _ -> ["-M"; "--max-states"]
   | Prism _ -> ["-p"; "--export-prism"]
   | Quiet -> ["-q"; "--quiet"]
   | States _ -> ["-s"; "--export-states"]
   | Steps _ -> ["-S"; "--simulation-steps"]
   | Time _ -> ["-T"; "--simulation-time"]
   | Verb -> ["-v"; "--verbose"])
  |> String.concat sep
      
type t =
  | Check of opt list * big_file * bilog_file
  | Full of opt list * big_file * bilog_file
  | Sim of opt list * big_file * bilog_file
  | StandAloneOpt of stand_alone_opt

type cmd_t =
  [ `check | `full | `sim ]
		       
let string_of_t = function
  | Check (_, _, _) -> "validate"
  | Full (_, _, _) -> "full"
  | Sim (_, _, _) -> "sim"
  | StandAloneOpt x -> string_of_stand_alone_opt x
  
type settings = {
    mutable consts : Ast.const list;
    mutable debug : bool;
    mutable export_decs : path option;
    mutable export_graph : file option;
    mutable export_lab : file option;
    mutable export_prism : file option;
    mutable export_states : path option;
    mutable export_states_flag : bool;
    mutable help : bool;
    mutable max_states : int;
    mutable model : string;
    mutable out_format : format_op list;
    mutable pred : string option;
    mutable quiet : bool;
    mutable steps : int;
    mutable time : float;
    mutable verb : bool;
  }

let defaults = {
    consts = [];
    debug = false;
    export_decs = None;
    export_graph = None;
    export_lab = None;
    export_prism = None;
    export_states = None;
    export_states_flag = false;
    help = false;
    max_states = 1000;
    model = "";
    out_format = [Svg];
    pred = None;
    quiet = false;
    steps = 1000;
    time  = 1000.0;
    verb = false;
  }
		 
(* Update defaults with environment variables *)    		 
let eval_env () =
  let parse_format s =
    Str.split (Str.regexp_string ",") s
    |> List.map (function
		  | "svg" -> Svg
		  | "dot" -> Dot
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
      defaults.out_format <-
	parse_format (Sys.getenv "BIGFORMAT")
    with
    | Not_found -> ())

(* Update defaults with command line options *)    
let eval =
  List.iter (function
	      | Const l -> defaults.consts <- l		
	      | Debug -> defaults.debug <- true
	      | Decs p -> defaults.export_decs <- Some p
	      | Ext l -> defaults.out_format <- l
	      | Graph f -> defaults.export_graph <- Some f
	      | Help -> defaults.help <- true
	      | Labels f -> defaults.export_lab <- Some f
	      | Max x -> defaults.max_states <- x
	      | Prism f -> defaults.export_prism <- Some f
	      | Quiet -> defaults.quiet <- true
	      | States p -> defaults.export_states <- p
	      | Steps x -> defaults.steps <- x
	      | Time t -> defaults.time <- t
	      | Verb -> defaults.verb <- true)

(* Stand alone options *)
let msg_so_opt fmt = function
  | Config -> fprintf fmt "@[<hov>Print a summary of your configuration.@]"
  | Help_top_level -> fprintf fmt "@[<hov>Show this help.@]"
  | Version -> fprintf fmt "@[<hov>Show version information.@]"

let msg_opt fmt = function
  | Const _ -> fprintf fmt "@[<hov>Specify a comma-separated list of variable assignments.@]"
  | Debug ->   fprintf fmt "" (* Undocumented *)
  | Decs _ -> fprintf fmt "@[<hov>Export each declaration@ in@ the@ model@ to@ a file@ in@ \
		        	  %s.@ Dummy@ values@ are@ used@ to@ instantiate@ functional@ values.@]"
		      (colorise `underline "DIR")
  | Ext _ ->  fprintf fmt "@[<hov>Specify a comma-separated list@ of@ output@ formats@ for@ options@ \
			          %s,@ %s@ and@ %s.@ This@ is@ equivalent@ to@ setting@ \
		                  %s@ to@ a@ non-empty@ value.@]"
		      (colorise `bold "`-t|--export-transition-system'")
		      (colorise `bold "`-s|--export-states'")
		      (colorise `bold "`-d|--export-decs'")
		      (colorise `bold "BIGFORMAT")
  | Graph _ -> fprintf fmt "@[<hov>Export the transition@ system@ to@ %s.@]"
		       (colorise `underline "FILENAME")
  | Help -> fprintf fmt "@[<hov>Show this help.@]"
  | Labels _ -> fprintf fmt "@[<hov>Export the labelling@ function@ in@ PRISM@ csl@ format@ \
		                    to@ %s.@]" (colorise `underline "FILENAME")
  | Max _ -> fprintf fmt "@[<hov>Set the maximum number of states.@]"
  | Prism _ -> fprintf fmt "@[<hov>Export the transition@ system@ in@ PRISM@ tra@ format@ \
			           to@ %s.@]" (colorise `underline "FILENAME")
  | Quiet -> fprintf fmt "@[<hov>Disable progress indicator.@ This is@ equivalent to@ setting@ \
			         %s@ to a@ non-empty@ value.@]" (colorise `bold "$BIGQUIET")
  | States _ -> fprintf fmt "@[<hov>Export each state@ to@ a file@ in %s.@ \
			            State@ indices@ are@ used@ as@ file names.@ \
			            When %s@ is@ omitted,@ it@ is@ inferred@ from option %s.]"
			(colorise `underline "DIR")
			(colorise `underline "DIR")
			(colorise `bold "\'-t|--export-transition-system\'")
  | Steps _ -> fprintf fmt "@[<hov>Set the maximum number of simulation steps.@ This@ option@ is@ \
                                  valid@ only@ for@ deterministic@ models.@]"
  | Time _ -> fprintf fmt "@[<hov>Set the maximum simulation time.@ This@ option@ is@ valid@ \
			   only@ for@ stochstic@ models.@]"
  | Verb -> fprintf fmt "@[<hov>Be more verbose.@ This is@ equivalent to@ setting@ \
			        %s@ to a@ non-empty@ value.@]" (colorise `bold "$BIGVERBOSE")

let msg_cmd fmt = function
  | Check (_, _, _) -> fprintf fmt "@[<hov>Parse a model and check its validity.@]"
  | Full (_, _, _) -> fprintf fmt "@[<hov>Compute the transition system of a model.@]"
  | Sim (_, _, _) -> fprintf fmt "@[<hov>Simulate a model.@]"
  | StandAloneOpt x -> msg_so_opt fmt x

let usage_str fmt () =  
  fprintf fmt "@[USAGE: @[<v>bigrapher <OPTION>@,\
	                     bigrapher <COMMAND> <ARGS> @]@]@,"

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
      print_break (l_max - offset) 0;
      fprintf fmt "@]";
      pp_set_tab fmt ();
      fprintf fmt "%a" f_r first;
      List.iter (pp_row fmt) rows;
      pp_close_tbox fmt ())
  | _ -> assert false (* Assumed always non-empty *)
    
let eval_help_top fmt () =
  let commands fmt () =
    print_table fmt [ Check ([], "", None);
		      Full ([], "", None);
		      Sim ([], "", None) ]
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
  fprintf fmt "@[<v>%a@,@[<v 2>COMMANDS:@,%a@]@,\
	       @,@[<v 2>OPTIONS:@,%a@]@,\
	       @,See `bigrapher <COMMAND> -h' for more information on a specific subcommand.@]@."
	  usage_str () commands () opts ();
  exit 0

let help_fun fmt l cmd =
  let options fmt () =
    print_table fmt l (string_of_opt ", ") msg_opt in
  fprintf fmt "@[<v>USAGE: bigrapher %s [OPTIONS] <MODEL.big> [PRED.bilog]@,\
	       @[<v 2>OPTIONS:@,%a@]@]" cmd options ();
  exit 0

let opt_chk = [ Const []; Debug; Decs ""; Ext []; Help; Quiet; Verb ]
       
let eval_help_check fmt () =
  help_fun fmt opt_chk "validate"

let opt_full = [ Const []; Debug; Decs ""; Ext []; Graph ""; Help;
		 Labels ""; Max 0; Prism ""; Quiet; States None;
		 Verb ]
	   
let eval_help_full fmt () =
  help_fun fmt opt_full "full"

let opt_sim  = [ Const []; Debug; Decs ""; Ext []; Graph ""; Help;
		 Labels ""; Prism ""; Quiet; States None;
		 Steps 0; Time 0.0; Verb ]
		 
let eval_help_sim fmt () =
  help_fun fmt opt_sim "sim"

let eval_version fmt () =
  fprintf fmt "@[%s@]@." Version.version;
  exit 0

let string_of_format f =
  List.map (function
	     | Svg -> "svg"
	     | Dot -> "dot") f
  |> String.concat ","

let string_of_file = function
  | None -> "-"
  | Some f -> f
		
let eval_config fmt () =
  let config_str fmt () =
    let conf =
      [("consts",
	fun fmt () ->
	fprintf fmt "@[<hov>%s@]" (match Ast.string_of_consts defaults.consts with
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
       ("export_prism",
	fun fmt () ->
	fprintf fmt "@[<hov>%s@]" (string_of_file defaults.export_lab));
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
  fprintf fmt "@[<v>CONFIGURATION:@,%a@]@." config_str ();
  exit 0
	    
let dot = dot_installed ()

let dot_msg = "`dot' is not installed on this system."
			
let report_warning fmt msg opt =
  fprintf fmt "@[<v>%s: %s@,Ignoring option `%s'@]@."
	  warn msg opt
	  
let usage fmt () =
  fprintf fmt "@[<v>%a@,@[Try `bigrapher --help' for more information.@]@]" usage_str ()

let usage_sub fmt cmd =
  fprintf fmt "@[<v>USAGE: bigrapher %s [OPTIONS] <MODEL.big> [PRED.bilog]@,\
	       Try `bigrapher %s --help' for more information.@]@." cmd cmd
	  
let report_error fmt e =
  fprintf fmt "@[<v>%s: %a@]@." err report_error_aux e
	  
let eval_chk fmt options model pred =
  (* Update defaults *)
  List.iter (function
	      | Const l -> defaults.consts <- l
	      | Debug -> defaults.debug <- true
	      | Decs f -> defaults.export_decs <- Some f
	      | Ext l -> defaults.out_format <- l 
	      | Help -> eval_help_check fmt ()
	      | Quiet -> defaults.quiet <- true
	      | Verb -> defaults.verb <-true
	      | o ->
		 (report_warning fmt "" (string_of_opt "|" o);
		  usage_sub fmt "validate"))
	    options;
  defaults.model <- model;
  defaults.pred <- pred

let front_graph l =
  let (a, b) = List.partition (function
				| Graph _ -> true
				|_ -> false)
			      l in
  a @ b
		     
let eval_full fmt options model pred =
  front_graph options
  |> List.iter (function
	      | Const l -> defaults.consts <- l
	      | Debug -> defaults.debug <- true
	      | Decs f -> defaults.export_decs <- Some f
	      | Ext l -> defaults.out_format <- l 
	      | Graph f -> defaults.export_graph <- Some f
	      | Help -> eval_help_full fmt ()
	      | Labels f -> defaults.export_lab <- Some f
	      | Max n -> defaults.max_states <- n
	      | Prism f -> defaults.export_prism <- Some f
	      | Quiet -> defaults.quiet <- true
	      | States None ->
		 (defaults.export_states_flag <- true;
		  match defaults.export_graph with
		  | None -> (report_error fmt (Malformed_states);
			     usage_sub fmt "full";
			     exit 1)
		  | f -> defaults.export_states <- f)
	      | States f ->
		 (defaults.export_states_flag <- true;
		  defaults.export_states <- f)
	      | Verb -> defaults.verb <- true
	      | o ->
		 (report_warning fmt "" (string_of_opt "|" o);
		  usage_sub fmt "full"));
  defaults.model <- model;
  defaults.pred <- pred

let eval_sim fmt options model pred =
  front_graph options
  |> List.iter (function
	      | Const l -> defaults.consts <- l
	      | Debug -> defaults.debug <- true
	      | Decs f -> defaults.export_decs <- Some f
	      | Ext l -> defaults.out_format <- l 
	      | Graph f -> defaults.export_graph <- Some f
	      | Help -> eval_help_full fmt ()
	      | Labels f -> defaults.export_lab <- Some f
	      | Prism f -> defaults.export_prism <- Some f
	      | Quiet -> defaults.quiet <- true
	      | States None ->
		 (defaults.export_states_flag <- true;
		  match defaults.export_graph with
		  | None -> (report_error fmt (Malformed_states);
			     usage_sub fmt "sim";
			    exit 1)
		  | f -> defaults.export_states <- f)
	      | States f ->
		 (defaults.export_states_flag <- true;
		  defaults.export_states <- f)
	      | Verb -> defaults.verb <- true					   
	      | Steps s -> defaults.max_states <- s
	      | Time t -> defaults.time <- t
	      | o ->
		 (report_warning fmt "" (string_of_opt "|" o);
		  usage_sub fmt "sim"));
  defaults.model <- model;
  defaults.pred <- pred
