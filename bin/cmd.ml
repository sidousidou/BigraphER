open Format
open Utils
open Version

type error =
  | Malformed_env of string
  | Parse
	       
exception ERROR of error

let report_error_aux tok = function
  | Malformed_env s ->
     "String `" ^ s ^ "' is not a valid format"
  | Parse -> "" 
		     
type big_file = string

type bilog_file = string option		  

type path = string
	      
type file = string
	      	    
type stand_alone_opt =
  | Config
  | Help_top_level
  | Version
		    
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
    
type t =
  | Check of opt list * big_file * bilog_file
  | Full of opt list * big_file * bilog_file
  | Sim of opt list * big_file * bilog_file
  | StandAloneOpt of stand_alone_opt
	       
type settings = {
    mutable consts : Ast.const list;
    mutable debug : bool;
    mutable export_decs : path option;
    mutable export_graph : file option;
    mutable export_lab : file option;
    mutable export_prism : file option;
    mutable export_states : bool;
    mutable help : bool;
    mutable max_states : int;
    mutable out_format : format_op list option;
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
    export_states = false;
    help = false;
    max_states = 1000;
    out_format = Some [Svg];
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
	(match parse_format (Sys.getenv "BIGFORMAT") with
	 | [] -> None
	 | l -> Some l);
    with
    | Not_found -> ())

(* Update defaults with command line options *)    
let eval =
  List.iter (function
	      | Const l -> defaults.consts <- l		
	      | Debug -> defaults.debug <- true
	      | Decs p -> defaults.export_decs <- Some p
	      | Ext l -> defaults.out_format <- Some l
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
		      (colorise `underline "PATH")
  | Ext _ ->  fprintf fmt "@[<hov>Specify a comma-separated list@ of@ output@ formats@ for@ options@ \
			          %s,@ %s@ and@ %s.@ This@ is@ equivalent@ to@ setting@ \
		                  %s@ to@ a@ non-empty@ value.@]"
		      (colorise `bold "\'--export-transition-system\' | \'-t\'")
		      (colorise `bold "\'--export-states\' | \'-s\'")
		      (colorise `bold "\'--export-decs\' | \'-d\'")
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
			            State@ indices@ are@ used@ as@ file names.@]"
			(colorise `underline "PATH")
  | Steps _ -> fprintf fmt "@[<hov>Set the maximum number of simulation steps.@ This@ option@ is@ \
                                  valid@ only@ for@ deterministic@ models.@]"
  | Time _ -> fprintf fmt "@[<hov>Set the maximum simulation time.@ This@ option@ is@ valid@ \ 
			          only@ for@ stochstic@ models.@]"
  | Verb -> fprintf fmt "@[<hov>Be more verbose.@ This is@ equivalent to@ setting@ \
			        %s@ to a@ non-empty@ value.@]" (colorise `bold "$BIGVERBOSE")

let msg_opt fmt = function
  | Check (_, _, _) -> fprintf fmt "@[<hov>Parse a model and check its validity.@]"
  | Full (_, _, _) -> fprintf fmt "@[<hov>Compute the transition system of a model.@]"
  | Sim (_, _, _) -> fprintf fmt "@[<hov>Simulate a model.@]"
  | StandAloneOpt x -> msg_so_opt fmt x



(*Top level screen
usage
options
*)


(* subcommand check *)

(* subcommand full*)

(* subcommand sim *)
				  
				  
let options_str fmt () =
  let l =
    [ `consts; `out_raw; `out_states; `out_dot; `help; `out_store; `out_csl; `s_max; 
      `out_prism; `sim; `verbose; `version ]
  and flag_str a = String.concat ", " (flags a) in	    
  let pp_row fmt = function
    | `consts as a-> (* First row *)
       (pp_set_tab fmt ();
	fprintf fmt "@<28>%s" ((flag_str a) ^ " <x=val,...>");
	pp_set_tab fmt ();
	(* I don't understand why I need these spaces for a correct alignment *)
	fprintf fmt "    %a" msg a)
    | `debug -> fprintf fmt ""
    | `version as a ->
       (pp_print_tab fmt ();
	fprintf fmt "%s" (flag_str a);
	pp_print_tab fmt ();
	fprintf fmt "%a" msg a) 
    | `sim as a ->
       (pp_print_tab fmt ();
	fprintf fmt "%s <float>" (flag_str a);
	pp_print_tab fmt ();
	fprintf fmt "%a"  msg a) 
    | `s_max as a ->
       (pp_print_tab fmt ();
	fprintf fmt "%s [int]" (flag_str a);
	pp_print_tab fmt ();
	fprintf fmt "%a" msg a)
    | `out_csl
    | `out_dot
    | `out_raw
    | `out_prism as a ->
       (pp_print_tab fmt ();
	fprintf fmt "%s [file]" (flag_str a);
	pp_print_tab fmt ();
	fprintf fmt "%a"  msg a)
    | `out_store as a ->
       (pp_print_tab fmt ();
	fprintf fmt "%s [dir]" (flag_str a);
	pp_print_tab fmt ();
	fprintf fmt "%a" msg a)
    | `help
    | `verbose
    | `out_states as a->
       (pp_print_tab fmt ();
	fprintf fmt "%s" (flag_str a);
	pp_print_tab fmt ();
	fprintf fmt "%a" msg a) in
  pp_open_tbox fmt ();
  List.iter (pp_row fmt) l;
  pp_close_tbox fmt ()


let config_str fmt () =
  let print_row l r () =
    pp_print_tab fmt ();
    pp_print_string fmt l;
    pp_print_tab fmt ();
    pp_print_string fmt r in
  pp_open_tbox fmt ();
  pp_set_tab fmt ();
  pp_print_string fmt "consts";
  pp_set_tab fmt ();
  pp_print_string fmt (string_of_consts (defaults.consts));




     debug = false;
    export_decs = None;
    export_graph = None;
    export_lab = None;
    export_prism = None;
    export_states = false;
    help = false;
    max_states = 1000;
    out_format = Some [Svg];
    quiet = false;
    steps = 1000;
    time  = 1000.0;
    verb = false;  

    

  
  pp_close_tbox fmt ()
		
let usage_str fmt () =  
  fprintf fmt "@[USAGE: bigrapher @[<v>[--version]@,\
	       [--help|-h]@,<model.big> [predicates.bilog] [options]@]@]"

let help fmt () =
  fprintf fmt "@[<v>%a@,@[<v 2>OPTIONS:@,%a@]@]@." usage_str () options_str ();
  exit 0

let config fmt () =
  fprintf fmt "@[<v>%a@]@." config_str ();
  exit 0
       
let eval_stand_alone fmt = function
  | Config -> config std_formatter ()
  | Help_top_level -> help std_formatter ()
  | Version -> fprintf std_formatter "@[%s@]@." version; exit 0



	    
let dot = dot_installed ()

		

	       
let report_warning_dot fmt a =
  fprintf fmt "@[%s: @[`dot' is not installed on this system.@ Ignoring option `%s'@]@]@."
	  warn a

	  
let usage fmt () =
  fprintf fmt "@[<v>%a@,@[Try `bigrapher --help' for more information.@]@]" usage_str ()

let report_error fmt e =
  fprintf fmt "@[<v>%s: %s@,%a@]@." err (report_error_aux e) usage ()




		 	  
let flags = function
  | `debug ->      ["--debug"]
  | `verbose ->    ["-v"; "--verbose"]
  | `sim ->        ["-s"; "--simulation"]
  | `s_max ->      ["-m"; "--max-states"]
  | `version ->    ["-V"; "--version"]
  | `consts ->     ["-c"; "--consts"]
  | `out_csl ->    ["-l"; "--export-labels"]
  | `out_raw ->    ["-d"; "--export-dot"]
  | `out_dot ->    ["-g"; "--export-svg"]    (* g for svG         *)
  | `out_states -> ["-f"; "--export-states"] (* f stands for Full *)
  | `out_prism ->  ["-p"; "--export-prism"]
  | `out_store ->  ["-i"; "--export-store"]  (* i stands for Info *)
  | `help ->       ["-h"; "--help"] 

		     		   		     
let is_option s =
  (String.length s >= 1) && (s.[0] = '-') 

let parse_option s =
  if is_option s then 
    match s with
    | "--debug" ->                `debug
    | "-v" | "--verbose" ->       `verbose
    | "-s" | "--simulation" ->    `sim
    | "-m" | "--max-states" ->    `s_max
    | "-V" | "--version" ->       `version
    | "-c" | "--consts" ->        `consts
    | "-l" | "--export-labels" -> `out_csl 
    | "-d" | "--export-dot" ->    `out_raw
    | "-g" | "--export-svg" ->    `out_dot
    | "-f" | "--export-states" -> `out_states
    | "-p" | "--export-prism" ->  `out_prism
    | "-i" | "--export-store" ->  `out_store
    | "-h" | "--help" ->          `help
    | _ ->                         raise (ERROR (Unknown_option s))
  else raise (ERROR (Not_option s))

	  
