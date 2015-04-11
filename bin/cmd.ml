open Format
open Utils
open Version

type arg = 
  [ `debug
  | `verbose 
  | `sim 
  | `s_max
  | `version 
  | `consts
  | `out_csl
  | `out_dot
  | `out_states
  | `out_prism
  | `out_store
  | `help ]

type error = 
  | Unknown_option of string
  | Not_option of string
  | Malformed_option of arg
  | Model_missing
  | Not_big of string

exception ERROR of error

type env = {
    mutable debug : bool;
    mutable verbose : bool;
    mutable sim : bool;
    mutable t_max : float;
    mutable s_max : int;
    mutable model: string option;
    mutable bilog : string option;
    mutable consts : Ast.const list;
    mutable out_csl : string option;
    mutable out_dot : string option;
    mutable out_states : bool;
    mutable out_prism : string option;
    mutable out_store : string option;
  }

let defaults = {
    debug = false;
    verbose = false;
    sim = false;
    t_max = 10000.0;
    s_max = 1000;
    model = None;
    bilog = None;
    consts = [];
    out_csl = None;
    out_dot = None;
    out_states = false;
    out_prism = None;
    out_store = None;
  }

let to_string = function
  | None -> ""
  | Some s -> s
		 
let msg fmt = function
  | `debug ->      fprintf fmt ""
  | `verbose ->    fprintf fmt "@[Be more verbose.@]"
  | `sim ->        fprintf fmt "@[Simulate the model.@ The optional@ argument sets@ the maximum@ simulation@ time.@]"  
  | `s_max ->      fprintf fmt "@[Set the maximum@ number of states.@]"
  | `version ->    fprintf fmt "@[Show version@ information.@]"
  | `consts ->     fprintf fmt "@[Specify a list@ of constants.@]"  
  | `out_csl ->    fprintf fmt "@[Export the labelling@ function to PRISM csl format.@]"
  | `out_dot ->    fprintf fmt "@[Export the transition@ system@ to svg format.@]"
  | `out_states -> fprintf fmt "@[Export each state to@ svg format.@ This@ option may@ only be@ use in conjuntion@ with the@ -d or@ --export-dot@ options.@]"
  | `out_prism ->  fprintf fmt "@[Export the transition@ system to PRISM@ tra@ format.@]"
  | `out_store ->  fprintf fmt "@[Export each declaration@ in the@ model to@ svg format.@ Dummy@ values are@ used to@ instantiate functional@ values.@]"
  | `help ->       fprintf fmt "@[Show this help.@]"
	  
let flags = function
  | `debug ->      ["--debug"]
  | `verbose ->    ["-v"; "--verbose"]
  | `sim ->        ["-s"; "--simulation"]
  | `s_max ->      ["-m"; "--max-states"]
  | `version ->    ["--version"]
  | `consts ->     ["-c"; "--consts"]
  | `out_csl ->    ["-l"; "--export-labels"]
  | `out_dot ->    ["-d"; "--export-dot"]
  | `out_states -> ["-o"; "--export-states"]
  | `out_prism ->  ["-p"; "--export-prism"]
  | `out_store ->  ["-g"; "--export-store"]
  | `help ->       ["-h"; "--help"] 
	       
let report_error_aux fmt = function
  | Unknown_option s
  | Not_option s -> fprintf fmt "Unknown option: %s" s
  | Malformed_option opt -> fprintf fmt "Missing argument for option `%s'"
				    (String.concat "|" (flags opt))
  | Model_missing -> fprintf fmt "Model missing"
  | Not_big s -> fprintf fmt "`%s' is not a valid model" s
		   		     
let is_option s =
  (String.length s >= 1) && (s.[0] = '-') 

let parse_option s =
  if is_option s then 
    match s with
    | "--debug" ->                `debug
    | "-v" | "--verbose" ->       `verbose
    | "-s" | "--simulation" ->    `sim
    | "-m" | "--max-states" ->    `s_max
    | "--version" ->              `version
    | "-c" | "--consts" ->        `consts
    | "-l" | "--export-labels" -> `out_csl 
    | "-d" | "--export-dot" ->    `out_dot
    | "-o" | "--export-states" -> `out_states
    | "-p" | "--export-prism" ->  `out_prism
    | "-g" | "--export-store" ->  `out_store
    | "-h" | "--help" ->          `help
    | _ ->                         raise (ERROR (Unknown_option s))
  else raise (ERROR (Not_option s))

let usage_str fmt () =  
  fprintf fmt "@[USAGE: bigrapher @[[--version]@\n\
[--help|-h]@\n<model.big> [predicates.bilog] [options]@]@]@."
	  
let usage fmt () =
  fprintf fmt "%a@[Try `bigrapher --help' for more information.@]@." usage_str ()

let report_error fmt err =
  fprintf fmt "@[%a@]@.%a" report_error_aux err usage ();
  exit 1
	  
let options_str fmt () =
  let l =
    [ `consts; `out_dot; `out_store; `help; `out_csl; `s_max; 
      `out_states; `out_prism; `sim; `verbose; `version ]
  and flag_str a = String.concat ", " (flags a) in	    
  let format fmt = function
    | `debug -> fprintf fmt ""
    | `version as a -> fprintf fmt "%s@\n%a" (flag_str a) msg a 
    | `sim as a -> fprintf fmt "%s <float>@\n%a" (flag_str a) msg a
    | `s_max as a -> fprintf fmt "%s [int]@\n%a" (flag_str a) msg a
    | `consts as a-> fprintf fmt "%s x=val,...@\n%a" (flag_str a) msg a
    | `out_csl
    | `out_dot
    | `out_prism as a -> fprintf fmt "%s [outfile]@\n%a" (flag_str a) msg a
    | `out_store as a -> fprintf fmt "%s [dir]@\n%a" (flag_str a) msg a
    | `help
    | `verbose
    | `out_states as a-> fprintf fmt "%s@\n%a" (flag_str a) msg a in
  List.iter (fprintf fmt "@[<h 6>%a@]@\n" format) l 

let help fmt () =
  fprintf fmt "@[<0>%a@\n@[<6>OPTIONS:@\n%a@]@]@." usage_str () options_str ()

let parse_consts consts i =
  let lexbuf = Lexing.from_string consts in
  lexbuf.Lexing.lex_curr_p <-
    { Lexing.pos_fname = "Argv " ^ (string_of_int i);
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0;
    };
  Parser.const_list Lexer.token lexbuf          

let parse_int args i =
  try defaults.s_max <- int_of_string args.(i + 1) with
  | _ -> raise (ERROR (Malformed_option `s_max))

let parse_file a args i =
  try
    match a with
    | `out_csl -> defaults.out_csl <- Some (args.(i))
    | `out_dot -> defaults.out_dot <- Some (args.(i))
    | `out_prism -> defaults.out_prism <- Some (args.(i))
    | `out_store -> defaults.out_store <- Some (args.(i))
    | `verbose | `sim | `s_max | `version | `out_states
    | `consts | `help | `debug -> assert false
  with
  | _ -> raise (ERROR (Malformed_option a))
  
let is_big str = Filename.check_suffix (Filename.basename str) ".big"

let is_bilog str = Filename.check_suffix (Filename.basename str) ".bilog"

let check fmt args =
  if args.out_states && 
       (match args.out_dot with | None -> true | Some _ -> false)
  then fprintf fmt "%s: Ignoring option `%s'.\n%!" 
	       warn (String.concat "|" (flags `out_states))

let parse_options args =
  let rec _parse args i =
    if i < Array.length args then
      (match parse_option args.(i) with
       | `debug -> defaults.debug <- true; _parse args (i + 1)
       | `verbose -> defaults.verbose <- true; _parse args (i + 1)
       | `version -> fprintf std_formatter "@[%s@]@." version; exit 0
       | `help -> help std_formatter (); exit 0 
       | `out_states -> defaults.out_states <- true; _parse args (i + 1)
       | `s_max -> parse_int args (i + 1); _parse args (i + 2)
       | `sim -> (defaults.sim <- true;
		  (try defaults.t_max <- float_of_string args.(i + 1) with
		   | Failure _ -> _parse args (i + 1));
		  _parse args (i + 2))
       | `out_csl -> parse_file `out_csl args (i + 1); _parse args (i + 2)
       | `consts -> (defaults.consts <- parse_consts args.(i + 1) (i + 1);
		     _parse args (i + 2))
       | `out_dot -> parse_file `out_dot args (i + 1); _parse args (i + 2)
       | `out_prism -> parse_file `out_prism args (i + 1); _parse args (i + 2)
       | `out_store -> parse_file `out_store args (i + 1); _parse args (i + 2)
      ); in
  _parse args 0;
  check err_formatter defaults

let parse_model str =
  try 
    match parse_option str with
    | `version -> fprintf std_formatter "@[%s@]@." version; exit 0
    | `help -> help std_formatter (); exit 0
    | `verbose | `sim | `s_max  | `consts  | `out_csl
    | `out_dot | `out_states | `out_prism | `debug  
    | `out_store -> raise (ERROR Model_missing)
  with
  | ERROR (Not_option _) ->
     (if is_big str then defaults.model <- Some str
      else raise (ERROR (Not_big str)))
  | ERROR Model_missing -> report_error err_formatter Model_missing 

let parse_bilog str =
  if is_bilog str && not (is_option str) then (
    defaults.bilog <- Some str; true
  ) else false

let parse cmd =
  if Array.length cmd > 1 then
    (try
	parse_model cmd.(1);
	if parse_bilog cmd.(2) then 
          parse_options (Array.sub cmd 3 ((Array.length cmd) - 3))
	else parse_options (Array.sub cmd 2 ((Array.length cmd) - 2)) 
      with
      | Invalid_argument _ -> ()
      | ERROR (_ as e) -> report_error err_formatter e 
    ) else report_error err_formatter Model_missing
