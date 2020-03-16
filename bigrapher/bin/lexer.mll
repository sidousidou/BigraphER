{

open Parser

type error =
  | Unknown_char of char
  | Int_overflow of string
  | Invalid_assign of string
  | Invalid_format of string

exception ERROR of error * Loc.t

let int_literal s =
  - int_of_string ("-" ^ s)

let read_int lexbuf =
  let s = Lexing.lexeme lexbuf in	
  try CINT (int_literal s) with
  | Failure _ -> raise (ERROR (Int_overflow s, Loc.curr lexbuf))

let read_float lexbuf =
  try CFLOAT (float_of_string (Lexing.lexeme lexbuf)) with
  | Failure _ -> assert false (*BISECT-IGNORE*)

}

(* REGULAR DEFINITIONS *)

let blank = [' ' '\009' '\012']
let newline = ('\r' | '\n' | "\r\n")
let int_literal = ['0'-'9'] ['0'-'9' '_']*
let float_literal =
  ("inf" | (
      ['0'-'9'] ['0'-'9' '_']*
      ('.' ['0'-'9' '_']*)?
      (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?
    )
  )
let ctrl_identifier = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let identifier = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let comment = '#' [^'\r' '\n']* (newline | eof)  

let argv_sep = "\n"
let const = ("--const" | "-c") argv_sep
let format = ("--format" | "-f") argv_sep
let path = [^'\r' '\n']+			    
let big = path ".big"

(* RULES *)

rule token =  parse 
  | blank+                  { token lexbuf }
  | (newline | comment)     { Lexing.new_line lexbuf; token lexbuf }
  | int_literal             { read_int lexbuf }
  | float_literal           { read_float lexbuf }
  | "["                     { LSBR }
  | "]"                     { RSBR }
  | "{"                     { LCBR }
  | "}"                     { RCBR }
  | "("                     { LPAR }
  | ")"                     { RPAR }
  | "ctrl"                  { CTRL }
  | "atomic"                { ATOMIC }     
  | "big"                   { BIG } 
  | "react"                 { REACT }
  | "init"                  { INIT }
  | "int"                   { INT }
  | "float"                 { FLOAT }       
  | "fun"                   { FUN }  
  | "begin"                 { BEGIN }
  | "brs"		    { BRS }
  | "sbrs"		    { SBRS }
  | "pbrs"                  { PBRS }
  | "end"		    { END }
  | "rules"		    { RULES }
  | "preds"                 { PREDS }
  | "share"                 { SHARE }
  | "by"                    { BY }
  | "in"                    { IN }
  | "id"                    { ID }
  | "merge"		    { MERGE }
  | "split"                 { SPLIT }
  | ":"                     { COLON }
  | ";"                     { SEMICOLON }
  | "="                     { EQUAL }  
  | ","                     { COMMA }    
  | ("->" | "-->")          { ARR }
  | "-["                    { LARR }
  | "]->"                   { RARR }
  | "@"                     { AT }
  | "."                     { DOT } 
  | "||"                    { DPIPE }  
  | "|"                     { PIPE }
  | "+"                     { PLUS }
  | "-"                     { MINUS }
  | "*"                     { PROD }
  | "/"                     { SLASH }
  | "^"                     { CARET }
  | ctrl_identifier         { CIDE (Lexing.lexeme lexbuf) }
  | identifier              { IDE (Lexing.lexeme lexbuf) }
  | eof                     { EOF }
  | _ as c                  { raise (ERROR (Unknown_char c, Loc.curr lexbuf)) }

and cmd =  parse
  | argv_sep                { cmd lexbuf }
  | int_literal             { read_int lexbuf }
  | float_literal           { read_float lexbuf }
  (* COMMANDS *)			    
  | "validate"              { C_CHECK }
  | "full"                  { C_FULL }
  | "sim"                   { C_SIM }
  | "interactive"           { C_INTERACTIVE }
  (* STAND-ALONE OPTIONS *)			    
  | ("--config" | "-C")     { O_CONF }
  | ("--version" | "-V")    { O_VERS }
  | ("--help" | "-h")       { O_HELP }
  (* COMMON OPTIONS *)			    
  | ("--verbose" | "-v")    { O_VERB }
  | ("--quiet" | "-q")      { O_QUIET }     
  | const      	 	    { O_CONST (read_string_const (Buffer.create 20) lexbuf) }
  | "--debug"               { O_DEBUG }
  | ("--no-colors" | "-n")  { O_COLORS } 
  (* COMMAND OPTIONS *)
  | ("--export-decs"
    | "-d")                 { O_DECS }
  | format                  { O_FORMAT (read_string_format (Buffer.create 20) lexbuf) }
  | ("--export-ts"
    | "-t")                 { O_TS }
  | ("--export-states"
    | "-s")                 { O_STATES }
  | ("--export-labels"
    | "-l")                 { O_LABELS }
  | ("--export-prism"
    | "-p")                 { O_PRISM }
  | ("--export-ml"    
    | "-m")                 { O_ML } 
  | ("--max-states"
    | "-M")                 { O_MAX }			    
  | ("--simulation-time"
    | "-T")                 { O_TIME }
  | ("--simulation-steps"
    | "-S")                 { O_STEPS } 	 
  | big                     { BIG_FILE (Lexing.lexeme lexbuf) }
  | path                    { PATH (Lexing.lexeme lexbuf) }
  | eof                     { EOF }
  | _ as c                  { raise (ERROR (Unknown_char c, Loc.curr lexbuf)) }

and read_string_const buf = parse
  | argv_sep
    { let s = Buffer.contents buf in
      try
        Parser.consts read_consts (Lexing.from_string s)
      with
      | _ ->
        raise (ERROR (Invalid_assign s, Loc.curr lexbuf)) }  
  | [^ '\n'] as c  	    { Buffer.add_char buf c; read_string_const buf lexbuf }
  | eof                     { raise (ERROR (Invalid_assign (Buffer.contents buf), Loc.curr lexbuf)) }
  | _ as c 	            { raise (ERROR (Unknown_char c, Loc.curr lexbuf)) }

and read_string_format buf = parse
  | argv_sep
    { let s = Buffer.contents buf in
      try
        Parser.format read_format (Lexing.from_string s)
      with
      | _ ->
        raise (ERROR (Invalid_format s, Loc.curr lexbuf)) }  
  | [^ '\n'] as c  	    { Buffer.add_char buf c; read_string_format buf lexbuf }
  | eof                     { raise (ERROR (Invalid_format (Buffer.contents buf), Loc.curr lexbuf)) }
  | _ as c 	            { raise (ERROR (Unknown_char c, Loc.curr lexbuf)) }

and read_consts = parse
  | int_literal             { read_int lexbuf }
  | float_literal           { read_float lexbuf }
  | "="                     { EQUAL }  
  | ","                     { COMMA }
  | identifier              { IDE (Lexing.lexeme lexbuf) }	
  | eof                     { EOF }
  | _ as c                  { raise (ERROR (Unknown_char c, Loc.curr lexbuf)) }
 
and read_format = parse 
  | "svg"                   { F_SVG }
  | "dot"                   { F_DOT }
  | "txt"                   { F_TXT }
  | "json"		    { F_JSON }
  | ","                     { COMMA }
  | eof  		    { EOF }
  | _ as c      	    { raise (ERROR (Unknown_char c, Loc.curr lexbuf)) }

{

open Format

let report_error fmt err = function
  | Unknown_char c ->
     fprintf fmt "@[%s: Unknown character `%c'@]" err c
  | Int_overflow s ->
     fprintf fmt "@[%s: Integer out of bounds: %s is not in [%i, %i]@]" 
	     err s min_int max_int
  | Invalid_assign s ->
     fprintf fmt "@[%s: Invalid constant assignments `%s'@]" err s
  | Invalid_format s ->
     fprintf fmt "@[%s: Invalid format `%s'@]" err s

}
