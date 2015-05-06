{

open Parser

type error =
  | Unknown_char of char
  | Int_overflow of string

exception ERROR of error * Loc.t

let int_literal s =
  - int_of_string ("-" ^ s)

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
let ctrl_identifier = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let identifier = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let comment = '#' [^'\r' '\n']* (newline | eof)  

let path = [^'\r' '\n' ' ']+			    
let big = path ".big"
let bilog = path ".bilog"

(* RULES *)

rule token =  parse 
  | blank+                  { token lexbuf }
  | (newline | comment)     { Lexing.new_line lexbuf; token lexbuf }
  | int_literal             { let s = Lexing.lexeme lexbuf in
			      try CINT (int_literal s) with
			      | Failure _ -> 
				 raise (ERROR (Int_overflow s, Loc.curr lexbuf)) }
  | float_literal           { try CFLOAT (float_of_string (Lexing.lexeme lexbuf)) with
			      | Failure _ -> assert false }
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
  | "sreact"                { SREACT }
  | "init"                  { INIT }
  | "int"                   { INT }
  | "float"                 { FLOAT }       
  | "fun"                   { FUN }  
  | "brs"		    { BRS }
  | "endbrs"		    { ENDBRS }
  | "sbrs"		    { SBRS }
  | "endsbrs"		    { ENDSBRS }
  | "rules"		    { RULES }
  | "share"                 { SHARE }
  | "by"                    { BY }
  | "in"                    { IN }
  | "id"                    { ID } 
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
  | blank+                  { cmd lexbuf }
  | int_literal             { print_endline "int"; let s = Lexing.lexeme lexbuf in
			      try CINT (int_literal s) with
			      | Failure _ -> 
				 raise (ERROR (Int_overflow s, Loc.curr lexbuf)) }
  | float_literal           { print_endline "float"; try CFLOAT (float_of_string (Lexing.lexeme lexbuf)) with
			      | Failure _ -> assert false }
  | "="                     { print_endline "equal"; EQUAL }  
  | ","                     { print_endline "comma"; COMMA }
  | "svg"                   { print_endline "svg"; F_SVG }
  | "dot"                   { print_endline "dot"; F_DOT }
  (* COMMANDS *)			    
  | "validate"              { print_endline "validate"; C_CHECK }
  | "full"                  { print_endline "full"; C_FULL }
  | "sim"                   { print_endline "sim"; C_SIM }
  (* STAND-ALONE OPTIONS *)			    
  | ("--config" | "-c")     { print_endline "config"; O_CONF }
  | ("--version" | "-V")    { print_endline "version"; O_VERS }
  | ("--help" | "-h")       { print_endline "help"; O_HELP }
  (* COMMON OPTIONS *)			    
  | ("--verbose" | "-v")    { print_endline "verb"; O_VERB }
  | ("--quiet" | "-q")      { print_endline "quiet"; O_QUIET }     
  | ("--const" | "-c")      { print_endline "const"; O_CONST } 
  | "--debug"               { O_DEBUG }
  (* COMMAND OPTIONS *)
  | ("--export-decs"
    | "-d")                 { print_endline "decs"; O_DECS }
  | ("--format" | "-f")     { print_endline "format"; O_FORMAT }
  | ("--export-transition-system"
    | "-t")                 { print_endline "ts"; O_TS }
  | ("--export-states"
    | "-s")                 { O_STATES }
  | ("--export-labels"
    | "-l")                 { O_LABELS }
  | ("--export-prism"
    | "-p")                 { O_PRISM } 
  | ("--max-states"
    | "-M")                 { O_MAX }			    
  | ("--simulation-time"
    | "-T")                 { print_endline "time"; O_TIME }
  | ("--simulation-steps"
    | "-S")                 { O_STEPS } 	 
  | identifier              { print_endline "ide"; IDE (Lexing.lexeme lexbuf) }
  | big                     { print_endline "big"; BIG_FILE (Lexing.lexeme lexbuf) }
  | bilog                   { print_endline "bilog"; BILOG_FILE (Lexing.lexeme lexbuf) }
  | path                    { print_endline "path"; PATH (Lexing.lexeme lexbuf) }
  | eof                     { print_endline "EOF"; EOF }
  | _ as c                  { raise (print_endline "Lexing error"; ERROR (Unknown_char c, Loc.curr lexbuf)) }
			    
{

open Format

let report_error fmt = function
  | Unknown_char c ->
     fprintf fmt "@[%s: Unknown character `%c'@]@," Utils.err c
  | Int_overflow s ->
     fprintf fmt "@[%s: Integer out of bounds: %s is not in [%i, %i]@]@," 
	     Utils.err s min_int max_int

}
