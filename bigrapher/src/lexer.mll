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
  | Failure _ -> assert false

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
let string_literal = '\"' ['a'-'z' 'A'-'Z' '0'-'9' '-' '_' '\'']* '\"'

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
  | string_literal          { CSTRING (Lexing.lexeme lexbuf) }
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
  | "string"                { STRING }
  | "fun"                   { FUN }
  | "action"                { ACTION }
  | "begin"                 { BEGIN }
  | "brs"		    { BRS }
  | "sbrs"		    { SBRS }
  | "pbrs"                  { PBRS }
  | "nbrs"                  { NBRS }
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
  | "par"                   { PAR }
  | "ppar"                  { PPAR }
  | "if"                    { IF }
  | "ctx"                   { CTX }
  | "param"                 { PARAM }
  | "!"                     { BANG }
  | ctrl_identifier         { CIDE (Lexing.lexeme lexbuf) }
  | identifier              { IDE (Lexing.lexeme lexbuf) }
  | eof                     { EOF }
  | _ as c                  { raise (ERROR (Unknown_char c, Loc.curr lexbuf)) }

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
