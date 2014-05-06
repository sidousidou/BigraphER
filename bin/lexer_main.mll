{

open Lexing
open Parser_main

exception UNKNOWN_CHAR of char * position

let incr_linenum lexbuf =
  let pos = lexbuf.lex_curr_p in 
  lexbuf.lex_curr_p <- 
    { pos with pos_lnum = pos.pos_lnum + 1;
               pos_bol = lexbuf.lex_curr_pos
    }

}

(* REGULAR DEFINITIONS *)

let digit = ['0'-'9']
let int = digit+
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit+ frac? exp? | "inf"
let num = int | float

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let ctrl_identifier = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let identifier = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let comment = '#' [^'\r' '\n']* (newline | eof)  
 
(* RULES *)

rule lex =  parse 
  | white                   { lex lexbuf }
  | newline                 { incr_linenum lexbuf; lex lexbuf }
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
  | "->"                    { ARR }  
  | "@"                     { AT }
  | "."                     { DOT } 
  | "||"                    { DPIPE }  
  | "|"                     { PIPE }
  | "+"                     { PLUS }
  | "-"                     { MINUS }
  | "*"                     { PROD }
  | "/"                     { SLASH }
  | "^"                     { CARET }
  | "true"                  { TRUE }
  | "false"                 { FALSE }
  | "not"                   { NOT }
  | "sort"                  { SORT }
  | num                     { NUM (Lexing.lexeme lexbuf) }
  | ctrl_identifier         { CIDE (Lexing.lexeme lexbuf) }
  | identifier              { IDE (Lexing.lexeme lexbuf) }
  | comment                 { incr_linenum lexbuf; lex lexbuf } 
  | _ as c                  { raise (UNKNOWN_CHAR (c, Lexing.lexeme_start_p lexbuf)) }
  | eof                     { EOF }

{


}
