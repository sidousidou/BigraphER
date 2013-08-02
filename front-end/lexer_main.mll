{

open Parser_main

}

(* REGULAR DEFINITIONS *)

let ctrl_identifier = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let identifier = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let comment = '#' [^'\n']* ('\n' | eof )  
let num =   ((['0'-'9']+ | (['0'-'9']+ '.'['0'-'9']+))
		(['E' 'e'](['+' '-']?)['0'-'9']+)?) 
  | "inf"
 
(* RULES *)

rule lex =  parse 
  | "["                     { LSBR }
  | "]"                     { RSBR }
  | "{"                     { LCBR }
  | "}"                     { RCBR }
  | "("                     { LPAR }
  | ")"                     { RPAR }
  | "ctrl"                  { CTRL }
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
  | comment                 { Utils.incr_linenum lexbuf; lex lexbuf } 
  | [' ' '\t']              { lex lexbuf }
  | ['\n']                  { Utils.incr_linenum lexbuf; lex lexbuf }
  | eof                     { EOF }
  | _ as c                  { raise (Utils.UNKNOWN_CHAR (c, Lexing.lexeme_start_p lexbuf)) }

{


}
