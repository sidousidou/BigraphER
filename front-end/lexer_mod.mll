{

open Parser_mod

exception Unknown_character of char

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in 
  lexbuf.Lexing.lex_curr_p <- 
    { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum
    }
    
let drop s =
	String.sub s 0 ((String.length s) - 1)
	  
}

(* REGULAR DEFINITIONS *)

let ctrl_identifier = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let identifier = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let fun_ctrl_ide = ctrl_identifier '('
let fun_ide = identifier '('
let comment = '#' [^'\n']* ('\n' | eof )  
let num =   ((['0'-'9']+ | (['0'-'9']+ '.'['0'-'9']+))
		(['E' 'e'](['+' '-']?)['0'-'9']+)?) 
  | "inf"
 
(* RULES *)

rule lex =
  parse 
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
  | "rate"		    { RATE }
  | "brs"		    { BRS }
  | "endbrs"		    { ENDBRS }
  | "sbrs"		    { SBRS }
  | "endsbrs"		    { ENDSBRS }
  | "rules"		    { RULES }
  | "share"                 { SHARE }
  | "by"                    { BY }
  | "in"                    { IN }
  | "id"                    { ID } 
  | "id("		    { IDP }
  | "id{"		    { IDC }
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
  | num                     { NUM (Lexing.lexeme lexbuf) }
  | ctrl_identifier         { CIDE (Lexing.lexeme lexbuf) }
  | identifier              { IDE (Lexing.lexeme lexbuf) }
  | fun_ctrl_ide						{ FCIDE (drop (Lexing.lexeme lexbuf)) }
  | fun_ide									{ FIDE (drop (Lexing.lexeme lexbuf)) }
  | comment                 { incr_linenum lexbuf; lex lexbuf } 
  | [' ' '\t']              { lex lexbuf }
  | ['\n']                  { incr_linenum lexbuf; lex lexbuf }
  | eof                     { EOF }
  | _ as c                  { raise (Unknown_character c) }

{


}
