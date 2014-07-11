{

open Parser_main

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
let comment = '#' [^'\r' '\n']* newline  
 
(* RULES *)

rule token =  parse 
  | blank+                  { token lexbuf }
  | (newline | comment)     { Lexing.new_line lexbuf;
                              token lexbuf }
  | ctrl_identifier         { CIDE (Lexing.lexeme lexbuf) }
  | identifier              { IDE (Lexing.lexeme lexbuf) }
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
  (* | "true"                  { TRUE } *)
  (* | "false"                 { FALSE } *)
  (* | "not"                   { NOT } *)
  (* | "sort"                  { SORT } *)
  | eof                     { EOF }
  | int_literal
    { let s = Lexing.lexeme lexbuf in
      try
        CINT (int_literal s)
      with
      | Failure _ -> 
        raise (ERROR (Int_overflow s, Loc.curr lexbuf))
    }
  | float_literal
    { try
        CFLOAT (float_of_string (Lexing.lexeme lexbuf))
      with
      | Failure _ -> assert false
    }
  | _ as c                  
    { raise (ERROR (Unknown_char c, Loc.curr lexbuf)) }

{

open Format

let report_error fmt = function
  | Unknown_char c ->
    fprintf fmt "unknown character %c" c
  | Int_overflow s ->
    fprintf fmt "integer out of bounds: %s is not in [%i, %i]" 
      s min_int max_int

}
