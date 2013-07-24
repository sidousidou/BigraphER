{

open Parser_pri

exception Unknown_character of char

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p 
	  in lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum}

}

(* REGULAR DEFINITIONS *)

let identifier = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let num = 
	((['0'-'9']+ 
	|(['0'-'9']+ '.'['0'-'9']+))(['E' 'e'](['+' '-']?)['0'-'9']+)?) 
  	| "inf"
let comment = '#' [^'\n']* ('\n' | eof )  
 
(* RULES *)

rule lex =
	parse
		| "class"									{ CLASS }
		|	"endclass"							{ ENDCLASS }
		| "class*"								{ CLASSR }
		| "endclass*"							{ ENDCLASSR }
		| "="											{ EQUAL }	 
		| ";"											{ SEMICOLON }
		| ":"											{ COLON }
		| ","											{ COMMA }
		| "int"										{ INT }	
		| "float"		 							{ FLOAT }
		| "rules"									{ RULES }	
		| "["                     { LSBR }
		| "]"                     { RSBR }
		| "{"                     { LCBR }
		| "}"                     { RCBR }
		| "("                     { LPAR }
		| ")"                     { RPAR }
		| identifier              { IDE (Lexing.lexeme lexbuf) }
		| num                     { NUM (Lexing.lexeme lexbuf) }
		| comment                 { incr_linenum lexbuf; lex lexbuf } 
    | [' ' '\t']              { lex lexbuf }
		| ['\n']                  { incr_linenum lexbuf; lex lexbuf }
		| eof                     { EOF }
		| _ as c                  { raise (Unknown_character c) }

{


}
