%{

open Base  
open Syntax

let string_of_err start_pos end_pos =
  Printf.sprintf "File \"%s\", line %d, charachters %d-%d:\n"
    start_pos.Lexing.pos_fname start_pos.Lexing.pos_lnum
    (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
    (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol)

let mess = "Syntax error: "

let dec_error = mess ^ "Invalid declaration\n"

let bexp_error = mess ^ "Invalid bigraphical expression\n"

let form_error s = mess ^ "Invalid parameter " ^ s ^ "\n"

let id_error = mess ^ "Invalid identity\n"

let placexp_error = mess ^ "Invalid placing expression\n"

let num_error = mess ^ "Invalid numerical expression\n"

let ion_exp_error = mess ^ "Invalid ion expression\n"

let bilog_error = mess ^ "Invalid bilog\n"

let raise_parse_failure m =
  let start_pos = Parsing.symbol_start_pos ()
  and end_pos = Parsing.symbol_end_pos ()
  in failwith ((string_of_err start_pos end_pos) ^ m)

%}

%token <string>   CIDE
%token <string> 	FCIDE
%token <string>   IDE       
%token <string>		FIDE
%token <string>   NUM      


%token            LSBR RSBR LCBR RCBR LPAR RPAR 
%token 	          CTRL BIG INT FLOAT FUN BILOG ENDBILOG PREDS
%token            SHARE BY IN
%token            ID IDC IDP
%token            COLON SEMICOLON EQUAL COMMA

%token            DOT
%token            PIPE DPIPE PLUS MINUS
%token            PROD SLASH
%token            CARET 
%token            EOF
 
%start preds
%type <(Syntax.dec list * Syntax.bilog)> preds

%left  PIPE DPIPE PLUS MINUS
%left  PROD SLASH
%right DOT CARET

%%

preds
    : dec_list bilog EOF                        { ($1, $2) }
    ;

dec_list
    : dec SEMICOLON                           { [$1] }
    | dec SEMICOLON dec_list                  { $1::$3 }   
    ;

dec 
    : CTRL CIDE EQUAL NUM                     
    																					{ Ctrl_dec 
    																							($2, int_of_string $4) }
		| FUN CTRL FCIDE forms RPAR EQUAL NUM 
																							{ Ctrl_dec_f 
																									($3, $4, int_of_string $7) }
		| INT IDE EQUAL num_exp                   { Iexp_dec ($2, $4) }
		| FLOAT IDE EQUAL num_exp                 { Fexp_dec ($2, $4) }
		| BIG IDE EQUAL bexp     					        { Big_dec ($2, $4) }
		| FUN BIG FIDE forms RPAR EQUAL bexp			{ Big_dec_f ($3, $4, $7) }
		| error                                   { raise_parse_failure dec_error }
	  ;

bilog
		: BILOG params pred_set ENDBILOG       	  { Bilog ($2, $3) }
		| error                                   { raise_parse_failure bilog_error }
		;

params
		: /* EMPTY */															{ [ ] }
		| param SEMICOLON params									{ ($1)::$3 }
		;

param
		:	INT IDE EQUAL NUM												
																							{ Int_dec
																									($2, [int_of_string $4]) }
		| INT IDE EQUAL LCBR int_vals RCBR				{ Int_dec ($2, $5) }
		| INT IDE EQUAL LSBR NUM COLON NUM COLON NUM RSBR		
																							{ Int_dec ($2, int_interval
																									(int_of_string $5)
																									(int_of_string $7)
																									(int_of_string $9)) }
		|	FLOAT IDE EQUAL NUM
																							{ Float_dec 
																									($2, [float_of_string $4]) }
		| FLOAT IDE EQUAL LCBR float_vals RCBR		{ Float_dec ($2, $5) }
		| FLOAT IDE EQUAL LSBR NUM COLON NUM COLON NUM RSBR
																							{ Float_dec ($2, float_interval
																									(float_of_string $5) 
																									(float_of_string $7) 
																									(float_of_string $9)) }
		;	

int_vals
		: NUM																			{ [int_of_string $1] }
		| NUM COMMA int_vals											{ (int_of_string $1)::$3 }
		;

float_vals
		: NUM																			{ [float_of_string $1] }
		| NUM COMMA float_vals										{ (float_of_string $1)::$3 }
		;			

pred_set
		: PREDS EQUAL LCBR pred_list RCBR SEMICOLON		{ $4 }
		;

pred_list
		: pred																		{ [$1] }
		| pred COMMA pred_list												{ $1::$3 }
		;

pred
		: IDE																			{ Pred ($1, []) }
		| FIDE forms RPAR													{ Pred ($1, $2) }
		; 		

forms
		: IDE																			{ [$1] }
		| IDE COMMA forms													{ $1::$3 }
		;

num_exp
    : NUM                                     { Num_v $1 }
		| IDE                                     { Num_ide $1 } 
		| LPAR num_exp RPAR                       { $2 }
		| num_exp CARET num_exp                   { Pow ($1, $3) } 
		| num_exp PROD num_exp                    { Prod ($1, $3) }
    | num_exp SLASH num_exp                   { Div ($1, $3) }
		| num_exp PLUS num_exp                    { Plus ($1, $3) }   
		| num_exp MINUS num_exp                   { Minus ($1, $3) }
		| error                                   { raise_parse_failure num_error }
		;		

places
    : /* EMPTY */                             { [ ] }
    | LCBR int_vals RCBR											{ [$2] }				
    | LCBR int_vals RCBR COMMA places					{ $2::$5 }	
		;

ion_exp
		: CIDE																		{ Big_ion ($1, [], []) }
		| CIDE LCBR forms RCBR										{ Big_ion ($1, [], $3) }	
		| FCIDE args RPAR													{ Big_ion ($1, $2, []) }				
		| FCIDE args RPAR LCBR forms RCBR					{ Big_ion ($1, $2, $5) }
		;

id_exp
		: ID			                               	{ Big_id (1, []) }
  	| IDC forms RCBR													{ Big_id (0, $2) }	
  	| IDP NUM RPAR														{	Big_id (int_of_string $2, []) }							
  	| IDP NUM COMMA LCBR forms RCBR RPAR			{ Big_id (int_of_string $2, $5) }
		;

args
		: num_exp																	{ [$1] }
		| num_exp COMMA args											{ $1:: $3 }
		;

closures
		: SLASH IDE																{ Big_close $2 }
		|	SLASH IDE opt_clos simple_exp						{ Big_comp ($2::$3, $4) }
  	; 

opt_clos
		: /*EMPTY*/																{ [] }
		| SLASH IDE opt_clos											{ $2::$3 }	
		;

simple_exp
		: IDE                                			{ Big_ide ($1, []) }
    | FIDE args RPAR													{ Big_ide ($1, $2) }
    | ion_exp																	{ $1 }
    |	ion_exp DOT simple_exp									{ Big_nest ($1, $3) }	
		| LPAR LSBR places RSBR COMMA NUM RPAR   	{ Big_plac 
  																								($3, int_of_string $6) }
  	| LPAR bexp RPAR													{ $2 }				
  	| NUM                                     { Big_el (int_of_string $1) }
  	| closures																{ $1 }	
  	| id_exp																	{ $1 }
		;
  			
bexp
    :	
		| simple_exp															{ $1 }  
		| bexp PIPE bexp 					                { Big_par ($1, $3) }
		| bexp DPIPE bexp				                	{ Big_ppar ($1, $3) }
 		| SHARE simple_exp BY simple_exp IN simple_exp
 																							{ Big_share ($2, $4, $6) }
		| error                                   { raise_parse_failure
																										bexp_error }
		;
						 
%%

