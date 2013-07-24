%{
  
open Syntax
open Base

%}

%token <string>   IDE
%token <string>		NUM       

%token            LSBR RSBR LCBR RCBR LPAR RPAR
%token						CLASS ENDCLASS CLASSR ENDCLASSR INT FLOAT RULES
%token						EQUAL COLON SEMICOLON COMMA
%token            EOF
 
%start priorities
%type <Syntax.pri_class list> priorities

%%

priorities
		: /* EMPTY */															{ [] }
		| priority SEMICOLON priorities						{ $1::$3 }
		;

priority
		: CLASS params rule_set ENDCLASS					{ Pri_class ($2, $3) }
		| CLASSR params rule_set ENDCLASSR				{ Pri_classr ($2, $3) }
		;

params
		: /* EMPTY */															{ [] }
		| param SEMICOLON params									{ ($1)::$3 }
		;

param
		:	INT IDE EQUAL NUM												
				{ Int_dec ($2, [int_of_string $4]) }
		| INT IDE EQUAL LCBR int_vals RCBR				{ Int_dec ($2, $5) }
		| INT IDE EQUAL LSBR NUM COLON NUM COLON NUM RSBR		
				{ Int_dec ($2, int_interval
				(int_of_string $5) (int_of_string $7) (int_of_string $9)) }
		|	FLOAT IDE EQUAL NUM
				{ Float_dec ($2, [float_of_string $4]) }
		| FLOAT IDE EQUAL LCBR float_vals RCBR		{ Float_dec ($2, $5) }
		| FLOAT IDE EQUAL LSBR NUM COLON NUM COLON NUM RSBR
				{ Float_dec ($2, float_interval
				(float_of_string $5) (float_of_string $7) (float_of_string $9)) }
		;	

int_vals
		: NUM																			{ [int_of_string $1] }
		| NUM COMMA int_vals											{ (int_of_string $1)::$3 }
		;

float_vals
		: NUM																			{ [float_of_string $1] }
		| NUM COMMA float_vals										{ (float_of_string $1)::$3 }
		;			

rule_set
		: RULES EQUAL LCBR rules RCBR SEMICOLON		{ $4 }
		;

rules
		: rule																		{ [$1] }
		| rule COMMA rules												{ $1::$3 }
		;

rule
		: IDE																			{ Rul ($1, []) }
		| IDE LPAR forms RPAR											{ Rul ($1, $3) }
		; 		

forms
		: IDE																		{ [$1] }
		| IDE COMMA forms												{ $1::$3 }
		;	
						 
%%

