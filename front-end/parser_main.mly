%{

  open Utils
  open Syntax
 
  let add_closure n c =
    match c with
    | Big_close (l, pos) -> Big_close (n :: l, pos)
    | _ -> parse_error_msg "Invalid closure" n; 
      raise Parsing.Parse_error

%}

%token <string>   CIDE
%token <string>   IDE       
%token <string>   NUM      

%token            LSBR RSBR LCBR RCBR LPAR RPAR 
%token 	          CTRL BIG REACT SREACT INIT INT FLOAT FUN BRS ENDBRS SBRS ENDSBRS RULES SORT
%token            TRUE FALSE NOT
%token            SHARE BY IN
%token            ID 
%token            COLON SEMICOLON EQUAL COMMA ARR AT

%token            PIPE DPIPE PLUS MINUS
%token            DOT PROD SLASH
%token            CARET 
%token            EOF
 
%start model
%type <(Syntax.dec list * Syntax.brs)> model

%left  PIPE DPIPE 
%left  PLUS MINUS
%left  PROD SLASH
%right DOT CARET

%%

model
  : dec_list brs EOF                        { $1, $2 }
  ;

dec_list
  : dec SEMICOLON                           { [ $1 ] }
  | dec SEMICOLON dec_list                  {  $1 :: $3 }   
  ;

dec 
  : CTRL CIDE EQUAL NUM 		    { try
						let n = int_of_string $4 in
						if n >= 0 then Ctrl_dec ($2, n, add_pos ())
						else (parse_error_msg "Invalid ctrl arity" $4;
						      raise Parsing.Parse_error)
                                              with
                                              | Failure _ -> 
                                                  parse_error_msg "Invalid ctrl arity" $4; 
                                                  raise Parsing.Parse_error
                                            }
  | FUN CTRL CIDE LPAR forms RPAR EQUAL NUM { try
						let n = int_of_string $8 in
						if n >= 0 then Ctrl_dec_f ($3, $5, n, add_pos ())
						else (parse_error_msg "Invalid ctrl arity" $8;
						      raise Parsing.Parse_error)  
                                              with
                                              | Failure _ -> 
                                                  parse_error_msg "Invalid ctrl arity" $8; 
                                                  raise Parsing.Parse_error
                                            }
  | INT IDE EQUAL num_exp                   { Int_dec ($2, $4, add_pos ()) }
  | FLOAT IDE EQUAL num_exp                 { Float_dec ($2, $4, add_pos ()) }
  | BIG IDE EQUAL bexp                      { Big_dec ($2, $4, add_pos ()) }
  | FUN BIG IDE LPAR forms RPAR EQUAL bexp  { Big_dec_f ($3, $5, $8, add_pos ()) }
  | REACT IDE EQUAL bexp ARR bexp           { React_dec ($2, $4, $6, add_pos ()) }
  | FUN REACT IDE LPAR forms RPAR EQUAL bexp ARR bexp
	                                    { React_dec_f ($3, $5, $8, $10, add_pos ()) }
  | SREACT IDE EQUAL bexp ARR bexp AT num_exp
                                            { Sreact_dec ($2, $4, $6, $8, add_pos ()) }
  | FUN SREACT IDE LPAR forms RPAR EQUAL bexp ARR bexp AT num_exp
                                            { Sreact_dec_f ($3, $5, $8, $10, $12, add_pos ()) }
  | error                                   { parse_error_msg "Invalid declaration" ""; 
                                              raise PARSE_ERROR}		   
  ;

brs
  : BRS params init rule_set ENDBRS         { Brs ($2, $3, $4, add_pos ()) }
  | SBRS params init rule_set ENDSBRS       { Sbrs ($2, $3, $4, add_pos ()) }
  /*| error                                   { parse_error_msg "Invalid brs" ""; 
                                              raise Parsing.Parse_error}*/		    
  ;

params
  : /* EMPTY */                             { [ ] }
  | param SEMICOLON params                  { $1 :: $3 }
  ;

param
  : INT IDE EQUAL num_exp                   { Int_param ($2, [ $4 ], add_pos ()) }
  | INT IDE EQUAL LCBR acts RCBR            { Int_param ($2, $5, add_pos ()) }
  | INT IDE EQUAL LSBR NUM COLON NUM COLON NUM RSBR		
                                            { let start =
						try
						  int_of_string $5
						with
						| Failure _ -> 
                                                    parse_error_msg "Invalid int" $5; 
                                                    raise Parsing.Parse_error
					      and step =
						try
						  int_of_string $7
						with
						| Failure _ -> 
                                                    parse_error_msg "Invalid int" $7; 
                                                    raise Parsing.Parse_error
					      and stop =
						try
						  int_of_string $9
						with
						| Failure _ -> 
                                                    parse_error_msg "Invalid int" $9; 
                                                    raise Parsing.Parse_error in
					      try
						Int_param ($2, List.map (fun x -> 
						  Num_val (float_of_int x, add_pos())) (int_interval start step stop), add_pos ())
					      with
					      | INVALID_INTERVAL ->
						  parse_error_msg "Invalid interval for parameter" $2; 
                                                  raise Parsing.Parse_error
                                            }
  | FLOAT IDE EQUAL num_exp                 { Float_param ($2, [ $4 ], add_pos ()) }
  | FLOAT IDE EQUAL LCBR acts RCBR          { Float_param ($2, $5, add_pos ()) }
  | FLOAT IDE EQUAL LSBR NUM COLON NUM COLON NUM RSBR
                                            { let start =
						try
						  float_of_string $5
						with
						| Failure _ -> 
                                                    parse_error_msg "Invalid float" $5; 
                                                    raise Parsing.Parse_error
					      and step =
						try
						  float_of_string $7
						with
						| Failure _ -> 
                                                    parse_error_msg "Invalid float" $7; 
                                                    raise Parsing.Parse_error
					      and stop =
						try
						  float_of_string $9
						with
						| Failure _ -> 
                                                    parse_error_msg "Invalid float" $9; 
                                                    raise Parsing.Parse_error in
					      try
						Float_param ($2, List.map (fun x -> Num_val (x, add_pos ())) (float_interval start step stop), add_pos ())
					      with
					      | INVALID_INTERVAL ->
						  parse_error_msg "Invalid interval for parameter" $2; 
                                                  raise Parsing.Parse_error
                                            }
  | error                                   { parse_error_msg "Invalid parameter" ""; 
                                              raise PARSE_ERROR}	    
  ;	

init
  : INIT IDE SEMICOLON                      { Init ($2, add_pos ()) }
  | INIT IDE LPAR acts RPAR SEMICOLON       { Init_fun ($2, $4, add_pos ()) }
 /* | error                                   { parse_error_msg "Invalid init" ""; 
                                              raise Parsing.Parse_error}*/		    
  ;

rule_set
  : RULES EQUAL LSBR p_classes RSBR SEMICOLON
                                            { $4 }
  | error                                   { parse_error_msg "Invalid rules" ""; 
                                              raise PARSE_ERROR}	    
  ;

p_classes
 : p_class                                  { [ $1 ] }
 | p_class COMMA p_classes                  { $1 :: $3 }
 ;

p_class
 : LCBR rules RCBR                          { Pri_class ($2, add_pos ()) }
 | LPAR rules RPAR                          { Pri_classr ($2, add_pos ()) }
 ;

rules
  : rule                                    { [ $1 ] }
  | rule COMMA rules                        { $1 :: $3 }
  ;

rule
  : IDE                                     { Rul ($1, add_pos ()) }
  | IDE LPAR forms RPAR                     { Rul_fun ($1, $3, add_pos ()) }
  ; 		

forms
  : IDE                                     { [ $1 ] }
  | IDE COMMA forms                         { $1 :: $3 }
  ;

num_exp
  : NUM                                     { try
						Num_val (float_of_string $1, add_pos ())
                                              with
                                              | Failure _ -> 
                                                  parse_error_msg "Invalid numerical expression" $1; 
                                                  raise Parsing.Parse_error 
					    }
  | IDE                                     { Num_ide ($1, add_pos ()) } 
  | LPAR num_exp RPAR                       { $2 }
  | num_exp PROD num_exp                    { Num_prod ($1, $3, add_pos ()) }     
  | num_exp CARET num_exp                   { Num_pow ($1, $3, add_pos ()) }
  | num_exp SLASH num_exp                   { Num_div ($1, $3, add_pos ()) }
  | num_exp PLUS num_exp                    { Num_plus ($1, $3, add_pos ()) }   
  | num_exp MINUS num_exp                   { Num_minus ($1, $3, add_pos ()) }
  | error                                   { parse_error_msg "Invalid numerical expression" ""; 
                                              raise Parsing.Parse_error}		    
  ;		

acts
  : num_exp                                 { [ $1 ] }
  | num_exp COMMA acts                      { $1 :: $3 }
  ;

bexp
  : simple_exp                              { $1 } 
  | closures                                { $1 }
  | bexp PROD bexp                          { Big_comp ($1, $3, add_pos ()) }
  | bexp PLUS bexp                          { Big_tens ($1, $3, add_pos ()) }
  | bexp PIPE bexp                          { Big_par ($1, $3, add_pos ()) }
  | bexp DPIPE bexp                         { Big_ppar ($1, $3, add_pos ()) }
  | SHARE simple_exp BY simple_exp IN simple_exp
                                            { Big_share ($2, $4, $6, add_pos ()) }
  | error                                   { parse_error_msg "Invalid bigraph" ""; 
                                              raise Parsing.Parse_error}		    
  ;

simple_exp
  : IDE                                     { Big_ide ($1, add_pos ()) }
  | IDE LPAR acts RPAR                      { Big_ide_fun ($1, $3, add_pos ()) }
  | ion_exp                                 { $1 }
  | ion_exp DOT simple_exp                  { Big_nest ($1, $3, add_pos ()) }	
  | LPAR LSBR places RSBR COMMA NUM RPAR    { try
						let n = int_of_string $6 in
						if n < 0 then (parse_error_msg "Invalid placing" $6; 
							       raise Parsing.Parse_error)
						else Big_plac ($3, n, add_pos ())
                                              with
					      | Failure _ ->
						  parse_error_msg "Invalid int" $6; 
                                                  raise Parsing.Parse_error
					    }
  | LPAR bexp RPAR                          { $2 }				
  | NUM                                     { try
						let n = int_of_string $1 in
						if (n != 0) && (n != 1) then (parse_error_msg "Invalid elementary bigraph" $1; 
									      raise Parsing.Parse_error)
						else Big_el (n, add_pos ())
                                              with
					      | Failure _ ->
						  parse_error_msg "Invalid int" $1; 
                                                  raise Parsing.Parse_error
					    }
  | id_exp                                  { $1 }
  ;

plc_vals
  : NUM                                     { try 
						let n = int_of_string $1 in
						if n < 0 then (parse_error_msg "Invalid placing" $1; 
							       raise Parsing.Parse_error)
						else [ n ]
                                              with
					      | Failure _ ->
						  parse_error_msg "Invalid int" $1; 
                                                  raise Parsing.Parse_error
					    }
  | NUM COMMA plc_vals                      { try 
						let n = int_of_string $1 in
						if n < 0 then (parse_error_msg "Invalid placing" $1; 
							       raise Parsing.Parse_error)
						else n :: $3 
                                              with
					      | Failure _ ->
						  parse_error_msg "Invalid int" $1; 
                                                  raise Parsing.Parse_error
					    }
  ;

places
  : /* EMPTY */                             { [ ] }
  | LCBR plc_vals RCBR                      { [ $2 ] }				
  | LCBR plc_vals RCBR COMMA places         { $2 :: $5 }
  | error                                   { parse_error_msg "Invalid vector" ""; 
                                               raise Parsing.Parse_error}		    	
  ;

ion_exp
  : CIDE                                    { Big_ion ($1, [], add_pos ()) }
  | CIDE LCBR forms RCBR                    { Big_ion ($1, $3, add_pos ()) }	
  | CIDE LPAR acts RPAR                     { Big_ion_fun ($1, [], $3, add_pos ()) }
  | CIDE LPAR acts RPAR LCBR forms RCBR     { Big_ion_fun ($1, $6, $3, add_pos ()) }
/*  | error                                   { parse_error_msg "Invalid ion" ""; 
                                              raise Parsing.Parse_error}		*/    
  ;

id_exp
  : ID                                      { Big_id (1, [], add_pos ()) }
  | ID LCBR forms RCBR                      { Big_id (0, $3, add_pos ()) }	
  | ID LPAR NUM RPAR                        { try
						let n = int_of_string $3 in
						if n >= 0 then Big_id (n, [], add_pos ())
						else (parse_error_msg "Invalid identity" $3;
						      raise Parsing.Parse_error)
                                              with
					      | Failure _ ->
						  parse_error_msg "Invalid int" $3; 
                                                  raise Parsing.Parse_error
					    }
  | ID LPAR NUM COMMA LCBR forms RCBR RPAR  { try
						let n = int_of_string $3 in
						if n >= 0 then Big_id (n, $6, add_pos ())
						else (parse_error_msg "Invalid identity" $3;
						      raise Parsing.Parse_error)
                                              with
					      | Failure _ ->
						  parse_error_msg "Invalid int" $3; 
                                                  raise Parsing.Parse_error
					    }
 /* | error                                   { parse_error_msg "Invalid identity" ""; 
                                              raise Parsing.Parse_error}		*/    
  ;

closures
  : closure                                 { $1 }
  | closure simple_exp                      { Big_comp_c ($1, $2, add_pos ()) }
/*  | error                                   { parse_error_msg "Invalid closure" ""; 
                                              raise Parsing.Parse_error} */		    
  ; 

closure
  : SLASH IDE                               { Big_close ([ $2 ], add_pos ()) }
  | SLASH IDE closure                       { add_closure $2 $3 }
  ;
						 
%%

