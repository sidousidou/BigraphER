%{

open Loc 
open Ast
open Cmd
       
%}

(* BIG *)
   
%token            EOF

%token <string>   CIDE
%token <string>   IDE       
%token <int>      CINT
%token <float>    CFLOAT     

%token 	          CTRL 
%token            ATOMIC
%token            BIG
%token            REACT
%token            SREACT
%token            INIT
%token            RULES
%token            INT
%token            FLOAT
%token            FUN
%token            BRS
%token            ENDBRS
%token            SBRS
%token            ENDSBRS 
(* %token            SORT TRUE FALSE NOT *)
%token            SHARE
%token            BY
%token            IN
%token            ID 
%token            ARR
%token            LARR
%token            RARR
%token            AT
%token            EQUAL
%token            PLUS
%token            MINUS
%token            PROD
%token            SLASH
%token            CARET

%token            LSBR RSBR LCBR RCBR LPAR RPAR 
%token            COLON SEMICOLON COMMA 

%token            PIPE DPIPE
%token            DOT 

%left  PIPE DPIPE 
%left  PLUS MINUS
%left  PROD SLASH
%right CARET

(* CMD *)

%token <string> BIG_FILE
%token <string> BILOG_FILE
%token <string> PATH

%token   F_SVG
%token   F_DOT
%token   C_CHECK
%token   C_FULL
%token   C_SIM
%token   O_CONF
%token   O_VERS
%token   O_HELP
%token   O_VERB
%token   O_QUIET
%token   O_CONST
%token   O_DEBUG
%token   O_DECS
%token   O_FORMAT
%token   O_TS
%token   O_STATES
%token   O_LABELS
%token   O_PRISM 
%token   O_MAX
%token   O_TIME
%token   O_STEPS
   
%start model cmd
%type <Ast.model> model
%type <Cmd.cmd_t> cmd 
(* %type <Ast.predicates> pred_list *)

%%

model:
  dec_list rs EOF                           { { model_decs = $1;
						model_rs = $2;
						model_loc = loc $startpos $endpos;} };

dec_list:
  decs = nonempty_list(dec)                 { decs };
     
dec:
  | dec_int SEMICOLON                       { Dint $1    }
  | dec_float SEMICOLON                     { Dfloat $1  }
  | dec_ctrl SEMICOLON                      { Dctrl $1   }
  | dec_big SEMICOLON                       { Dbig $1    }
  | dec_react SEMICOLON                     { Dreact $1  }
  | dec_sreact SEMICOLON                    { Dsreact $1 };

rs:
  | brs                                     { Dbrs $1  }
  | sbrs                                    { Dsbrs $1 };

dec_int:
  INT IDE EQUAL int_exp                     { { dint_id = $2;
						dint_exp = $4;
						dint_loc = loc $startpos $endpos;} };

dec_float:
  FLOAT IDE EQUAL float_exp                 { { dfloat_id = $2;
						dfloat_exp = $4;
						dfloat_loc = loc $startpos $endpos;} };

dec_ctrl:
  | ATOMIC ctrl_exp                         { Atomic ($2, loc $startpos $endpos)     }
  | ctrl_exp                                { Non_atomic ($1, loc $startpos $endpos) };

ctrl_exp:
  | CTRL CIDE EQUAL CINT                    { Ctrl_exp ($2, $4, loc $startpos $endpos)         }
  | FUN CTRL CIDE LPAR ide_list RPAR EQUAL CINT
                                            { Ctrl_fun_exp ($3, $5, $8, loc $startpos $endpos) };

ide_list:
  ides = separated_list(COMMA, IDE)         { ides };
 
int_exp:
  | CINT                                    { Int_val ($1, loc $startpos $endpos)       }
  | IDE                                     { Int_var ($1, loc $startpos $endpos)       } 
  | LPAR int_exp RPAR                       { $2                                        }
  | int_exp PROD int_exp                    { Int_prod ($1, $3, loc $startpos $endpos)  }     
  | int_exp SLASH int_exp                   { Int_div ($1, $3, loc $startpos $endpos)   }     
  | int_exp PLUS int_exp                    { Int_plus ($1, $3, loc $startpos $endpos)  }     
  | int_exp MINUS int_exp                   { Int_minus ($1, $3, loc $startpos $endpos) };     

float_exp:
  | CFLOAT                                  { Float_val ($1, loc $startpos $endpos)       }
  | IDE                                     { Float_var ($1, loc $startpos $endpos)       } 
  | LPAR float_exp RPAR                     { $2                                          }
  | float_exp CARET float_exp               { Float_pow ($1, $3, loc $startpos $endpos)   }
  | float_exp PROD float_exp                { Float_prod ($1, $3, loc $startpos $endpos)  }     
  | float_exp SLASH float_exp               { Float_div ($1, $3, loc $startpos $endpos)   }     
  | float_exp PLUS float_exp                { Float_plus ($1, $3, loc $startpos $endpos)  }     
  | float_exp MINUS float_exp               { Float_minus ($1, $3, loc $startpos $endpos) };

dec_big:
  | BIG IDE EQUAL bexp                      { Big_exp ($2, $4, loc $startpos $endpos)         }
  | FUN BIG IDE LPAR ide_list RPAR EQUAL bexp  
                                            { Big_fun_exp ($3, $5, $8, loc $startpos $endpos) };

dec_react:
  | REACT IDE EQUAL bexp ARR bexp eta_exp_opt 
                                            { React_exp ($2, $4, $6, $7, loc $startpos $endpos)           }
  | FUN REACT IDE LPAR ide_list RPAR EQUAL bexp ARR bexp eta_exp_opt
                                            { React_fun_exp ($3, $5, $8, $10, $11, loc $startpos $endpos) };

eta_exp_opt:
  eta = option(eta_exp)                     { eta };

eta_exp:
  | AT LSBR int_list RSBR                   { ($3, loc $startpos $endpos) };

int_list:
  ints = separated_list(COMMA, CINT)        { ints };

dec_sreact:  
  | SREACT IDE EQUAL bexp LARR float_exp RARR bexp eta_exp_opt 
                                            { Sreact_exp ($2, $4, $8, $9, $6, loc $startpos $endpos)            }
  | FUN SREACT IDE LPAR ide_list RPAR EQUAL bexp LARR float_exp RARR bexp eta_exp_opt
                                            { Sreact_fun_exp ($3, $5, $8, $12, $13, $10, loc $startpos $endpos) };

brs:
  | BRS params init rules ENDBRS            { { dbrs_pri = $4;
						dbrs_init = $3;
						dbrs_params = $2;
						dbrs_loc = loc $startpos $endpos; } };

sbrs: 
  | SBRS params init srules ENDSBRS         { { dsbrs_pri = $4;
					        dsbrs_init = $3;
						dsbrs_params = $2;
						dsbrs_loc = loc $startpos $endpos; } };
    
init:
  | INIT IDE SEMICOLON                      { Init ($2, loc $startpos $endpos)         }
  | INIT IDE LPAR num_list RPAR SEMICOLON   { Init_fun ($2, $4, loc $startpos $endpos) };

num_list:
  nums = separated_nonempty_list(COMMA, num_exp)	    { nums };

num_exp:
  | CINT                                    { Num_int_val ($1, loc $startpos $endpos)   }
  | CFLOAT                                  { Num_float_val ($1, loc $startpos $endpos) }
  | IDE                                     { Num_var ($1, loc $startpos $endpos)       } 
  | LPAR num_exp RPAR                       { $2                                        }
  | num_exp CARET num_exp                   { Num_pow ($1, $3, loc $startpos $endpos)   }
  | num_exp PROD num_exp                    { Num_prod ($1, $3, loc $startpos $endpos)  }     
  | num_exp SLASH num_exp                   { Num_div ($1, $3, loc $startpos $endpos)   }     
  | num_exp PLUS num_exp                    { Num_plus ($1, $3, loc $startpos $endpos)  }     
  | num_exp MINUS num_exp                   { Num_minus ($1, $3, loc $startpos $endpos) };

params:
  p = list(param)                           { p };

ide_list_nonempty:
  ides = separated_nonempty_list(COMMA, IDE)                { ides };

param:
  | INT ide_list_nonempty EQUAL param_int_exp SEMICOLON     { Param_int ($2, $4, loc $startpos $endpos)   }
  | FLOAT ide_list_nonempty EQUAL param_float_exp SEMICOLON { Param_float ($2, $4, loc $startpos $endpos) };

int_exp_list:
  l = separated_nonempty_list(COMMA, int_exp)               { l };

float_exp_list:
  l = separated_nonempty_list(COMMA, float_exp)             { l };

param_int_exp:
  | int_exp                                 { Param_int_val ($1, loc $startpos $endpos)           }
  | LCBR int_exp_list RCBR                  { Param_int_set ($2, loc $startpos $endpos)           }
  | LSBR int_exp COLON int_exp COLON int_exp RSBR		
                                            { Param_int_range ($2, $4, $6, loc $startpos $endpos) };

 param_float_exp:
  | float_exp                               { Param_float_val ($1, loc $startpos $endpos)           }
  | LCBR float_exp_list RCBR                { Param_float_set ($2, loc $startpos $endpos)           }
  | LSBR float_exp COLON float_exp COLON float_exp RSBR		
                                            { Param_float_range ($2, $4, $6, loc $startpos $endpos) };

priority_list:
  l = separated_nonempty_list(COMMA, priority_class)        { l };
spriority_list:
  l = separated_nonempty_list(COMMA, spriority_class)       { l };

priority_class:
  | LCBR rule_ide_list RCBR                { Pr ($2, loc $startpos $endpos)     }
  | LPAR rule_ide_list RPAR                { Pr_red ($2, loc $startpos $endpos) };

rules:
  | RULES EQUAL LSBR priority_list RSBR SEMICOLON           { $4 };

srules:
  | RULES EQUAL LSBR spriority_list RSBR SEMICOLON          { $4 };

rule_ide_list:
  l = separated_nonempty_list(COMMA, rule_ide)              { l };

rule_ide:
  | IDE                                     { Rul_id ($1, loc $startpos $endpos)         }
  | IDE LPAR num_list RPAR                  { Rul_id_fun ($1, $3, loc $startpos $endpos) };

spriority_class:
  | LCBR srule_ide_list RCBR                { Spr ($2, loc $startpos $endpos)     }
  | LPAR srule_ide_list RPAR                { Spr_red ($2, loc $startpos $endpos) };

srule_ide_list:
  l = separated_nonempty_list(COMMA, srule_ide)             { l };

srule_ide:
  | IDE                                     { Srul_id ($1, loc $startpos $endpos)         }
  | IDE LPAR num_list RPAR                  { Srul_id_fun ($1, $3, loc $startpos $endpos) };

bexp:
  | simple_bexp                             { $1                                                         }
  | closure_list LPAR bexp RPAR             { Big_closures ($1, $3, loc $startpos $endpos)               }
  | closure_list ion_exp                    { Big_closures ($1, Big_ion $2, loc $startpos $endpos)       }
  | closure_list ion_exp DOT simple_bexp    { Big_closures ($1, Big_nest ($2, $4, loc $startpos $endpos), 
							    loc $startpos $endpos)                       }
  | bexp PROD bexp                          { Big_comp ($1, $3, loc $startpos $endpos)                   }
  | bexp PLUS bexp                          { Big_tens ($1, $3, loc $startpos $endpos)                   }
  | bexp PIPE bexp                          { Big_par ($1, $3, loc $startpos $endpos)                    }
  | bexp DPIPE bexp                         { Big_ppar ($1, $3, loc $startpos $endpos)                   }
  | SHARE simple_bexp BY simple_bexp IN simple_bexp
                                            { Big_share ($2, $4, $6, loc $startpos $endpos)              };  

simple_bexp:
  | LPAR bexp RPAR                          { $2                                          } 
  | id_exp                                  { Big_id $1                                   }
  | closure                                 { Big_close $1                                }
  | CINT                                    { Big_num ($1, loc $startpos $endpos)         }
  | LCBR IDE RCBR                           { Big_new_name ($2, loc $startpos $endpos)    }
  | place_exp                               { Big_plc $1                                  }
  | IDE                                     { Big_var ($1, loc $startpos $endpos)         }
  | IDE LPAR num_list RPAR                  { Big_var_fun ($1, $3, loc $startpos $endpos) }
  | ion_exp                                 { Big_ion $1                                  }
  | ion_exp DOT simple_bexp                 { Big_nest ($1, $3, loc $startpos $endpos)    };

id_exp:
  | ID                                      { { id_place = 1;
						id_link = [];
						id_loc = loc $startpos $endpos; } } 
  | ID LCBR ide_list RCBR                   { { id_place = 0;
						id_link = $3;
						id_loc = loc $startpos $endpos; } }
  | ID LPAR CINT RPAR                       { { id_place = $3;
						id_link = [];
						id_loc = loc $startpos $endpos; } }
  | ID LPAR CINT COMMA LCBR ide_list RCBR RPAR  
                                            { { id_place = $3;
						id_link = $6;
						id_loc = loc $startpos $endpos; } };

ion_exp:
  | CIDE                                    { Big_ion_exp ($1, [], loc $startpos $endpos)         }
  | CIDE LCBR ide_list RCBR                 { Big_ion_exp ($1, $3, loc $startpos $endpos)         }	
  | CIDE LPAR num_list RPAR                 { Big_ion_fun_exp ($1, $3, [], loc $startpos $endpos) }
  | CIDE LPAR num_list RPAR LCBR ide_list RCBR  
                                            { Big_ion_fun_exp ($1, $3, $6, loc $startpos $endpos) };

place_exp:
  | LPAR LSBR int_list_list RSBR COMMA CINT RPAR   
                                            { { plc_parents = $3;
						plc_roots = $6;
						plc_loc = loc $startpos $endpos; } };

int_list_list
  : /* EMPTY */                             { [ ] }
  | LCBR int_list RCBR                      { [ $2 ] }				
  | LCBR int_list RCBR COMMA int_list_list  { $2 :: $5 }
    
closure_list:
  l = nonempty_list(closure)                { l }; 

closure:
  | SLASH IDE                               { { cl_name = $2; 
						cl_loc = loc $startpos $endpos; } };
		
(* COMMAND LINE *)

cmd:
  | sub_cmd EOF                             { $1 }
  | stand_alone_opt EOF                     { $1; exit 0 }

stand_alone_opt:
  | O_CONF
      { eval_config Format.std_formatter () }
  | O_VERS
      { eval_version Format.std_formatter () }
  | O_HELP
      { eval_help_top Format.std_formatter () };

const:
| IDE EQUAL CINT
      { Cint { dint_id = $1;
        dint_exp = Int_val ($3, loc $startpos $endpos);
	dint_loc = loc $startpos $endpos; } }
| IDE EQUAL CFLOAT
      { Cfloat { dfloat_id = $1;
        dfloat_exp = Float_val ($3, loc $startpos $endpos);
	dfloat_loc = loc $startpos $endpos; } };

common_opt:
  | O_VERB
      { defaults.verb <- true }
  | O_QUIET
      { defaults.quiet <- true }
  | O_DEBUG
      { defaults.debug <- true }
  | O_CONST l=separated_nonempty_list(COMMA, const)
      { defaults.consts <- l };

ext:
  | F_SVG { Svg }
  | F_DOT { Dot };

export_opt:
  | O_DECS PATH
      { defaults.export_decs <- Some $2 }
  | O_TS PATH
      { defaults.export_graph <- Some $2 }
  | O_LABELS PATH
      { defaults.export_lab <- Some $2 }
  | O_STATES option(PATH)
      { defaults.export_states_flag <- true;
	defaults.export_states <- $2 }
  | O_PRISM PATH
      { defaults.export_prism <- Some $2 }
  | O_FORMAT l=separated_nonempty_list(COMMA, ext)
      { defaults.out_format <- l };

opt_chk:
  | common_opt
      { $1 }
  | O_DECS PATH
      { defaults.export_decs <- Some $2 }
  | O_FORMAT l=separated_nonempty_list(COMMA, ext)
      { defaults.out_format <- l };
  
opt_full:
  | common_opt
      { $1 }
  | export_opt
      { $1 }
  | O_MAX CINT
      { defaults.max_states <- $2 };

opt_sim:
  | common_opt
      { $1 }
  | export_opt
      { $1 }
  | O_TIME CFLOAT
      { defaults.time <- $2 }
  | O_STEPS CINT
      { defaults.steps <- $2 };

sub_cmd:
  | C_CHECK O_HELP
      { eval_help_check Format.std_formatter () }
  | C_FULL O_HELP
      { eval_help_full Format.std_formatter () }
  | C_SIM O_HELP
      { eval_help_sim Format.std_formatter () }
  | C_CHECK list(opt_chk) BIG_FILE option(BILOG_FILE)
      { List.iter (fun x-> x) $2;
	defaults.model <- $3;
	defaults.pred <- $4;
        `check }
  | C_FULL list(opt_full) BIG_FILE option(BILOG_FILE)
      { List.iter (fun x-> x) $2;
	defaults.model <- $3;
	defaults.pred <- $4;
        `full }
  | C_SIM list(opt_sim) BIG_FILE option(BILOG_FILE)
      { List.iter (fun x-> x) $2;
	defaults.model <- $3;
	defaults.pred <- $4;
        `sim };

%%

