%{

open Loc 
open Ast
open Cmd
open Bigraph

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
%token            INIT
%token            RULES
%token		  PREDS
%token            INT
%token            FLOAT
%token            FUN
%token            ACTION
%token            BEGIN
%token            BRS
%token            PBRS
%token            SBRS
%token            NBRS
%token            END
%token		  MERGE
%token		  SPLIT
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
%token <string> PATH
%token <Ast.const list> O_CONST
%token <Cmd.format_op list> O_FORMAT

%token   F_SVG
%token   F_DOT
%token   F_TXT
%token 	 F_JSON
%token   C_CHECK
%token   C_FULL
%token   C_SIM
%token   O_CONF
%token   O_COLORS
%token   O_VERS
%token   O_HELP
%token   O_VERB
%token   O_QUIET
%token   O_DEBUG
%token   O_DECS
%token   O_TS
%token   O_STATES
%token   O_LABELS
%token   O_PRISM
%token   O_STATE_REWARDS
%token   O_ML 
%token   O_MAX
%token   O_TIME
%token   O_STEPS
   
%start model cmd consts format
%type <Ast.model> model
%type <Cmd.cmd_t> cmd
%type <Ast.const list> consts
%type <Cmd.format_op list> format
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
  | dec_action                              { Daction $1 }

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

dec_reacts:
  | dec_react SEMICOLON                     { $1 }

dec_action:
  | ACTION IDE reward rules=nonempty_list(dec_reacts) END
    { { action_id = $2; action_rules = rules; action_reward = $3 } };

ctrl_exp:
  | CTRL CIDE EQUAL CINT                    { Ctrl_exp ($2, $4, loc $startpos $endpos)         }
  | FUN CTRL CIDE LPAR ide_list_nonempty RPAR EQUAL CINT
                                            { Ctrl_fun_exp ($3, $5, $8, loc $startpos $endpos) };

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
  | FUN BIG IDE LPAR ide_list_nonempty RPAR EQUAL bexp  
                                            { Big_fun_exp ($3, $5, $8, loc $startpos $endpos) };

dec_react:
  | REACT IDE EQUAL bexp arrow bexp eta_exp_opt 
      { React_exp ($2, "", 0, $4, $6, $5, $7, loc $startpos $endpos)           }
 | FUN REACT IDE LPAR ide_list_nonempty RPAR EQUAL bexp arrow bexp eta_exp_opt
      { React_fun_exp ($3, "", 0, $5, $8, $10, $9, $11, loc $startpos $endpos) }

arrow:
  | ARR                   { None }
  | LARR float_exp RARR   { Some $2 }

eta_exp_opt:
  eta = option(eta_exp)                     { eta };

eta_exp:
  | AT LSBR int_list RSBR                   { ($3, loc $startpos $endpos) };

int_list:
  ints = separated_list(COMMA, CINT)        { ints };

rs:
  | BEGIN rs_type params init rules preds END
      { { dbrs_type = $2; 
          dbrs_pri = $5;
	  dbrs_init = $4;
	  dbrs_params = $3;
	  dbrs_preds = $6;
	  dbrs_loc = loc $startpos $endpos; } };

rs_type:
  | BRS   { Rs.BRS }
  | PBRS  { Rs.PBRS }
  | SBRS  { Rs.SBRS }
  | NBRS  { Rs.NBRS };

preds:
  |					    { [ ] }
  | PREDS EQUAL LCBR pred_list RCBR SEMICOLON
                                            { $4 };

pred_list:
  l = separated_nonempty_list(COMMA, pred_ide)
                                            { l };

reward:
  |                                         { None    }
  | LSBR int_exp RSBR                       { Some $2 };

pred_ide:
  | IDE reward                              { Pred_id ($1, $2, loc $startpos $endpos)         }
  | IDE LPAR num_list RPAR reward           { Pred_id_fun ($1, $3, $5, loc $startpos $endpos) };

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

priority_class:
  | LCBR rule_ide_list RCBR                { Pr ($2, loc $startpos $endpos)     }
  | LPAR rule_ide_list RPAR                { Pr_red ($2, loc $startpos $endpos) };

rules:
  | RULES EQUAL LSBR priority_list RSBR SEMICOLON           { $4 };

rule_ide_list:
  l = separated_nonempty_list(COMMA, rule_ide)              { l };

rule_ide:
  | IDE                                     { Rul_id ($1, loc $startpos $endpos)         }
  | IDE LPAR num_list RPAR                  { Rul_id_fun ($1, $3, loc $startpos $endpos) };

bexp:
  | wire_exp LPAR bexp RPAR                 { Big_wire ($1, $3, loc $startpos $endpos)                   }
  | wire_exp ion_exp                        { Big_wire ($1, Big_ion $2, loc $startpos $endpos)           }
  | wire_exp ion_exp DOT simple_bexp        { Big_wire ($1, Big_nest ($2, $4, loc $startpos $endpos), 
							    loc $startpos $endpos)                       }
  | wire_exp IDE                            { Big_wire ($1, Big_var ($2, loc $startpos $endpos),
                                                            loc $startpos $endpos)                       }
  | wire_exp IDE LPAR num_list RPAR         { Big_wire ($1, Big_var_fun ($2, $4,
                                                                             loc $startpos $endpos),
                                                            loc $startpos $endpos)                       }
  | simple_bexp                             { $1                                                         }
  | bexp PROD bexp                          { Big_comp ($1, $3, loc $startpos $endpos)                   }
  | bexp PLUS bexp                          { Big_tens ($1, $3, loc $startpos $endpos)                   }
  | bexp PIPE bexp                          { Big_par ($1, $3, loc $startpos $endpos)                    }
  | bexp DPIPE bexp                         { Big_ppar ($1, $3, loc $startpos $endpos)                   }
  | SHARE simple_bexp BY simple_bexp IN simple_bexp
                                            { Big_share ($2, $4, $6, loc $startpos $endpos)              };  

simple_bexp:
  | LPAR bexp RPAR                          { $2                                          }
  | LPAR bexp RPAR LPAR bexp RPAR           { Big_comp ($2, $5, loc $startpos $endpos)    }	
  | id_exp                                  { Big_id $1                                   }
  | merge_exp                               { $1                                          } 
  | split_exp                               { $1                                          }
  | SLASH IDE                               { Big_close { cl_name = $2; 
					                  cl_loc = loc $startpos $endpos;}}
  | IDE SLASH LCBR ide_list_nonempty RCBR   { Big_sub { out_name = $1;
                                                        in_names = $4;
                                                        sub_loc = loc $startpos $endpos; }}
  | LCBR IDE RCBR                           { Big_new_name ($2, loc $startpos $endpos)    }
  | CINT                                    { Big_num ($1, loc $startpos $endpos)         }
  | place_exp                               { Big_plc $1                                  }
  | IDE                                     { Big_var ($1, loc $startpos $endpos)         }
  | IDE LPAR num_list RPAR                  { Big_var_fun ($1, $3, loc $startpos $endpos) }
  | ion_exp                                 { Big_ion $1                                  }
  | ion_exp DOT simple_bexp                 { Big_nest ($1, $3, loc $startpos $endpos)    };

id_exp:
  | ID o_delim_int_1                        { { id_place = $2;
						id_link = [];
						id_loc = loc $startpos $endpos; } } 
  | ID LCBR ide_list_nonempty RCBR          { { id_place = 0;
						id_link = $3;
						id_loc = loc $startpos $endpos; } }
  | ID LPAR CINT COMMA LCBR ide_list_nonempty RCBR RPAR  
                                            { { id_place = $3;
						id_link = $6;
						id_loc = loc $startpos $endpos; } };

merge_exp:
  | MERGE o_delim_int_2                     { Big_merge ($2, loc $startpos $endpos) };

split_exp:
  | SPLIT o_delim_int_2                     { Big_split ($2, loc $startpos $endpos) };

o_delim_int_1:
  n = option(delimited(LPAR,CINT,RPAR))     { match n with
                                               | None -> 1
					       | Some n -> n };

o_delim_int_2:
  n = option(delimited(LPAR,CINT,RPAR))     { match n with
                                               | None -> 2
					       | Some n -> n };
ion_exp:
  | CIDE                                    { Big_ion_exp ($1, [], loc $startpos $endpos)         }
  | CIDE LCBR ide_list_nonempty RCBR        { Big_ion_exp ($1, $3, loc $startpos $endpos)         }
  | CIDE LPAR num_list RPAR                 { Big_ion_fun_exp ($1, $3, [], loc $startpos $endpos) }
  | CIDE LPAR num_list RPAR LCBR ide_list_nonempty RCBR  
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


wire_exp:
  | closures                                { Close_exp $1  }
  | IDE SLASH LCBR ide_list_nonempty RCBR   { Sub_exp { out_name = $1;
                                                in_names = $4;
                                                sub_loc = loc $startpos $endpos; }}
  | SLASH LCBR ide_list_nonempty RCBR       { Merge_close_exp {
                                                m_cl_names = $3;
                                                m_cl_loc = loc $startpos $endpos; }};

closures: 
  | cs = nonempty_list(closure)             { cs }; 

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

consts:
  | l=separated_nonempty_list(COMMA, const) EOF
    { l };

common_opt:
  | O_COLORS
      { defaults.colors <- false}
  | O_VERB
      { defaults.verb <- true }
  | O_QUIET
      { defaults.quiet <- true }
  | O_DEBUG
      { defaults.debug <- true }
  | O_CONST 
      { defaults.consts <- $1 };

format:
  | l=separated_nonempty_list(COMMA, ext) EOF
    { l };

ext:
  | F_SVG  { Svg }
  | F_JSON { Json }
  | F_DOT  { Dot }
  | F_TXT  { Txt };

export_opt:
  | O_TS PATH
     { defaults.export_graph <- Some $2 } 
  | O_LABELS PATH
     { defaults.export_lab <- Some $2 } 
  | O_STATES option(PATH)
     {  defaults.export_states_flag <- true;
     	defaults.export_states <- $2 }
  | O_PRISM PATH
     { defaults.export_prism <- Some $2 }
  | O_STATE_REWARDS PATH
     { defaults.export_state_rewards <- Some $2 }
  | common_export_opt
    { $1 };

common_export_opt:   
  | O_DECS PATH
     { defaults.export_decs <- Some $2 }
  | O_ML PATH
     { defaults.export_ml <- Some $2 }
  | O_FORMAT
     { defaults.out_format <- $1 };

opt_chk:
  | common_opt
    { $1 }
  | common_export_opt
    { $1 };
  
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
    { defaults.time <- $2;
      defaults.time_flag <- true }
  | O_STEPS CINT
    { defaults.steps <- $2;
      defaults.steps_flag <- true };

sub_cmd:
  | C_CHECK O_HELP
      { eval_help_check Format.std_formatter () }
  | C_FULL O_HELP
      { eval_help_full Format.std_formatter () }
  | C_SIM O_HELP
      { eval_help_sim Format.std_formatter () }
  | C_CHECK list(opt_chk) input_file
      { List.iter (fun x-> x) $2;
        check_dot ();
	check_states ();
	defaults.model <- $3;
        `check }
  | C_FULL list(opt_full) input_file
      { List.iter (fun x-> x) $2;
        check_dot ();
	check_states ();      
	defaults.model <- $3;
        `full }
  | C_SIM list(opt_sim) input_file
      { List.iter (fun x-> x) $2;
        check_dot ();
	check_states ();      
	defaults.model <- $3;
        `sim };

input_file:
  | { None }
(*  | MINUS { None } *)
  | BIG_FILE { Some $1 };
%%

