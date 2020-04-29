%{

open Loc
open Ast
open Bigraph

%}

(* BIG *)

%token            EOF

%token <string>   CIDE
%token <string>   IDE       
%token <int>      CINT
%token <float>    CFLOAT
%token <string>   CSTRING

%token 	          CTRL
%token            ATOMIC
%token            BIG
%token            REACT
%token            INIT
%token            RULES
%token		  PREDS
%token            INT
%token            FLOAT
%token            STRING
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
%token            PAR
%token            PPAR
%token            IF
%token            CTX
%token            PARAM
%token            BANG
%token            LSBR RSBR LCBR RCBR LPAR RPAR
%token            COLON SEMICOLON COMMA 

%token            PIPE DPIPE
%token            DOT 

%left  PIPE DPIPE 
%left  PLUS MINUS
%left  PROD SLASH
%right CARET

%start model
%type <Ast.model> model

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
  | dec_str SEMICOLON                       { Dstr $1  }
  | dec_ctrl SEMICOLON                      { Dctrl $1   }
  | dec_big SEMICOLON                       { Dbig $1    }
  | dec_react SEMICOLON                     { Dreact $1  }
  | dec_action                              { Daction $1 }

dec_gen:
  IDE EQUAL exp                          { { d_id = $1;
                                             d_exp = $3;
						                                 d_loc = loc $startpos $endpos;} };

dec_int:
  INT dec_gen { $2 };

dec_float:
  FLOAT dec_gen { $2 };

dec_str:
  STRING dec_gen { $2 };

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

dec_big:
  | BIG IDE EQUAL bexp                      { Big_exp ($2, $4, loc $startpos $endpos)         }
  | FUN BIG IDE LPAR ide_list_nonempty RPAR EQUAL bexp
                                            { Big_fun_exp ($3, $5, $8, loc $startpos $endpos) };

dec_react:
  | REACT IDE EQUAL bexp arrow bexp eta_exp_opt option(conds)
      { React_exp ($2, "", 0, $4, $6, $5, $7, $8, loc $startpos $endpos)           }
  | FUN REACT IDE LPAR ide_list_nonempty RPAR EQUAL bexp arrow bexp eta_exp_opt option(conds)
      { React_fun_exp ($3, "", 0, $5, $8, $10, $9, $11, $12, loc $startpos $endpos) };

arrow:
  | ARR                   { None }
  | LARR exp RARR         { Some $2 }

eta_exp_opt:
  eta = option(eta_exp)                     { eta };

conds:
  IF; conds = separated_list(COMMA, cond)      { conds };

cond:
  option(is_bang) bexp IN place { { neg = $1; pred = $2; place = $4; loc = loc $startpos $endpos } };

is_bang:
  | BANG { true }

place:
  | PARAM { Cond_Param }
  | CTX   { Cond_Ctx   }

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
  | LSBR exp RSBR                           { Some $2 };

pred_ide:
  | IDE reward                              { Pred_id ($1, $2, loc $startpos $endpos)         }
  | IDE LPAR exp_list RPAR reward           { Pred_id_fun ($1, $3, $5, loc $startpos $endpos) };

init:
  | INIT IDE SEMICOLON                      { Init ($2, loc $startpos $endpos)         }
  | INIT IDE LPAR exp_list RPAR SEMICOLON   { Init_fun ($2, $4, loc $startpos $endpos) };

exp_list:
  exps = separated_nonempty_list(COMMA, exp)	        { exps };

exp:
  | num_exp { ENum $1 }
  | str_exp { EStr $1 }
  | var_exp { EVar $1 }
  | op_exp  { EOp $1 }
  | LPAR exp RPAR { $2 };

num_exp:
  | CINT                                    { Num_int_val ($1, loc $startpos $endpos)   }
  | CFLOAT                                  { Num_float_val ($1, loc $startpos $endpos) };

var_exp:
  | IDE                                     { Var ($1, loc $startpos $endpos) };

str_exp:
  | CSTRING                                 { Str_val ($1, loc $startpos $endpos) };

op_exp:
  | exp PLUS exp                            { Plus ($1, $3, loc $startpos $endpos)  }
  | exp MINUS exp                           { Minus ($1, $3, loc $startpos $endpos) }
  | MINUS exp                               { UMinus ($2, loc $startpos $endpos) }
  | exp PROD exp                            { Prod ($1, $3, loc $startpos $endpos)  }
  | exp SLASH exp                           { Div ($1, $3, loc $startpos $endpos)   }
  | exp CARET exp                           { Pow ($1, $3, loc $startpos $endpos)   };

params:
  p = list(param)                           { p };

ide_list_nonempty:
  ides = separated_nonempty_list(COMMA, IDE)                { ides };

param:
  | INT ide_list_nonempty EQUAL param_int_exp SEMICOLON     { Param_int ($2, $4, loc $startpos $endpos)   }
  | FLOAT ide_list_nonempty EQUAL param_float_exp SEMICOLON { Param_float ($2, $4, loc $startpos $endpos) }
  | STRING ide_list_nonempty EQUAL param_str_exp SEMICOLON  { Param_str ($2, $4, loc $startpos $endpos) };

param_int_exp:
  | exp                                 { Param_int_val ($1, loc $startpos $endpos)           }
  | LCBR exp_list RCBR                  { Param_int_set ($2, loc $startpos $endpos)           }
  | LSBR exp COLON exp COLON exp RSBR
                                            { Param_int_range ($2, $4, $6, loc $startpos $endpos) };

param_float_exp:
  | exp                               { Param_float_val ($1, loc $startpos $endpos)           }
  | LCBR exp_list RCBR                { Param_float_set ($2, loc $startpos $endpos)           }
  | LSBR exp COLON exp COLON exp RSBR { Param_float_range ($2, $4, $6, loc $startpos $endpos) };

param_str_exp:
  | exp                              { Param_str_val ($1, loc $startpos $endpos)           }
  | LCBR exp_list RCBR               { Param_str_set ($2, loc $startpos $endpos)           };

priority_list:
  l = separated_list(COMMA, priority_class)        { l };

priority_class:
  | LCBR rule_ide_list RCBR                { Pr ($2, loc $startpos $endpos)     }
  | LPAR rule_ide_list RPAR                { Pr_red ($2, loc $startpos $endpos) };

rules:
  | RULES EQUAL LSBR priority_list RSBR SEMICOLON           { $4 };

rule_ide_list:
  l = separated_nonempty_list(COMMA, rule_ide)              { l };

rule_ide:
  | IDE                                     { Rul_id ($1, loc $startpos $endpos)         }
  | IDE LPAR exp_list RPAR                  { Rul_id_fun ($1, $3, loc $startpos $endpos) };

bexp:
  | wire_exp LPAR bexp RPAR                 { Big_wire ($1, $3, loc $startpos $endpos)                   }
  | wire_exp ion_exp                        { Big_wire ($1, Big_ion $2, loc $startpos $endpos)           }
  | wire_exp ion_exp DOT simple_bexp        { Big_wire ($1, Big_nest ($2, $4, loc $startpos $endpos),
							    loc $startpos $endpos)                       }
  | wire_exp IDE                            { Big_wire ($1, Big_var ($2, loc $startpos $endpos),
                                                            loc $startpos $endpos)                       }
  | wire_exp IDE LPAR exp_list RPAR         { Big_wire ($1, Big_var_fun ($2, $4,
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
  | IDE LPAR exp_list RPAR                  { Big_var_fun ($1, $3, loc $startpos $endpos) }
  | ion_exp                                 { Big_ion $1                                  }
  | ion_exp DOT simple_bexp                 { Big_nest ($1, $3, loc $startpos $endpos)    }
  | PAR LPAR exp COMMA bexp RPAR
                                            { Big_par_fn ($3, $5, loc $startpos $endpos)  }
  | PPAR LPAR exp COMMA bexp RPAR
                                            { Big_ppar_fn ($3, $5, loc $startpos $endpos) };

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
  | CIDE LPAR exp_list RPAR                 { Big_ion_fun_exp ($1, $3, [], loc $startpos $endpos) }
  | CIDE LPAR exp_list RPAR LCBR ide_list_nonempty RCBR
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
