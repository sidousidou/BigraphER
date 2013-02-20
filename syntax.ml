open Base
open Big
open Brs

exception Parse_failure of string

(*****************************************************************************)
(******************************* PRIORITIES **********************************)
(*****************************************************************************)

type var_dec = 
	| Int_dec of string * int list
	| Float_dec of string * float list

type rule = 
	| Rul of string * string list


type pri_class =
	| Pri_class of var_dec list * rule list
	| Pri_classr of var_dec list * rule list

(*****************************************************************************)
(********************************** MODEL ************************************)
(*****************************************************************************)

type num_exp =
	| Num_v of string
	| Num_ide of string
	| Plus of num_exp * num_exp
	| Minus of num_exp * num_exp
	| Prod of num_exp * num_exp
	| Div of num_exp * num_exp
	| Pow of num_exp * num_exp

type init =
	Init of string * num_exp list

type brs = 
	| Brs of var_dec list * init * rule list
	| Sbrs of var_dec list * init * rule list

type rexp = 
	| Rate of string * num_exp list

type bexp =
	| Big_ide of string * num_exp list             (* B *)
	| Big_plac of int list list * int              (* ({{0,2,3},{}} ,5) *)
	| Big_comp of string list * bexp               (* composition *)
	| Big_close of string													 (* closure *)
	| Big_par of bexp * bexp                       (* merge parallel *)
	| Big_ppar of bexp * bexp                      (* parallel *)
	| Big_nest of bexp * bexp                      (* nesting *)
	| Big_el of int                                (* 0 or 1 *)
	| Big_id of int * string list                  (* id(1) id(3,{a,c,f})*)
	| Big_ion of string * num_exp list * string list (*K p0 p1 (a + b + {f, g}) *)
	| Big_share of bexp * bexp * bexp              (* share A by psi in B *)

type dec =
	(* ctrl ide = arity *)
	| Ctrl_dec of string * int
	(* fun ctrl ide(p0, p1, p2) = arity *)
	| Ctrl_dec_f of string * string list * int
	(* big ide = big *)
	| Big_dec of string * bexp
	(* fun big ide(p0, p1, p2) = big *)
	| Big_dec_f of string * string list * bexp
	(* rate ide = fexp *)
	| Rate_dec of string * num_exp
	(* rate ide(p0, p1, p2) = fexp *)
	| Rate_dec_f of string * string list * num_exp
	(* react ide = big -> big *)
	| React_dec of string * bexp * bexp
	(* react ide p0 p1 p2 = big -> big *)
	| React_dec_f of string * string list * bexp * bexp
	(* react ide = big -> big @ rho *)
	| Sreact_dec of string * bexp * bexp * rexp
	(* react ide p0 p1 p2 = big -> big @ rho *)
	| Sreact_dec_f of string * string list * bexp * bexp * rexp
	(* float ide = fexp *)
	| Fexp_dec of string * num_exp
	(* int ide = iexp *)
	| Iexp_dec of string * num_exp

(*****************************************************************************)
(********************************** BILOG ************************************)
(*****************************************************************************)
type pred = 
	| Pred of string * string list

type bilog = 
	| B_null
	| Bilog of var_dec list * pred list

	
