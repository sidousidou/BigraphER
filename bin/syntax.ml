open Utils
open Printf

(*****************************************************************************)
(******************************* PRIORITIES **********************************)
(*****************************************************************************)

type rule = 
  | Rul of string * (Lexing.position * Lexing.position)
  | Rul_fun of string * string list * (Lexing.position * Lexing.position)

(*****************************************************************************)
(********************************** MODEL ************************************)
(*****************************************************************************)

type num_exp =
  | Num_val of float * (Lexing.position * Lexing.position)
  | Num_ide of string * (Lexing.position * Lexing.position)
  | Num_plus of num_exp * num_exp * (Lexing.position * Lexing.position)
  | Num_minus of num_exp * num_exp * (Lexing.position * Lexing.position)
  | Num_prod of num_exp * num_exp * (Lexing.position * Lexing.position)
  | Num_div of num_exp * num_exp * (Lexing.position * Lexing.position)
  | Num_pow of num_exp * num_exp * (Lexing.position * Lexing.position)

type param = 
  | Int_param of string * num_exp list * (Lexing.position * Lexing.position)
  | Float_param of string * num_exp list * (Lexing.position * Lexing.position)

type pri_class =
  | Pri_class of rule list * (Lexing.position * Lexing.position)
  | Pri_classr of rule list* (Lexing.position * Lexing.position)

type init =
  | Init of string * (Lexing.position * Lexing.position)
  | Init_fun of string * num_exp list * (Lexing.position * Lexing.position)

type brs = 
  | Brs of param list * init * pri_class list * (Lexing.position * Lexing.position)
  | Sbrs of param list * init * pri_class list * (Lexing.position * Lexing.position)

type bexp =
  | Big_ide of string * (Lexing.position * Lexing.position)                                   (* b *)
  | Big_name of string * (Lexing.position * Lexing.position)                                  (* name *)
  | Big_ide_fun of string * num_exp list * (Lexing.position * Lexing.position)                (* b(1, 5.67) *)
  | Big_plac of int list list * int * (Lexing.position * Lexing.position)                     (* ({{0,2,3}, {}}, 5) *)
  | Big_comp_c of bexp * bexp * (Lexing.position * Lexing.position)                           (* composition (lhs is a closure)*)    
  | Big_comp of bexp * bexp * (Lexing.position * Lexing.position)                             (* composition *)
  | Big_close of string list * (Lexing.position * Lexing.position)                            (* closure *)
  | Big_par of bexp * bexp * (Lexing.position * Lexing.position)                              (* merge parallel *)
  | Big_ppar of bexp * bexp * (Lexing.position * Lexing.position)                             (* parallel *)
  | Big_nest of bexp * bexp * (Lexing.position * Lexing.position)                             (* nesting *)
  | Big_el of int * (Lexing.position * Lexing.position)                                       (* 0 or 1 *)
  | Big_id of int * string list * (Lexing.position * Lexing.position)                         (* id(1) id(3, {a, c, f}) *)
  | Big_ion of string * string list * (Lexing.position * Lexing.position)                     (* K{f, g} *)
  | Big_ion_fun of string * string list * num_exp list * (Lexing.position * Lexing.position)  (* K(2.46, 1){f, g} *)
  | Big_share of bexp * bexp * bexp * (Lexing.position * Lexing.position)                     (* share A by psi in B *)
  | Big_tens of bexp * bexp * (Lexing.position * Lexing.position)                             (* tensor product *) 

(* Only for debugging *)
let string_of_bexp b =
  match b with
  | Big_ide _ -> "Big_ide"
  | Big_name _ -> "Big_name"
  | Big_ide_fun _ -> "Big_ide_fun"
  | Big_plac _ -> "Big_plac"
  | Big_comp_c _ -> "Big_comp_c"
  | Big_comp _ -> "Big_comp"
  | Big_close _ -> "Big_close"
  | Big_par _ -> "Big_par"
  | Big_ppar _ -> "Big_ppar"
  | Big_nest _ -> "Big_nest"
  | Big_el _ -> "Big_el"
  | Big_id _ -> "Big_id"
  | Big_ion _ -> "Big_ion"
  | Big_ion_fun _ -> "Big_ion_fun"
  | Big_share _ -> "Big_share"
  | Big_tens _ -> "Big_tens"  

let string_of_num_exp n =
  match n with
  | Num_val (f, _) -> Printf.sprintf "%g" f
  | Num_ide (ide, _) -> ide
  | Num_plus _ -> "Plus"
  | Num_minus _ -> "Minus"
  | Num_prod _ -> "Prod"
  | Num_div _ -> "Div"
  | Num_pow _ -> "Pow"

type dec =
  | Ctrl_dec of string * int * (Lexing.position * Lexing.position)	                      (* ctrl c = 3 *)
  | Ctrl_dec_f of string * string list * int * (Lexing.position * Lexing.position)            (* fun ctrl c(a, b) = 2 *)
  | Int_dec of string * num_exp * (Lexing.position * Lexing.position) 
  | Float_dec of string * num_exp * (Lexing.position * Lexing.position)
  | Big_dec of string * bexp * (Lexing.position * Lexing.position)                            (* big b = ... *)
  | Big_dec_f of string * string list * bexp * (Lexing.position * Lexing.position) 	      (* fun big b(a, b) = ... *)
  | React_dec of string * bexp * bexp * (Lexing.position * Lexing.position) 	              (* react r = ... -> ... *)
  | React_dec_f of string * string list * bexp * bexp * (Lexing.position * Lexing.position)   (* react r(a, b) = ... -> ... *)
  | Sreact_dec of string * bexp * bexp * num_exp  * (Lexing.position * Lexing.position)        (* react r = ... -> ... @ 3.4 *)
  | Sreact_dec_f of string * string list * bexp * bexp * num_exp * (Lexing.position * Lexing.position)

let print_pos (p0, p1) =
  eprintf "File \"%s\", line %d, charachters %d-%d:\n"
    p0.Lexing.pos_fname p0.Lexing.pos_lnum (p0.Lexing.pos_cnum - p0.Lexing.pos_bol)
    (p1.Lexing.pos_cnum - p1.Lexing.pos_bol)

(*****************************************************************************)
(********************************** BILOG ************************************)
(*****************************************************************************)
(* type pred =  *)
(*   | Pred of string * string list *)

(* type bilog =  *)
(*   | B_null *)
  (*   | Bilog of var_dec list * pred list  ~path_out: !out_path*)

