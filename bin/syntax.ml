open Printf

(*****************************************************************************)
(******************************* PRIORITIES **********************************)
(*****************************************************************************)
type pp = (Lexing.position * Lexing.position)

type rule = 
  | Rul of string * pp
  | Rul_fun of string * string list * pp

(*****************************************************************************)
(********************************** MODEL ************************************)
(*****************************************************************************)

type num_exp =
  | Num_val of float * pp
  | Num_ide of string * pp
  | Num_plus of num_exp * num_exp * pp
  | Num_minus of num_exp * num_exp * pp
  | Num_prod of num_exp * num_exp * pp
  | Num_div of num_exp * num_exp * pp
  | Num_pow of num_exp * num_exp * pp

type param = 
  | Int_param of string * num_exp list * pp
  | Float_param of string * num_exp list * pp

type pri_class =
  | Pri_class of rule list * pp
  | Pri_classr of rule list* pp

type init =
  | Init of string * pp
  | Init_fun of string * num_exp list * pp

type brs = 
  | Brs of param list * init * pri_class list * pp
  | Sbrs of param list * init * pri_class list * pp

type bexp =
  | Big_ide of string * pp                                   (* b *)
  | Big_name of string * pp                                  (* name *)
  | Big_ide_fun of string * num_exp list * pp                (* b(1, 5.67) *)
  | Big_plac of int list list * int * pp                     (* ({{0,2,3}, {}}, 5) *)
  | Big_comp_c of bexp * bexp * pp                           (* composition (lhs is a closure)*)    
  | Big_comp of bexp * bexp * pp                             (* composition *)
  | Big_close of string list * pp                            (* closure *)
  | Big_par of bexp * bexp * pp                              (* merge parallel *)
  | Big_ppar of bexp * bexp * pp                             (* parallel *)
  | Big_nest of bexp * bexp * pp                             (* nesting *)
  | Big_el of int * pp                                       (* 0 or 1 *)
  | Big_id of int * string list * pp                         (* id(1) id(3, {a, c, f}) *)
  | Big_ion of string * string list * pp                     (* K{f, g} *)
  | Big_ion_fun of string * string list * num_exp list * pp  (* K(2.46, 1){f, g} *)
  | Big_share of bexp * bexp * bexp * pp                     (* share A by psi in B *)
  | Big_tens of bexp * bexp * pp                             (* tensor product *) 

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

type ctrl_dec =
  | Ctrl_dec of string * int * pp	                   (* ctrl c = 3 *)
  | Ctrl_dec_f of string * string list * int * pp          (* fun ctrl c(a, b) = 2 *)

type dec =
  | Atomic of ctrl_dec
  | Non_atomic of ctrl_dec
  | Int_dec of string * num_exp * pp 
  | Float_dec of string * num_exp * pp
  | Big_dec of string * bexp * pp                          (* big b = ... *)
  | Big_dec_f of string * string list * bexp * pp 	   (* fun big b(a, b) = ... *)
  | React_dec of string * bexp * bexp * pp 	           (* react r = ... -> ... *)
  | React_dec_f of string * string list * bexp * bexp * pp (* react r(a, b) = ... -> ... *)
  | Sreact_dec of string * bexp * bexp * num_exp  * pp     (* react r = ... -> ... @ 3.4 *)
  | Sreact_dec_f of string * string list * bexp * bexp * num_exp * pp

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

