open Format
open Syntax

type error =
  | Invalid_priority of string
  
exception ERROR of error * Loc.t

(* let report_error fmt = function *)
(*   | Unbound_variable s -> *)
(*     fprintf fmt "Unbound variable %s" s *)
(*   | Multiple_declaration s -> *)
(*     fprintf fmt "Variable %s is defined multiple times" s *)

module E = Env.Env

let rec eval_int env = function
  | Int_val (v, _) -> v
  | Int_var (ide, l) -> Env.to_int l (E.find_value_exn env ide l)
  | Int_plus (a, b, _) -> (eval_int env a) + (eval_int env b)
  | Int_minus (a, b, _) -> (eval_int env a) - (eval_int env b)
  | Int_prod (a, b, _) -> (eval_int env a) * (eval_int env b)
  | Int_div (a, b, _) -> (eval_int env a) / (eval_int env b)

let rec eval_float env = function
  | Float_val (v, _) -> v
  | Float_var (ide, l) -> Env.to_float l (E.find_value_exn env ide l)
  | Float_plus (a, b, _) -> (eval_float env a) +. (eval_float env b)
  | Float_minus (a, b, _) -> (eval_float env a) -. (eval_float env b)
  | Float_prod (a, b, _) -> (eval_float env a) *. (eval_float env b)
  | Float_div (a, b, _) -> (eval_float env a) /. (eval_float env b)
  | Float_pow (a, b, _) -> (eval_float env a) ** (eval_float env b)

let rec eval_ctrl = function
  | Ctrl_exp (c, _, _) -> Ctrl.Ctrl (c, [])
  | Ctrl_fun_exp (c, f, _, _) -> 
    Ctrl.Fun_ctrl (c, List.map (fun s -> Ctrl.F s) f)


(* let load_consts_exn verb consts fmt loc = *)
(*   List.fold_left (fun env (ide, v) ->  *)
(*       try *)
(* 	E.add_value env ide (E.Vint (int_of_string v)) verb fmt loc *)
(*       with *)
(*       | Failure _ -> ( *)
(* 	  try *)
(*      E.add_value env ide (E.Vfloat (float_of_string v)) verb fmt loc *)
(*           with *)
(*           | Failure _ -> (raise ERROR (, loc)) *)
(*         ) *)
(*     ) E.empty consts *)

(* let load_decs verb decs env fmt = *)
(*   List.fold_left (fun env = function *)
(*       | Dctrl v -> *)
(*       | Dint v -> ( *)
(*           let value = eval_int verb v.dint_exp env fmt in *)
(*           E.add_value env v.dint_id value verb fmt v.dint_loc *)
(*         ) *)
(*       | Dfloat ( *)
(*           let value = eval_float verb v.dfloat_exp env fmt in *)
(*           E.add_value env v.dfloat_id value verb fmt v.dfloat_loc *)
(*         ) *)
(*       | Dbig of dbig *)
(*       | Dreact of dreact *)
(*       | Dsreact of dsreact *)
(*     ) env decs *)
