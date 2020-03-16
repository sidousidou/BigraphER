open Format
open Ast

type error =
  | Invalid_priority of string
  
exception ERROR of error * Loc.t

(* let report_error fmt = function *)
(*   | Unbound_variable s -> *)
(*     fprintf fmt "Unbound variable %s" s *)
(*   | Multiple_declaration s -> *)
(*     fprintf fmt "Variable %s is defined multiple times" s *)

module E = Env.Env


(*******************************************************************************)
(*                            Type inference                                   *)
(*******************************************************************************)

(* warning and no check *)
let eval_t_consts c verb fmt env =
  List.fold_left (fun env -> function
      | Cint d -> E.add_type d.dint_id [`int] verb fmt d.dint_loc env
      | Cfloat d -> E.add_type d.float_id [`float] verb fmt d.dflota_loc env
    ) env c

let eval_t_params params verb fmt env =
  List.fold_left (fun env -> function
      | Param_int (d, _, l) -> 
        E.add_type d [`int] verb fmt l env 
      | Param_float (d, _, l) ->
        E.add_type d [`int] verb fmt l env 
    ) env params

let rec eval_t_num_exp env cset = function
  | Num_int_val _ -> (`int, E.empty)
  | Num_float_val _ -> (`float, E.empty)
  | Num_var (d, l) -> (
     match E.find_type_exn env d l with
       | ['int] -> (`int, E.empty)  
       | ['float] -> (`float, E.empty)
       | ['undef] -> ('undef, E.add_type d ['undef] l E.empty)
       | t -> (
           ignore (check ['undef] t l);
           assert false;
         )
    )
  | Num_plus (a, b, l) | Num_minus (a, b, l) 
  | Num_prod (a, b, l) | Num_div (a, b, l)  
  | Num_pow (a, b, l) -> (
      let (t, env_out) = eval_t_num_exp env a 
      and (t', env_out') = eval_t_num_exp env b in
      let new_t = merge_exn t t' l in
      (new_t, E.to_type_exn (
          E.merge_type_exn env_out env_out' l
        ) new_t l
      )
    )

let eval_t_num_exps env = 
  List.map (fun a -> eval_t_num_exp env a)

let rec eval_t_int_exp env env_out = function
  | Int_val _ -> (`int, env_out)
  | Int_var (d, l) -> (
      if equal_exn `int (E.find_type_exn env d l) l then (`int, env_out)
      else (`int, E.add_type d [`int] l env_out)
    )
  | Int_plus (a, b, l) | Int_minus (a, b, l) 
  | Int_prod (a, b, l) | Int_div (a, b, l) -> (
      let (t, env_out) = eval_t_int_exp env env_out a 
      and (t', env_out') = eval_t_int_exp env env_out b in
      if equal_exn t t' l then (`int, E.merge_type_exn env_out env_out' l)
      else assert false
    )

let rec eval_t_float_exp env env_out = function
  | Float_val _ -> (`float, env_out)
  | Float_var (d, l) -> (
      if equal_exn `float (E.find_type_exn env d l) l then (`float, env_out)
      else (`float, E.add_type d [`float] l env_out)
    )
  | Float_plus (a, b, l) | Float_minus (a, b, l) 
  | Float_prod (a, b, l) | Float_div (a, b, l)  
  | Float_pow (a, b, l) -> (
      let (t, env_out) = eval_t_float_exp env env_out a
      and (t', env_out') = eval_t_float_exp env env_out b in
      if equal_exn t t' l then (`float, E.merge_type_exn env_out env_out' l)
      else assert false
    )

let bind_t forms l env =
  List.fold_left (fun env (f, t) ->
      E.add_type_scope f t l env
    ) env forms

let rec scan_t_big_exp env = function
  | Big_var_fun (id, , l) -> (
      (* Check for recursive call and get type *)
      let t = E.find_type_exn env id l in
      let t' = List.map ()  
      check t () l;
      (* Update type *)
      E.add_type_scope id t' l env
    )
  | Big_var (id, l) -> (
      (* Check for recursive calls *)
      ignore (E.find_type_exn env id l);
      (* No type propagated up *)
      env
    )
  | Big_new_name _ | Big_num _ | Big_id _ | Big_plc _
  | Big_close _ -> env
  | Big_comp (a, b, l) | Big_tens (a, b, l) | Big_par (a, b, l)
  | Big_ppar (a, b, l) -> (
      scan_t_big_exp (scan_t_big_exp env_scope env a) b 
    )

  | Big_share of big_exp * big_exp * big_exp * Loc.t        (* share A by psi in B *)
  | Big_nest of ion_exp * big_exp * Loc.t                   (* A . B *)
  | Big_closures of closure_exp list * big_exp  * Loc.t     (* /x/y/z A *)
  | Big_ion of ion_exp                                      


)
let scan_t_dbig env = function
  | Big_exp (_, rhs, l) | Big_fun_exp (_, _, rhs, l) ->   
    scan_t_big_exp env rhs
      
let scan_t_decs d verb fmt env =
  List.fold_left (fun env -> function
      | Dctrl (Atomic (d, l)) -> (
          let id = id_of_ctrl_exp d in
          E.add_type id (init_t_ctrl_exp d) verb fmt l
            (E.add_sig id (ar_of_ctrl_exp d)
               (E.add_atomic id env))
        )
      | Dctrl (Non_atomic (d, l)) -> (
          let id = id_of_ctrl_exp d in
          E.add_type id (init_t_ctrl_exp d) verb fmt l
            (E.add_sig id (ar_of_ctrl_exp d) env)
        )
      | Dint d -> 
        E.add_type d.dint_id [`int] verb fmt d.dint_loc env 
      | Dfloat d -> 
        E.add_type d.float_id [`float] verb fmt d.dfloat_loc env
      | Dbig d -> 
        E.add_type (id_of_dbig d) (init_t_dbig d) 
          verb fmt (loc_of_dbig d) (scan_t_dbig env d)
      | Dreact d ->
        E.add_type (id_of_dreact d) (init_t_dreact d) 
          verb fmt (loc_of_dreact d) (scan_t_dreact env d)
      | Dsreact d -> 
        E.add_type (id_of_dsreact d) (init_t_dsreact d) 
          verb fmt (loc_of_dsreact d) (scan_t_dsreact env d)
    ) env d


  
let eval_t_init d env =
  match d with
  | Init (d, l) -> E.add_type_exn d [] l env 
  | Init_fun (d, acts, l) -> 
    E.add_type_exn d eval_t_num_exps l env

let eval_t_rul_ides r_l l env =
  List.fold_left (fun env -> function
      | Rul_ide (d, l) ->
        E.add_type_exn d [] l env
      | Rul_ide_fun (d, acts, l) ->
        E.add_type_exn d eval_t_num_exps l env
  ) env r_l

let eval_t_pri pr_l env =
  List.fold_left (fun env -> function
      | Pr_red (r_l, l) | Pr (r_l, l) ->
        eval_t_rul_ides r_l l env
  ) env pr_l

let eval_t_srul_ides r_l l env =
  List.fold_left (fun env -> function
      | Srul_ide (d, l) ->
        E.add_type_exn d [] l env
      | Srul_ide_fun (d, acts, l) ->
        E.add_type_exn d eval_t_num_exps l env
  ) env r_l

let eval_t_spri pr_l env =
  List.fold_left (fun env -> function
      | Spr_red (r_l, l) | Spr (r_l, l) ->
        eval_t_rul_ides r_l l env
  ) env pr_l


let eval_t_decs d verb fmt env =
  List.fold_left (fun env -> function
      | Dctrl (Atomic (Ctrl_exp _, _))
      | Dctrl (Non_atomic (Ctrl_exp _, _)) -> env
      | Dctrl (Atomic (Ctrl_fun_exp (d, forms, l), _))
      | Dctrl (Non_atomic (Ctrl_fun_exp (d, forms, l), _)) -> (
          let t' = List.map (fun env f ->
              E.find_type_exn env f l
            ) forms in
          E.add_type d t' verb fmt l
        )
      | Dint d -> eval_t_init d env
      | Dfloat d -> eval_t_float d env




      | Dbig d -> 
        E.add_type (id_of_dbig d) (init_t_dbig d) 
          verb fmt (loc_of_dbig d) env
      | Dreact d -> 
        E.add_type (id_of_dreact d) (init_t_dreact d) 
          verb fmt (loc_of_dreact d) env
      | Dsreact d -> 
        E.add_type (id_of_dsreact d) (init_t_dsreact d) 
          verb fmt (loc_of_dsreact d) env
    ) env d

let eval_types c m env =
  (eval_t_consts c env)
  
(*******************************************************************************)
(*******************************************************************************)


(* let rec eval_int env = function *)
(*   | Int_val (v, _) -> v *)
(*   | Int_var (ide, l) -> Env.to_int l (E.find_value_exn env ide l) *)
(*   | Int_plus (a, b, _) -> (eval_int env a) + (eval_int env b) *)
(*   | Int_minus (a, b, _) -> (eval_int env a) - (eval_int env b) *)
(*   | Int_prod (a, b, _) -> (eval_int env a) * (eval_int env b) *)
(*   | Int_div (a, b, _) -> (eval_int env a) / (eval_int env b) *)

(* let rec eval_float env = function *)
(*   | Float_val (v, _) -> v *)
(*   | Float_var (ide, l) -> Env.to_float l (E.find_value_exn env ide l) *)
(*   | Float_plus (a, b, _) -> (eval_float env a) +. (eval_float env b) *)
(*   | Float_minus (a, b, _) -> (eval_float env a) -. (eval_float env b) *)
(*   | Float_prod (a, b, _) -> (eval_float env a) *. (eval_float env b) *)
(*   | Float_div (a, b, _) -> (eval_float env a) /. (eval_float env b) *)
(*   | Float_pow (a, b, _) -> (eval_float env a) ** (eval_float env b) *)

(* let rec eval_ctrl = function *)
(*   | Ctrl_exp (c, _, _) -> Ctrl.Ctrl (c, []) *)
(*   | Ctrl_fun_exp (c, f, _, _) ->  *)
(*     Ctrl.Fun_ctrl (c, List.map (fun s -> Ctrl.F s) f) *)



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
