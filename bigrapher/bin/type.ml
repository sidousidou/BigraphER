open Ast
open Format

type error =
  | Wrong_type of string * string
  | Atomic_ctrl of string
  | Arity of string * int * int
  | FunArity of string * int * int
  | Unbound_variable of string

type warning =   
  | Multiple_declaration of string * Loc.t

exception ERROR of error * Loc.t

let report_error fmt = function
  | Wrong_type (curr, exp) ->
    fprintf fmt "This expression has type %s but an expression was expected of type %s" curr exp
  | Atomic_ctrl id ->
    fprintf fmt "Control %s is atomic but it is here used in a nesting expression" id
  | Arity (id, ar, ar_dec) ->
    fprintf fmt "Control %s has arity %d but a control of arity %d was expected" id ar ar_dec
  | FunArity (id, ar, ar_dec) ->
    fprintf fmt "Identifier %s expects %d arguments but it is here used with %d arguments" id ar_dec ar
  | Unbound_variable s ->
    fprintf fmt "Unbound variable %s" s

let report_warning fmt = function
  | Multiple_declaration (s, loc) -> ( 
      Loc.print_loc fmt loc;
      fprintf fmt ":@.Warning: Variable %s is defined multiple times@." s
    )

(* Atomic controls check *)

module Atomic : sig
  type elt = Id.t
  type t = Set.Make(Id).t
  val empty : t
  val add : elt -> t -> t
  val is_atomic : elt -> t -> bool
end = struct
  
  include Set.Make (Id)
      
  let is_atomic = mem
 
end

let find_all_atomic =
  List.fold_left (fun acc -> function
      | Dctrl (Atomic (exp , _)) -> Atomic.add (id_of_ctrl_exp exp) acc
      | Dctrl _ | Dint _ | Dfloat _ | Dbig _ | Dreact _ | Dsreact _ -> acc
    ) Atomic.empty 

(* exception when false *)
let check_atomic_exn model =
  let store = find_all_atomic model.model_decs in
  let rec check_big = function
    | Big_comp (lexp, rexp, _) | Big_tens (lexp, rexp, _)
    | Big_par (lexp, rexp, _)  | Big_ppar (lexp, rexp, _) -> (
        check_big lexp && check_big rexp
      )
    | Big_share (exp0, exp1, exp2, _) -> (
        check_big exp0 && check_big exp1 && check_big exp2
      )
    | Big_var _ | Big_var_fun _ | Big_new_name _ | Big_ion _
    | Big_num _ | Big_id _ | Big_plc _ | Big_close _ -> true
    | Big_closures (_, exp, _) -> check_big exp
    | Big_nest (iexp, exp, l) -> (
        (let id = id_of_ion_exp iexp in
         if Atomic.is_atomic id store then
           raise (ERROR (Atomic_ctrl id, l))
         else true
        ) && check_big exp
      ) in 
  List.for_all (function
      | Dbig (Big_exp (_, exp, _)) 
      | Dbig (Big_fun_exp (_, _, exp, _)) -> ( 
          check_big exp
        )
      | Dreact (React_exp (_, lexp, rexp, _, _)) 
      | Dreact (React_fun_exp (_, _, lexp, rexp, _, _))
      | Dsreact (Sreact_exp (_, lexp, rexp, _, _, _))
      | Dsreact (Sreact_fun_exp (_, _, lexp, rexp, _, _, _)) -> (
          check_big lexp && check_big rexp
        )
      | Dctrl _ | Dint _ | Dfloat _  -> true
    ) model.model_decs
  
(* Arity check *)
module Arity : sig
  type key = Id.t
  type 'a t = 'a Map.Make(Id).t
  val empty : 'a t
  val add_w : key -> 'a -> 'a t -> Loc.t -> bool -> Format.formatter -> 'a t
  val find : key -> 'a t -> 'a 
  val to_signature : int t -> int Sig.t
end = struct 
  
  include Map.Make (Id)
  
  let add_w id v map loc verb fmt =
    if (mem id map) && verb then
      report_warning fmt (Multiple_declaration (id, loc));
    add id v map
    
  let to_signature store = 
    fold (fun id ar acc ->
        Sig.add id ar acc
      ) store Sig.empty

end

(* Build a signature and warning on double declarations
   Exception on error *)
let build_sig verb fmt =
  List.fold_left (fun acc -> function
      | Dctrl (Atomic (exp , loc)) 
      | Dctrl (Non_atomic (exp , loc)) -> (
          let id = id_of_ctrl_exp exp in
          Arity.add_w id (ar_of_ctrl_exp exp) acc loc verb fmt
        )
      | Dint _ | Dfloat _ | Dbig _ | Dreact _ | Dsreact _ -> acc
    ) Arity.empty 

(* Scan model and check all ions arities *)  
let check_sig_exn model verb fmt =
  let store = build_sig verb fmt model.model_decs in
  let check_ion_exp exp =
    let id = id_of_ion_exp exp 
    and ar = List.length (face_of_ion_exp exp) in
    try
      let ar_dec = Arity.find id store in
      if ar = ar_dec then true
      else raise (ERROR (Arity (id, ar, ar_dec), loc_of_ion_exp exp))
    with
    | Not_found -> raise (ERROR (Unbound_variable id, loc_of_ion_exp exp)) in
  let rec check_big = function
    | Big_comp (lexp, rexp, _) | Big_tens (lexp, rexp, _)
    | Big_par (lexp, rexp, _)  | Big_ppar (lexp, rexp, _) -> (
        check_big lexp && check_big rexp
      )
    | Big_share (exp0, exp1, exp2, _) -> (
        check_big exp0 && check_big exp1 && check_big exp2
      )
    | Big_var _ | Big_var_fun _ | Big_new_name _
    | Big_num _ | Big_id _ | Big_plc _ | Big_close _ -> true
    | Big_closures (_, exp, _) -> check_big exp
    | Big_ion iexp -> check_ion_exp iexp
    | Big_nest (iexp, exp, l) -> (
        check_ion_exp iexp && check_big exp
      ) in 
  if List.for_all (function
      | Dbig (Big_exp (_, exp, _)) 
      | Dbig (Big_fun_exp (_, _, exp, _)) -> ( 
          check_big exp
        )
      | Dreact (React_exp (_, lexp, rexp, _, _)) 
      | Dreact (React_fun_exp (_, _, lexp, rexp, _, _))
      | Dsreact (Sreact_exp (_, lexp, rexp, _, _, _))
      | Dsreact (Sreact_fun_exp (_, _, lexp, rexp, _, _, _)) -> (
          check_big lexp && check_big rexp
        )
      | Dctrl _ | Dint _ | Dfloat _  -> true
    ) model.model_decs then Arity.to_signature store
  else assert  false

(* Arity check for fun types *)
module ArFun : sig
  type key = Id.t
  type 'a t = 'a Map.Make(Id).t
  val empty : 'a t
  val add_w : key -> 'a -> 'a t -> Loc.t -> bool -> Format.formatter -> 'a t
  val find : key -> 'a t -> 'a
end = struct

  include Map.Make (Id)

  let add_w id v map loc verb fmt =
    if (mem id map) && verb then
      report_warning fmt (Multiple_declaration (id, loc));
    add id v map

end

let build_fmap verb fmt =
  List.fold_left (fun acc -> function
      | Dctrl (Atomic (Ctrl_exp (id, _, _), loc))
      | Dctrl (Non_atomic (Ctrl_exp (id, _, _), loc))
      | Dbig (Big_exp (id, _, loc))     
      | Dreact (React_exp (id, _, _, _, loc))
      | Dsreact (Sreact_exp (id, _ , _, _, _, loc)) ->
        ArFun.add_w id 0 acc loc verb fmt
      | Dint dec -> 
        ArFun.add_w dec.dint_id 0 acc dec.dint_loc verb fmt
      | Dfloat dec -> 
        ArFun.add_w dec.dfloat_id 0 acc dec.dfloat_loc verb fmt
      | Dctrl (Atomic (Ctrl_fun_exp (id, forms, _, _), loc))
      | Dctrl (Non_atomic (Ctrl_fun_exp (id, forms, _, _), loc))
      | Dbig (Big_fun_exp (id, forms, _, loc))
      | Dreact (React_fun_exp (id, forms, _, _, _, loc))
      | Dsreact (Sreact_fun_exp (id, forms, _, _, _, _, loc)) ->
        ArFun.add_w id (List.length forms) acc loc verb fmt
    ) ArFun.empty

(* true or exception *)
let check_fmap_exn model verb fmt =
  let store = build_fmap verb fmt model.model_decs in
  let check_id id ar store loc =
    try
      let ar_dec = ArFun.find id store in
      if ar = ar_dec then true
      else raise (ERROR (FunArity (id, ar, ar_dec), loc))
    with
    | Not_found -> raise (ERROR (Unbound_variable id, loc)) in   
  let check_init store = function
    | Init (id, loc) -> check_id id 0 store loc
    | Init_fun (id, acts, loc) -> check_id id (List.length acts) store loc in
  let rec check_int store = function
    | Int_var (id, loc) -> check_id id 0 store loc
    | Int_val _ -> true
    | Int_plus (a, b, _) | Int_minus (a, b, _) 
    | Int_prod (a, b, _) | Int_div (a, b, _) -> 
      check_int store a && check_int store b in 
  let rec check_float store = function
    | Float_var (id, loc) -> check_id id 0 store loc
    | Float_val _ -> true
    | Float_plus (a, b, _) | Float_minus (a, b, _) | Float_prod (a, b, _) 
    | Float_div (a, b, _) | Float_pow (a, b, _) ->
      check_float store a && check_float store b
  and check_ion store = function
    | Big_ion_exp (id, _, loc) -> check_id id 0 store loc
    | Big_ion_fun_exp (id, acts, _, loc) -> 
      check_id id (List.length acts) store loc in
  let rec check_big store = function
    | Big_var (id, loc) -> check_id id 0 store loc
    | Big_var_fun (id, acts, loc) -> check_id id (List.length acts) store loc
    | Big_comp (a, b, _) | Big_tens (a, b, _) 
    | Big_par (a, b, _) | Big_ppar (a, b, _) ->
      check_big store a && check_big store b
    | Big_share (a, b, c, _) ->
      check_big store a && check_big store b && check_big store c
    | Big_nest (ion, bexp, _) ->
      check_ion store ion && check_big store bexp
    | Big_ion ion -> check_ion store ion                                      
    | Big_closures (_, bexp, _) -> check_big store bexp
    | Big_new_name _ | Big_num _ | Big_id _ | Big_plc _ | Big_close _ -> true 
  in
  List.for_all (function
      | Dctrl _ -> true
      | Dint dec -> check_int store dec.dint_exp
      | Dfloat dec -> check_float store dec.dfloat_exp
      | Dbig (Big_exp (_, exp, _))
      | Dbig (Big_fun_exp (_, _, exp, _)) -> check_big store exp   
      | Dreact (React_exp (_, a, b, _, _))
      | Dsreact (Sreact_exp (_, a , b, _, _, _))
      | Dreact (React_fun_exp (_, _, a, b, _, _))
      | Dsreact (Sreact_fun_exp (_, _, a , b, _, _, _)) ->
        check_big store a && check_big store b   
    ) model.model_decs &&
  check_init store (init_of_ts (model.model_rs)) &&
  List.for_all (function
      | RulCall (id, n, loc) -> check_id id n store loc   
    ) (rules_of_ts (model.model_rs))

(* Type check for fun applications *)
let check_fun_types_exn model verb fmt =
  true
(* return a structure to be used in the evaluation *)

let type_check_exn model verb fmt =
  if check_atomic_exn model &&
     check_fmap_exn model verb fmt then 
    (check_sig_exn model verb fmt,
     check_fun_types_exn model verb fmt)
  else assert false
