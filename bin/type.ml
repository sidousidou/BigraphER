open Ast
open Format

type error =
  | Wrong_type of string * string
  | Atomic_ctrl of string
  | Arity of string * int * int
  | Unbound_variable of string

type warning =   
  | Multiple_declaration of string

exception ERROR of error * Loc.t

let report_error fmt = function
  | Wrong_type (curr, exp) ->
    fprintf fmt "This expression has type %s but an expression was expected of type %s" curr exp
  | Atomic_ctrl id ->
    fprintf fmt "Control %s is atomic but it is here used in a nesting expression" id
  | Arity (id, ar, ar_dec) ->
    fprintf fmt "Control %s has arity %d but a control of arity %d was expected" id ar ar_dec
  | Unbound_variable s ->
    fprintf fmt "Unbound variable %s" s

let report_warning verb fmt = function
  | Multiple_declaration s -> (
      if verb then fprintf fmt "Variable %s is defined multiple times" s
      else ()
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
  val add : key -> 'a -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val find : key -> 'a t -> 'a 
  val to_signature : int t -> int Sig.t
end = struct 
  
  include Map.Make (Id)
      
  let to_signature store = 
    fold (fun id ar acc ->
        Sig.add id ar acc
      ) store Sig.empty

end

(* Build a signature and warning on double declarations
   Exception on error *)
let build_sig verb fmt =
  List.fold_left (fun acc -> function
      | Dctrl (Atomic (exp , _)) 
      | Dctrl (Non_atomic (exp , _)) -> (
          let id = id_of_ctrl_exp exp in
          if Arity.mem id acc then
            report_warning verb fmt (Multiple_declaration id);
          Arity.add id (ar_of_ctrl_exp exp) acc
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

(* Type check for fun applications *)
