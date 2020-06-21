open Bigraph

module Id = struct
  type t = string

  let compare = String.compare
end

type str_exp = Str_val of string * Loc.t

type num_exp = Num_int_val of int * Loc.t | Num_float_val of float * Loc.t

type var_exp = Var of Id.t * Loc.t

type exp = ENum of num_exp | EStr of str_exp | EVar of var_exp | EOp of op

and op =
  | Plus of exp * exp * Loc.t
  | Minus of exp * exp * Loc.t
  | UMinus of exp * Loc.t
  | Prod of exp * exp * Loc.t
  | Div of exp * exp * Loc.t
  | Pow of exp * exp * Loc.t

type ctrl_exp =
  | Ctrl_exp of Id.t * int * Loc.t
  | Ctrl_fun_exp of Id.t * Id.t list * int * Loc.t

type dctrl = Atomic of ctrl_exp * Loc.t | Non_atomic of ctrl_exp * Loc.t

type dexp = { d_id : Id.t; d_exp : exp; d_loc : Loc.t }

type id_exp = { id_place : int; id_link : Id.t list; id_loc : Loc.t }

(* Atomic if ctrl is atomic *)
type ion_exp =
  (* K{f, g} *)
  | Big_ion_exp of Id.t * Id.t list * Loc.t
  (* K(2.46, 1){f, g} *)
  | Big_ion_fun_exp of Id.t * exp list * Id.t list * Loc.t

(* ({{0,2,3}, {}}, 5) *)
type place_exp = {
  plc_parents : int list list;
  plc_roots : int;
  plc_loc : Loc.t;
}

(* /x *)
type closure_exp = { cl_name : Id.t; cl_loc : Loc.t }

(* /{x0, x1} *)
type merge_closure_exp = { m_cl_names : Id.t list; m_cl_loc : Loc.t }

(* x/{x0, x1} *)
type sub_exp = { out_name : Id.t; in_names : Id.t list; sub_loc : Loc.t }

type wire_exp =
  | Close_exp of closure_exp list
  | Merge_close_exp of merge_closure_exp
  | Sub_exp of sub_exp

type big_exp =
  (* b *)
  | Big_var of Id.t * Loc.t
  | Big_var_fun of Id.t * exp list * Loc.t (* b(1, 5.67) *)
  | Big_new_name of Id.t * Loc.t (* {n} *)
  | Big_comp of big_exp * big_exp * Loc.t (* A * B *)
  | Big_tens of big_exp * big_exp * Loc.t (* A + B *)
  | Big_par of big_exp * big_exp * Loc.t (* A | B *)
  | Big_ppar of big_exp * big_exp * Loc.t (* A || B *)
  | Big_share of big_exp * big_exp * big_exp * Loc.t
  (* share A by psi in B *)
  | Big_num of int * Loc.t (* 0 or 1 *)
  | Big_id of id_exp (*id, id(1), id(3, {a, c, f}) *)
  | Big_merge of int * Loc.t
  | Big_split of int * Loc.t
  | Big_plc of place_exp
  | Big_nest of ion_exp * big_exp * Loc.t (* A . B *)
  | Big_ion of ion_exp
  | Big_close of closure_exp (* closure *)
  | Big_sub of sub_exp (* substitution *)
  | Big_wire of wire_exp * big_exp * Loc.t (* /x y/{y0, y1} /z A *)
  | Big_par_fn of exp * big_exp * Loc.t (* par(n,b) *)
  | Big_ppar_fn of exp * big_exp * Loc.t

(* ppar(n,b) *)

(* /x y/{y0, y1} /z A *)

type eta_exp = int list * Loc.t

type cond_where = Cond_Ctx | Cond_Param

type cond_exp = {
  neg : bool option;
  pred : big_exp;
  place : cond_where;
  loc : Loc.t;
}

type dbig =
  | Big_exp of Id.t * big_exp * Loc.t
  | Big_fun_exp of Id.t * Id.t list * big_exp * Loc.t

type dreact =
  | React_exp of
      Id.t
      * Id.t
      * int
      * big_exp
      * big_exp
      * exp option
      * eta_exp option
      * cond_exp list option
      * Loc.t
  | React_fun_exp of
      Id.t
      * Id.t
      * int
      * Id.t list
      * big_exp
      * big_exp
      * exp option
      * eta_exp option
      * cond_exp list option
      * Loc.t

type daction = {
  action_id : Id.t;
  action_rules : dreact list;
  action_reward : exp option;
}

type dec =
  | Dctrl of dctrl
  | Dint of dexp
  | Dfloat of dexp
  | Dstr of dexp
  | Dbig of dbig
  | Dreact of dreact
  | Daction of daction

type init_exp = Init of Id.t * Loc.t | Init_fun of Id.t * exp list * Loc.t

type param_int_exp =
  | Param_int_val of exp * Loc.t
  | Param_int_range of exp * exp * exp * Loc.t
  | Param_int_set of exp list * Loc.t

type param_float_exp =
  | Param_float_val of exp * Loc.t
  | Param_float_range of exp * exp * exp * Loc.t
  | Param_float_set of exp list * Loc.t

type param_str_exp =
  | Param_str_val of exp * Loc.t
  | Param_str_set of exp list * Loc.t

type param_exp =
  | Param_int of Id.t list * param_int_exp * Loc.t
  | Param_float of Id.t list * param_float_exp * Loc.t
  | Param_str of Id.t list * param_str_exp * Loc.t

type rul_id =
  | Rul_id of Id.t * Loc.t
  | Rul_id_fun of Id.t * exp list * Loc.t

type pred_id =
  | Pred_id of Id.t * exp option * Loc.t
  | Pred_id_fun of Id.t * exp list * exp option * Loc.t

type pr_exp = Pr_red of rul_id list * Loc.t | Pr of rul_id list * Loc.t

type dbrs = {
  dbrs_pri : pr_exp list;
  dbrs_init : init_exp;
  dbrs_params : param_exp list;
  dbrs_preds : pred_id list;
  dbrs_loc : Loc.t;
  dbrs_type : Rs.rs_type;
}

type model = { model_decs : dec list; model_rs : dbrs; model_loc : Loc.t }

type const = Cint of dexp | Cfloat of dexp | Cstr of dexp

type consts = const list

let id_of_ctrl_exp = function
  | Ctrl_exp (d, _, _) | Ctrl_fun_exp (d, _, _, _) -> d

let ar_of_ctrl_exp = function
  | Ctrl_exp (_, n, _) | Ctrl_fun_exp (_, _, n, _) -> n

let loc_of_ctrl_exp = function
  | Ctrl_exp (_, _, l) | Ctrl_fun_exp (_, _, _, l) -> l

let id_of_dbig = function Big_exp (d, _, _) | Big_fun_exp (d, _, _, _) -> d

let id_of_dreact = function
  | React_exp (d, _, _, _, _, _, _, _, _)
  | React_fun_exp (d, _, _, _, _, _, _, _, _, _) ->
      d

let loc_of_dbig = function
  | Big_exp (_, _, l) | Big_fun_exp (_, _, _, l) -> l

let loc_of_dreact = function
  | React_exp (_, _, _, _, _, _, _, _, l)
  | React_fun_exp (_, _, _, _, _, _, _, _, _, l) ->
      l

let id_of_ion_exp = function
  | Big_ion_exp (id, _, _) | Big_ion_fun_exp (id, _, _, _) -> id

let face_of_ion_exp = function
  | Big_ion_exp (_, f, _) | Big_ion_fun_exp (_, _, f, _) -> f

let loc_of_ion_exp = function
  | Big_ion_exp (_, _, l) | Big_ion_fun_exp (_, _, _, l) -> l

let loc_of_big_exp = function
  | Big_var (_, p)
  | Big_var_fun (_, _, p)
  | Big_new_name (_, p)
  | Big_comp (_, _, p)
  | Big_tens (_, _, p)
  | Big_par (_, _, p)
  | Big_ppar (_, _, p)
  | Big_share (_, _, _, p)
  | Big_num (_, p)
  | Big_nest (_, _, p)
  | Big_wire (_, _, p)
  | Big_merge (_, p)
  | Big_split (_, p)
  | Big_par_fn (_, _, p)
  | Big_ppar_fn (_, _, p) ->
      p
  | Big_id e -> e.id_loc
  | Big_plc e -> e.plc_loc
  | Big_ion e -> loc_of_ion_exp e
  | Big_close e -> e.cl_loc
  | Big_sub e -> e.sub_loc

let names_of_closures = List.fold_left (fun acc c -> c.cl_name :: acc) []

(* let init_of_ts = function *)
(* | Dbrs dbrs -> dbrs.dbrs_init *)
(* | Dsbrs dsbrs -> dsbrs.dsbrs_init *)

type rul_call = RulCall of Id.t * int * Loc.t

let ids_of_pr_exps =
  List.fold_left
    (fun acc -> function Pr_red (rules, _) | Pr (rules, _) -> rules @ acc)
    []

let rul_id_to_rul_call = function
  | Rul_id (id, loc) -> RulCall (id, 0, loc)
  | Rul_id_fun (id, acts, loc) -> RulCall (id, List.length acts, loc)

(* let rules_of_ts = function *)
(* | Dbrs dbrs -> *)
(* List.map rul_id_to_rul_call (ids_of_pr_exps (dbrs.dbrs_pri)) *)
(* | Dsbrs dsbrs -> *)
(* List.map srul_id_to_rul_call (ids_of_spr_exps (dsbrs.dsbrs_pri)) *)

(* let params_of_ts = function *)
(* | Dbrs ts -> ts.dbrs_params *)
(* | Dsbrs ts -> ts.dsbrs_params *)

let string_of_consts l =
  List.map
    (function
      | Cint x -> x.d_id ^ "=<exp>"
      | Cfloat x -> x.d_id ^ "=<exp>"
      | Cstr x -> x.d_id ^ "=<exp>")
    l
  |> String.concat ","
