module Id = struct
    type t = string
    let compare = String.compare 
  end
	      
type int_exp =
  | Int_val of int * Loc.t
  | Int_var of Id.t * Loc.t
  | Int_plus of int_exp * int_exp * Loc.t
  | Int_minus of int_exp * int_exp * Loc.t
  | Int_prod of int_exp * int_exp * Loc.t
  | Int_div of int_exp * int_exp * Loc.t
  | Int_pow of int_exp * int_exp * Loc.t   

type float_exp =
  | Float_val of float * Loc.t
  | Float_var of Id.t * Loc.t
  | Float_plus of float_exp * float_exp * Loc.t
  | Float_minus of float_exp * float_exp * Loc.t
  | Float_prod of float_exp * float_exp * Loc.t
  | Float_div of float_exp * float_exp * Loc.t
  | Float_pow of float_exp * float_exp * Loc.t

type num_exp =
  | Num_int_val of int * Loc.t
  | Num_float_val of float * Loc.t
  | Num_var of Id.t * Loc.t
  | Num_plus of num_exp * num_exp * Loc.t
  | Num_minus of num_exp * num_exp * Loc.t
  | Num_prod of num_exp * num_exp * Loc.t
  | Num_div of num_exp * num_exp * Loc.t
  | Num_pow of num_exp * num_exp * Loc.t

type ctrl_exp =
  | Ctrl_exp of Id.t * int * Loc.t
  | Ctrl_fun_exp of Id.t * Id.t list * int * Loc.t

type dctrl = 
  | Atomic of ctrl_exp * Loc.t
  | Non_atomic of ctrl_exp * Loc.t 

type dint =
  { dint_id : Id.t;
    dint_exp: int_exp;
    dint_loc: Loc.t;
  }

type dfloat =
  { dfloat_id : Id.t;
    dfloat_exp: float_exp;
    dfloat_loc: Loc.t;
  }

type id_exp =
  { id_place : int;
    id_link : Id.t list;
    id_loc : Loc.t;
  }

(* Atomic if ctrl is atomic *)
type ion_exp =
  | Big_ion_exp of Id.t * Id.t list * Loc.t                         (* K{f, g} *)
  | Big_ion_fun_exp of Id.t * num_exp list * Id.t list * Loc.t      (* K(2.46, 1){f, g} *) 

type place_exp =                                            (* ({{0,2,3}, {}}, 5) *)
  { plc_parents : int list list;
    plc_roots : int;
    plc_loc : Loc.t;
  }         
                                         
type closure_exp =                                          (* /x *)
  { cl_name : Id.t;
    cl_loc : Loc.t;
  }

type big_exp =
  | Big_var of Id.t * Loc.t                                 (* b *)
  | Big_var_fun of Id.t * num_exp list * Loc.t              (* b(1, 5.67) *)
  | Big_new_name of Id.t * Loc.t                            (* {n} *)
  | Big_comp of big_exp * big_exp * Loc.t                   (* A * B *) 
  | Big_tens of big_exp * big_exp * Loc.t                   (* A + B *) 
  | Big_par of big_exp * big_exp * Loc.t                    (* A | B *)
  | Big_ppar of big_exp * big_exp * Loc.t                   (* A || B *)
  | Big_share of big_exp * big_exp * big_exp * Loc.t        (* share A by psi in B *)
  | Big_num of int * Loc.t                                  (* 0 or 1 *)
  | Big_id of id_exp                                        (*id, id(1), id(3, {a, c, f}) *)
  | Big_plc of place_exp
  | Big_nest of ion_exp * big_exp * Loc.t                   (* A . B *)
  | Big_ion of ion_exp                                      
  | Big_close of closure_exp                                (* closure *)
  | Big_closures of closure_exp list * big_exp  * Loc.t     (* /x/y/z A *)

type eta_exp = int list * Loc.t
  
type dbig =
  | Big_exp of Id.t * big_exp * Loc.t 
  | Big_fun_exp of Id.t * Id.t list * big_exp * Loc.t

type dreact =
  | React_exp of Id.t * big_exp * big_exp * eta_exp option * Loc.t 
  | React_fun_exp of Id.t * Id.t list * big_exp * big_exp * eta_exp option * Loc.t

type dsreact =
  | Sreact_exp of Id.t * big_exp * big_exp * eta_exp option * float_exp * Loc.t 
  | Sreact_fun_exp of Id.t * Id.t list * big_exp * big_exp * eta_exp option * float_exp * Loc.t

type dec =
  | Dctrl of dctrl
  | Dint of dint
  | Dfloat of dfloat
  | Dbig of dbig
  | Dreact of dreact
  | Dsreact of dsreact

type init_exp =
  | Init of Id.t * Loc.t
  | Init_fun of Id.t * num_exp list * Loc.t

type param_int_exp =
  | Param_int_val of int_exp * Loc.t
  | Param_int_range of int_exp * int_exp * int_exp * Loc.t 
  | Param_int_set of int_exp list * Loc.t

type param_float_exp =
  | Param_float_val of float_exp * Loc.t
  | Param_float_range of float_exp * float_exp * float_exp * Loc.t 
  | Param_float_set of float_exp list * Loc.t

type param_exp =
  | Param_int of Id.t * param_int_exp * Loc.t
  | Param_float of Id.t * param_float_exp * Loc.t

type rul_id =
  | Rul_id of Id.t * Loc.t
  | Rul_id_fun of Id.t * num_exp list * Loc.t

type pr_exp =
  | Pr_red of rul_id list * Loc.t 
  | Pr of rul_id list * Loc.t

type dbrs =
  { dbrs_pri : pr_exp list;
    dbrs_init: init_exp;
    dbrs_params: param_exp list;
    dbrs_loc: Loc.t;
  }

type srul_id =
  | Srul_id of Id.t * Loc.t
  | Srul_id_fun of Id.t * num_exp list * Loc.t

type spr_exp =
  | Spr_red of srul_id list * Loc.t 
  | Spr of srul_id list * Loc.t

type dsbrs =
  { dsbrs_pri : spr_exp list;
    dsbrs_init: init_exp;
    dsbrs_params: param_exp list;
    dsbrs_loc: Loc.t;
  }

type ts =
  | Dbrs of dbrs
  | Dsbrs of dsbrs

type model =
  { model_decs : dec list;
    model_rs : ts;
    model_loc : Loc.t;
  }

type const =   
  | Cint of dint
  | Cfloat of dfloat

type consts = const list

let id_of_ctrl_exp = function
  | Ctrl_exp (d, _, _) | Ctrl_fun_exp (d, _, _, _) -> d

let ar_of_ctrl_exp = function
  | Ctrl_exp (_, n, _) | Ctrl_fun_exp (_, _, n, _) -> n

let loc_of_ctrl_exp = function
  | Ctrl_exp (_, _, l) | Ctrl_fun_exp (_, _, _, l) -> l

let id_of_dbig = function
  | Big_exp (d, _, _) | Big_fun_exp (d, _, _, _) -> d

let id_of_dreact = function
  | React_exp (d, _, _, _, _) | React_fun_exp (d, _, _, _, _, _) -> d

let id_of_dsreact = function
  | Sreact_exp (d, _, _, _, _, _) | Sreact_fun_exp (d, _, _, _, _, _, _) -> d

let loc_of_dbig = function
  | Big_exp (_, _, l) | Big_fun_exp (_, _, _, l) -> l

let loc_of_dreact = function
  | React_exp (_, _, _, _, l) | React_fun_exp (_, _, _, _, _, l) -> l

let loc_of_dsreact = function
  | Sreact_exp (_, _, _, _, _, l) | Sreact_fun_exp (_, _, _, _, _, _, l) -> l

let id_of_ion_exp = function
  | Big_ion_exp (id, _, _) | Big_ion_fun_exp (id, _, _, _) -> id

let face_of_ion_exp = function
  | Big_ion_exp (_, f, _) | Big_ion_fun_exp (_, _, f, _) -> f

let loc_of_ion_exp = function
  | Big_ion_exp (_, _, l) | Big_ion_fun_exp (_, _, _, l) -> l

let loc_of_big_exp = function
  | Big_var (_, p) | Big_var_fun (_, _, p) | Big_new_name (_, p)
  | Big_comp (_, _, p) | Big_tens (_, _, p) | Big_par (_, _, p)
  | Big_ppar (_, _, p) | Big_share (_, _, _, p) | Big_num (_, p)
  | Big_nest (_, _, p) | Big_closures (_, _, p) -> p
  | Big_id e -> e.id_loc 
  | Big_plc e -> e.plc_loc
  | Big_ion e -> loc_of_ion_exp e
  | Big_close e -> e.cl_loc
 	   
let init_of_ts = function
  | Dbrs dbrs -> dbrs.dbrs_init 
  | Dsbrs dsbrs -> dsbrs.dsbrs_init

type rul_call = RulCall of Id.t * int * Loc.t

let ids_of_pr_exps =
  List.fold_left (fun acc -> function
      | Pr_red (rules, _) 
      | Pr (rules, _) -> rules @ acc
    ) []

let ids_of_spr_exps =
  List.fold_left (fun acc -> function
      | Spr_red (rules, _) 
      | Spr (rules, _) -> rules @ acc
    ) []

let rul_id_to_rul_call = function
  | Rul_id (id, loc) -> RulCall (id, 0, loc)
  | Rul_id_fun (id, acts, loc) -> RulCall (id, List.length acts, loc)

let srul_id_to_rul_call = function
  | Srul_id (id, loc) -> RulCall (id, 0, loc)
  | Srul_id_fun (id, acts, loc) -> RulCall (id, List.length acts, loc)

let rules_of_ts = function
  | Dbrs dbrs -> 
    List.map rul_id_to_rul_call (ids_of_pr_exps (dbrs.dbrs_pri))
  | Dsbrs dsbrs -> 
    List.map srul_id_to_rul_call (ids_of_spr_exps (dsbrs.dsbrs_pri))
