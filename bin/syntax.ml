type id = string

type int_exp =
  | Int_val of int * Loc.t
  | Int_var of id * Loc.t
  | Int_plus of int_exp * int_exp * Loc.t
  | Int_minus of int_exp * int_exp * Loc.t
  | Int_prod of int_exp * int_exp * Loc.t
  | Int_div of int_exp * int_exp * Loc.t

type float_exp =
  | Float_val of float * Loc.t
  | Float_var of id * Loc.t
  | Float_plus of float_exp * float_exp * Loc.t
  | Float_minus of float_exp * float_exp * Loc.t
  | Float_prod of float_exp * float_exp * Loc.t
  | Float_div of float_exp * float_exp * Loc.t
  | Float_pow of float_exp * float_exp * Loc.t

type num_exp =
  | Num_int_val of int * Loc.t
  | Num_float_val of float * Loc.t
  | Num_var of id * Loc.t
  | Num_plus of num_exp * num_exp * Loc.t
  | Num_minus of num_exp * num_exp * Loc.t
  | Num_prod of num_exp * num_exp * Loc.t
  | Num_div of num_exp * num_exp * Loc.t
  | Num_pow of num_exp * num_exp * Loc.t

type ctrl_exp =
  | Ctrl_exp of id * int * Loc.t
  | Ctrl_fun_exp of id * id list * int * Loc.t

type dctrl = 
  | Atomic of ctrl_exp * Loc.t
  | Non_atomic of ctrl_exp * Loc.t 

type dint =
  { dint_id : id;
    dint_exp: int_exp;
    dint_loc: Loc.t;
  }

type dfloat =
  { dfloat_id : id;
    dfloat_exp: float_exp;
    dfloat_loc: Loc.t;
  }

type id_exp =
  { id_place : int;
    id_link : id list;
    id_loc : Loc.t;
  }

type ion_exp =
  | Big_ion of id * id list * Loc.t                         (* K{f, g} *)
  | Big_ion_fun of id * num_exp list * id list * Loc.t      (* K(2.46, 1){f, g} *) 

type place_exp =                                            (* ({{0,2,3}, {}}, 5) *)
  { plc_parents : int list list;
    plc_roots : int;
    plc_loc : Loc.t;
  }         
                                         
type closure_exp =                                          (* /x *)
  { cl_name : id;
    cl_loc : Loc.t;
  }

type big_exp =
  | Big_var of id * Loc.t                                   (* b *)
  | Big_var_fun of id * num_exp list * Loc.t                (* b(1, 5.67) *)
  | Big_new_name of id * Loc.t                              (* {n} *)
  | Big_comp of big_exp * big_exp * Loc.t                   (* A * B *) 
  | Big_tens of big_exp * big_exp * Loc.t                   (* A + B *) 
  | Big_par of big_exp * big_exp * Loc.t                    (* A | B *)
  | Big_ppar of big_exp * big_exp * Loc.t                   (* A || B *)
  | Big_share of big_exp * place_exp * big_exp * Loc.t      (* share A by psi in B *)
  | Big_num of int * Loc.t                                  (* 0 or 1 *)
  | Big_id of id_exp                                        (* id, id(1), id(3, {a, c, f}) *)
  | Big_plc of place_exp
  | Big_nest of ion_exp * big_exp * Loc.t                   (* A . B *)
  | Big_ion of ion_exp                                       
  | Big_close of closure_exp                                (* closure *)
  | Big_closures of closure_exp list * big_exp  * Loc.t     (* /x/y/z A *)

type eta_exp = int list * Loc.t
  
type dbig =
  | Big_exp of id * big_exp * Loc.t 
  | Big_fun_exp of id * id list * big_exp * Loc.t

type dreact =
  | React_exp of id * big_exp * big_exp * eta_exp option * Loc.t 
  | React_fun_exp of id * id list * big_exp * big_exp * eta_exp option * Loc.t

type dsreact =
  | Sreact_exp of id * big_exp * big_exp * eta_exp option * float_exp * Loc.t 
  | Sreact_fun_exp of id * id list * big_exp * big_exp * eta_exp option * float_exp * Loc.t

type dec =
  | Dctrl of dctrl
  | Dint of dint
  | Dfloat of dfloat
  | Dbig of dbig
  | Dreact of dreact
  | Dsreaact of dsreact

type init_exp =
  | Init of id * Loc.t
  | Init_fun of id * num_exp list * Loc.t

type param_int_exp =
  | Param_int_val of int_exp * Loc.t
  | Param_int_range of int_exp * int_exp * int_exp * Loc.t 
  | Param_int_set of int_exp list * Loc.t

type param_float_exp =
  | Param_float_val of float_exp * Loc.t
  | Param_float_range of float_exp * float_exp * float_exp * Loc.t 
  | Param_float_set of float_exp list * Loc.t

type param_exp =
  | Param_int of id * param_int_exp * Loc.t
  | Param_float of id * param_float_exp * Loc.t

type rul_ide =
  | Rul_ide of id * Loc.t
  | Rul_ide_fun of id * num_exp list * Loc.t

type pr_exp =
  | Pr_red of rul_ide list * Loc.t 
  | Pr of rul_ide list * Loc.t

type dbrs =
  { dbrs_pri : pr_exp list;
    dbrs_init: init_exp;
    dbrs_params: param_exp list;
    dbrs_loc: Loc.t;
  }

type srul_ide =
  | Srul_ide of id * Loc.t
  | Srul_ide_fun of id * num_exp list * Loc.t

type spr_exp =
  | Spr_red of srul_ide list * Loc.t 
  | Spr of srul_ide list * Loc.t

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

(* let int_of_num_exn = function *)
(*   | Num_int_val (v, l) -> Int_val (v, l) *)
(*   | Num_var (id, l) -> Int_var (id, l) *)
(*   | Num_plus (lhs, rhd, l) -> Int_plus (lhs, rhd, l) *)
(*   | Num_minus (lhs, rhd, l) -> Int_minus (lhs, rhd, l) *)
(*   | Num_prod (lhs, rhd, l) -> Int_prod (lhs, rhd, l) *)
(*   | Num_div (lhs, rhd, l) -> Int_div (lhs, rhd, l) *)
(*   | Num_pow (_, _, l) | Num_float_val (_, l) -> *)


