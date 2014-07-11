open Format

type error =
  | Unbound_variable of string
  | Multiple_declaration of string
  | Wrong_type of string * string

exception ERROR of error * Loc.t

let report_error fmt = function
  | Unbound_variable s ->
    fprintf fmt "Unbound variable %s" s
  | Multiple_declaration s ->
    fprintf fmt "Variable %s is defined multiple times" s
  | Wrong_type (curr, exp) ->
    fprintf fmt "This expression has type %s but an expression was expected of type %s" curr exp

type ptyp = 
  [ `int
  | `float
  | `undef of int ]

type big_value =
  | Big of Big.bg
  | Big_fun of Big.bg

type react_value =
  | React of Brs.react
  | React_fun of Brs.react
                   
type sreact_value =
  | Sreact of Sbrs.sreact
  | Sreact_fun of Big.bg * Big.bg * Syntax.float_exp

type ctrl_value =
  | Ctrl of Ctrl.t
  | Ctrl_fun of Ctrl.t
  | A_ctrl of Ctrl.t
  | A_ctrl_fun of Ctrl.t
  
type value =
  | Vint of int
  | Vfloat of float
  | Vint_p of int list
  | Vfloat_p of float list
  | Vbig of big_value      
  | Vctrl of ctrl_value
  | Vreact of react_value
  | Vsreact of sreact_value

let get_type = function
  | Vint _ -> "int"
  | Vfloat _ -> "float"
  | Vint_p _ -> "int param"
  | Vfloat_p _ -> "float param"
  | Vbig _ -> "big"
  | Vctrl _ -> "ctrl"
  | Vreact _ -> "react"
  | Vsreact _ -> "sreact"

let to_int loc = function
  | Vint v -> v
  | Vfloat _| Vint_p _ | Vfloat_p _ | Vbig _ 
  | Vctrl _ | Vreact _ | Vsreact _ as v -> 
    raise (ERROR (Wrong_type ("int", get_type v), loc))

let to_float loc = function
  | Vfloat v -> v
  | Vint _| Vint_p _ | Vfloat_p _ | Vbig _ 
  | Vctrl _ | Vreact _ | Vsreact _ as v -> 
    raise (ERROR (Wrong_type ("float", get_type v), loc))

module Ide = struct
  type t = string
  let compare = String.compare
end

module Env = struct

  module IdeMap = Map.Make (Ide)
  module IdeSet = Set.Make (Ide)    

  type t = 
    { store : value IdeMap.t;
      types : ptyp list IdeMap.t;
      atomic : IdeSet.t; 
      signature : int Sig.t;
    }

  let empty =
    { store = IdeMap.empty;
      types = IdeMap.empty;
      atomic = IdeSet.empty;
      signature = Sig.empty;
    }

  let find_value_exn env ide loc =
    try
      IdeMap.find ide env.store
    with
    | Not_found -> 
      raise (ERROR (Unbound_variable ide, loc))

  let find_type_exn env ide loc =
    try
      IdeMap.find ide env.types
    with
    | Not_found -> 
      raise (ERROR (Unbound_variable ide, loc))

  let is_atomic_exn env c = 
    IdeSet.mem c env.atomic

  let get_sig env = env.signature

  let add_value env ide v verbose fmt loc =
    if IdeMap.mem ide env.store && verbose then (
      report_error fmt (Multiple_declaration ide));
    { env with store = IdeMap.add ide v env.store; }

  let add_type env ide tyl =
    { env with types = IdeMap.add ide tyl env.types; }

  let add_atomic env c =
    { env with atomic = IdeSet.add c env.atomic; }

  let add_sig env c ar =
    { env with signature = Sig.add c ar env.signature; }
    
end
