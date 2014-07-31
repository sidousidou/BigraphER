open Format

type error =
  | Unbound_variable of string
  | Multiple_declaration of string

exception ERROR of error * Loc.t

let report_error fmt = function
  


type big_value =
  | Big of Big.bg
  | Big_fun of Big.bg

type react_value =
  | React of Brs.react
  | React_fun of Brs.react
                   
type sreact_value =
  | Sreact of Sbrs.sreact
  | Sreact_fun of Big.bg * Big.bg * Ast.float_exp

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

module Ide = struct
  type t = string
  let compare = String.compare                
end

module Env = struct

  module IdeMap = Map.Make (Ide)
  module IdeSet = Set.Make (Ide)    

  type t = 
    { store : value IdeMap.t;
      types : Type.typ list IdeMap.t;
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
        
  let find_exn env ide loc =
    (find_value_exn env ide loc, find_type_exn env ide loc)
  
  let is_atomic_exn env c = 
    IdeSet.mem c env.atomic

  let get_sig env = env.signature

  let add_value ide v verbose fmt loc env =
    if IdeMap.mem ide env.store && verbose then (
      (* No excpetion, just warning *)
      report_error fmt (Multiple_declaration ide));
    { env with store = IdeMap.add ide v env.store; }
    
  let add_type ide tyl verbose fmt loc env =
    if IdeMap.mem ide env.types && verbose then (
      (* No excpetion, just warning *)
      report_error fmt (Multiple_declaration ide));
    { env with types = IdeMap.add ide tyl env.types; }
    
  let add_type_exn ide tyl loc env =
    try 
      let t = IdeMap.find ide env.types in
      Type.check t tyl loc;
      { env with types = IdeMap.add ide tyl env.types; }
    with
    | Not_found -> { env with types = IdeMap.add ide tyl env.types; }

  let add_type_scope ide tyl loc env =
    { env with types = IdeMap.add ide tyl env.types; }

  let merge_type_exn env env' l =
    { env with types =  IdeMap.merge (fun id xt xt' ->
       match (xt, xt') with
       | (Some t, Some t') -> Some (Type.merge_l_exn t t' l)
       | (Some t, None) -> xt
       | (None, Some t') -> xt'
       | (None, None) -> None
       ) env.types env'.types; }
   
  let to_type_exn env t l =
    { env with types = IdeMap.map (fun x -> 
         Type.merge_l_exn t t' l 
       ) env.types; }

  let add_atomic c env =
    { env with atomic = IdeSet.add c env.atomic; }

  let add_sig c ar env =
    { env with signature = Sig.add c ar env.signature; }
    
end
