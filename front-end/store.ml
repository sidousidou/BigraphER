open Syntax
open Str
open Printf

exception INVALID_CONSTS
exception INVALID_VAL
exception INVALID_PRI
exception WRONG_TYPE
exception NO_IDE

type store_val =
| Store_int of int
| Store_float of float
| Store_int_fun of num_exp * string list
| Store_float_fun of num_exp * string list
| Store_big of Big.bg
| Store_big_fun of bexp * string list
| Store_ctrl of Base.ctrl
| Store_ctrl_fun of int * string list
| Store_react of Brs.react
| Store_react_fun of bexp * bexp * string list
| Store_sreact of Sbrs.sreact
| Store_sreact_fun of bexp * bexp * num_exp * string list
| Store_int_param of int list
| Store_float_param of float list

type p_class_ide = 
| P_class_ide of string list  (** Priority class *)
| P_rclass_ide of string list (** Reducable priority class *)

let p_to_brs c =
    match c with
    | P_class_ide x -> Brs.P_class_ide x 
    | P_rclass_ide x -> Brs.P_rclass_ide x

let p_to_sbrs c =
    match c with
    | P_class_ide x -> Sbrs.P_class_ide x 
    | P_rclass_ide x -> Sbrs.P_rclass_ide x
    
let init_env decs =
  Hashtbl.create (List.length decs)

let get_type v = 
  match v with
  | Store_int _ -> "int"
  | Store_float _ -> "float"
  | Store_int_fun _ -> "int"
  | Store_float_fun _ -> "float"
  | Store_big _ -> "big"
  | Store_big_fun _ -> "big"
  | Store_ctrl _ -> "ctrl"
  | Store_ctrl_fun _ -> "ctrl"
  | Store_react _ -> "react"
  | Store_react_fun _ -> "react"
  | Store_sreact _ -> "sreact"
  | Store_sreact_fun _ -> "sreact"
  | Store_int_param _ -> "int param"
  | Store_float_param _ -> "float param"

let _is_int f = 
  f -. (float (int_of_float f)) = 0.0 

let print_err msg p =
  Utils.print_pos p;
  prerr_endline msg

let type_err v f p =
  Utils.print_pos p;
  eprintf "Error: This expression has type %s\n" (get_type v);
  eprintf "       but an expression was expected of type %s" f;
  prerr_newline ();
  raise WRONG_TYPE

let unbound_err ide p  =
  Utils.print_pos p;
  prerr_endline ("Error: Unbound value " ^ ide);
  raise NO_IDE
    
let args_err ide forms acts p =
  Utils.print_pos p;
  eprintf "Error: %s expects %d argument(s) but is here used with %d argument(s)"
    ide forms acts;
  prerr_newline ();
  raise WRONG_TYPE

let pri_err p  =
  Utils.print_pos p;
  prerr_endline "Error: Invalid priority classes";
  raise INVALID_PRI
 
(* -consts a=2,b=3.4,c=inf*)
let parse_consts str env =
  let tokens = split (regexp_string ",") str in
  List.iter (fun s -> 
    match split (regexp_string "=") s with
    | [ide; v] -> begin 
      try
	Hashtbl.add env ide (Store_int (int_of_string v))
      with
      | Failure _ -> 
	try
	  Hashtbl.add env ide (Store_float (float_of_string v))
	with
	| Failure _ -> prerr_endline ("Error: invalid argument \"" ^ s ^ "\""); 
	  raise INVALID_CONSTS
    end
    | _ -> prerr_endline ("Error: invalid argument \"" ^ s ^ "\""); 
      raise INVALID_CONSTS) tokens

let get_int ide p env =
  try 
    match Hashtbl.find env ide with
    | Store_int v -> v
    | v -> type_err v "int" p
  with
  | Not_found -> unbound_err ide p

let get_int_m ide p env =
  try 
    match Hashtbl.find env ide with
    | Store_int v -> v
    | _ -> raise WRONG_TYPE
  with
  | Not_found -> unbound_err ide p

let get_float ide p env =
  try 
    match Hashtbl.find env ide with
    | Store_float v -> v
    | v -> type_err v "float" p
  with
  | Not_found -> unbound_err ide p

let get_float_m ide p env =
  try 
    match Hashtbl.find env ide with
    | Store_float v -> v
    | _ -> raise WRONG_TYPE
  with
  | Not_found -> unbound_err ide p

let get_ctrl ide p env =
  try 
    match Hashtbl.find env ide with
    | Store_ctrl c -> c
    | v -> type_err v "ctrl" p
  with
  | Not_found -> unbound_err ide p

let get_big ide p env =
  try 
    match Hashtbl.find env ide with
    | Store_big v -> v
    | v -> type_err v "big" p
  with
  | Not_found -> unbound_err ide p

let rec eval_int exp env =
  match exp with
  | Num_val (v, _) -> int_of_float v
  | Num_ide (ide, p) -> 
    (try get_int_m ide p env
     with
     | WRONG_TYPE -> 
       let v = get_float ide p env in
       if _is_int v then int_of_float v
       else type_err (Store_float 0.0) "int" p) 
  | Num_plus (l, r, _) -> (eval_int l env) + (eval_int r env)
  | Num_minus (l, r, _) -> (eval_int l env) - (eval_int r env)
  | Num_prod (l, r, _) -> (eval_int l env) * (eval_int r env)
  | Num_div (l, r, p) -> (try (eval_int l env) / (eval_int r env)
    with
    | Division_by_zero -> print_err "Error: Division by zero" p; raise INVALID_VAL)
  | Num_pow (l, r, _) -> int_of_float ((float (eval_int l env)) ** (float (eval_int r env))) 

(* No msg on WRONG_TYPE*)
let eval_int_m exp env =
  match exp with
  | Num_val (v, _) -> int_of_float v
  | Num_ide (ide, p) -> 
    (try get_int_m ide p env
     with
     | WRONG_TYPE -> 
       let v = get_float ide p env in
       if _is_int v then int_of_float v
       else raise WRONG_TYPE) 
  | Num_plus (l, r, _) -> (eval_int l env) + (eval_int r env)
  | Num_minus (l, r, _) -> (eval_int l env) - (eval_int r env)
  | Num_prod (l, r, _) -> (eval_int l env) * (eval_int r env)
  | Num_div (l, r, p) -> (try (eval_int l env) / (eval_int r env)
    with
    | Division_by_zero -> print_err "Error: Division by zero" p; raise INVALID_VAL)
  | Num_pow (l, r, _) -> int_of_float ((float (eval_int l env)) ** (float (eval_int r env)))

let rec eval_float exp env =
  match exp with
  | Num_val (v, _) -> v
  | Num_ide (ide, p) -> 
    (try get_float_m ide p env
     with
     | WRONG_TYPE -> float (get_int ide p env)) 
  | Num_plus (l, r, _) -> (eval_float l env) +. (eval_float r env)
  | Num_minus (l, r, _) -> (eval_float l env) -. (eval_float r env)
  | Num_prod (l, r, _) -> (eval_float l env) *. (eval_float r env)
  | Num_div (l, r, p) -> (let v = (eval_float l env) /. (eval_float r env) in
			  if v = nan then (print_err "Error: nan" p; raise INVALID_VAL)
			  else v)
  | Num_pow (l, r, _) -> (eval_float l env) ** (eval_float r env) 

let get_ctrl_fun ide acts p env =
  try 
    match Hashtbl.find env ide with
    | Store_ctrl_fun (ar, forms) -> 
      (let f_l = List.length forms
      and a_l = List.length acts in
       if f_l != a_l then args_err ide f_l a_l p
       else let acts_s = String.concat "," (List.map (fun exp ->
	 try
	   sprintf "%d" (eval_int_m exp env) (* MUTE *)
	 with
	 | WRONG_TYPE -> sprintf "%g" (eval_float exp env)) acts) in
	    Base.Ctrl (sprintf "%s(%s)" ide acts_s, ar))
    | v -> type_err v "ctrl" p
  with
  | Not_found -> unbound_err ide p

let scope env forms acts =
  let h = Hashtbl.copy env in
  List.iter (fun (ide, exp) ->
    let v = eval_float exp env in (* precision? *)
    if _is_int v then Hashtbl.add h ide (Store_int (int_of_float v))
    else Hashtbl.add h ide (Store_float v)) (List.combine forms acts);
  h

let eval_ion c names p  =
  try 
    Big.ion (Link.parse_face names) c
  with
  | Big.CTRL_ERROR (n, _) -> 
    (print_err ("Error: ctrl " ^ (Base.name_of_ctrl c) ^ " has arity " 
		^ (string_of_int n) ^ " but is here used with " 
		^ (string_of_int (List.length names)) ^ " name(s)") p; 
     raise INVALID_VAL)
 
let rec eval_big exp env =
  let get_big_fun ide acts p env =
    try (match Hashtbl.find env ide with
    | Store_big_fun (exp, forms) -> 
      (try eval_big exp (scope env forms acts)
       with
       | Invalid_argument _ -> args_err ide (List.length forms) (List.length acts) p) 
    | v -> type_err v "big" p)
    with
    | Not_found -> unbound_err ide p in
  match exp with
  | Big_ide (ide, p) -> get_big ide p env
  | Big_ide_fun(ide, acts, p) -> get_big_fun ide acts p env
  | Big_plac (l, roots, p) -> 
    (try Big.placing l roots Link.Face.empty
     with
     | _ -> print_err ("Error: Invalid placing expression") p; raise INVALID_VAL)
  | Big_comp (l, r, p) -> 
    (try Big.comp (eval_big l env) (eval_big r env)
     with
     | _ -> print_err ("Error: Invalid composition expression") p; raise INVALID_VAL)
  | Big_comp_c (closures, b, p) -> 
    (match closures with
    | Big_close (names, _) -> 
      (try Big.close (Link.parse_face names) (eval_big b env)
       with
       | _ -> print_err ("Error: Invalid composition expression") p; raise INVALID_VAL)
    | _ -> print_err ("Error: Invalid composition expression") p; raise INVALID_VAL)			
  | Big_close (names, p) -> 
    (try Big.ppar_of_list (List.map (fun n ->
      Big.closure (Link.Face.singleton (Link.Nam n))) names)
     with
     | _ -> print_err ("Error: Invalid closure") p; raise INVALID_VAL)
  | Big_tens (l, r, p) -> 
    (try Big.tens (eval_big l env) (eval_big r env)
     with
     | _ -> print_err ("Error: Invalid tensor product expression") p; raise INVALID_VAL)
  | Big_par (l, r, p) -> 
    (try Big.par (eval_big l env) (eval_big r env)
     with
     | _ -> print_err ("Error: Invalid merge product expression") p; raise INVALID_VAL)
  | Big_ppar (l, r, p) -> 
    (try Big.ppar (eval_big l env) (eval_big r env)
     with
     | _ -> print_err ("Error: Invalid parallel product expression") p; raise INVALID_VAL)
  | Big_nest (l, r, p) -> 
    (try match l with
    | Big_ion _ -> Big.nest (eval_big l env) (eval_big r env)
    | Big_ion_fun _ -> Big.nest (eval_big l env) (eval_big r env)
    | _ -> print_err ("Error: Invalid nesting expression 1") p; raise INVALID_VAL
     with
     | _ -> print_err ("Error: Invalid nesting expression") p; raise INVALID_VAL)
  | Big_el (v, p) -> 
    (match v with
    | 0 -> Big.zero
    | 1 -> Big.one
    | _ -> print_err ("Error: Expression " ^ (string_of_int v) ^ " is not a valid bigraph") p; 
      raise INVALID_VAL)
  | Big_id (n, names, _) -> Big.id (Big.Inter (n, (Link.parse_face names)))
  | Big_ion (c, names, p) -> eval_ion (get_ctrl c p env) names p
  | Big_ion_fun (c, names, acts, p) -> eval_ion (get_ctrl_fun c acts p env) names p
  | Big_share (a, psi, b, p) -> 
    (try Big.share (eval_big a env) (eval_big psi env) (eval_big b env)
     with
     | _ -> print_err ("Error: Invalid sharing expression") p; 
       raise INVALID_VAL)

let eval_react lhs rhs env p =
  let r = 
    { Brs.rdx = eval_big lhs env;
      Brs.rct = eval_big rhs env;
    } in
  if Brs.is_valid_react r then r 
  else (print_err ("Error: Invalid reaction") p; 
	raise INVALID_VAL)

let eval_sreact lhs rhs rate env p =
  let r = 
    { Sbrs.rdx = eval_big lhs env;
      Sbrs.rct = eval_big rhs env;
      Sbrs.rate = eval_float rate env;
    } in
  if Sbrs.is_valid_sreact r then r 
  else (print_err ("Error: Invalid stochastic reaction") p; 
	raise INVALID_VAL)

let store_decs decs env =
  let _dummy_acts =
    List.map (fun _ -> Num_val (0.0, (Lexing.dummy_pos, Lexing.dummy_pos))) in
  List.iter (fun d ->
    match d with
  | Ctrl_dec (ide, ar, _) ->
    Hashtbl.add env ide (Store_ctrl (Base.Ctrl (ide, ar)))
  | Ctrl_dec_f (ide, forms, ar, _) ->
    Hashtbl.add env ide (Store_ctrl_fun (ar, forms))
  | Int_dec (ide, exp, _) ->
    Hashtbl.add env ide (Store_int (eval_int exp env))
  | Float_dec (ide, exp, _) ->
    Hashtbl.add env ide (Store_float (eval_float exp env))
  | Big_dec (ide, exp, _) ->
    Hashtbl.add env ide (Store_big (eval_big exp env))
  | Big_dec_f (ide, forms, exp, _) ->
    ignore (eval_big exp (scope env forms (_dummy_acts forms)));
    Hashtbl.add env ide (Store_big_fun (exp, forms))
  | React_dec (ide, lhs, rhs, p) ->
    Hashtbl.add env ide (Store_react (eval_react lhs rhs env p))
  | React_dec_f (ide, forms, lhs, rhs, p) ->
    ignore (eval_react lhs rhs (scope env forms (_dummy_acts forms)) p);
    Hashtbl.add env ide (Store_react_fun (lhs, rhs, forms))
  | Sreact_dec (ide, lhs, rhs, exp, p) ->
    Hashtbl.add env ide (Store_sreact (eval_sreact lhs rhs exp env p))
  | Sreact_dec_f (ide, forms, lhs, rhs, exp, p) ->
    ignore (eval_sreact lhs rhs exp (scope env forms (_dummy_acts forms)) p);
    Hashtbl.add env ide (Store_sreact_fun (lhs, rhs, exp, forms))) decs

let get_react ide p env =
  try 
    match Hashtbl.find env ide with
    | Store_react r -> r
    | v -> type_err v "react" p
  with
  | Not_found -> unbound_err ide p

let _scope env forms acts =
    let h = Hashtbl.copy env in
    List.iter (fun (ide, v) ->
      Hashtbl.add h ide v) (List.combine forms acts);
    h 
  
let get_react_fun ide acts p env =
  try 
    match Hashtbl.find env ide with
    | Store_react_fun (lhs, rhs, forms) ->
      eval_react lhs rhs (_scope env forms acts) p
    | Store_react _ -> args_err ide 0 (List.length acts) p  
    | v -> type_err v "react" p
  with
  | Not_found -> unbound_err ide p

let get_sreact ide p env =
  try 
    match Hashtbl.find env ide with
    | Store_sreact r -> r
    | v -> type_err v "sreact" p
  with
  | Not_found -> unbound_err ide p

let get_sreact_fun ide acts p env =
  try 
    match Hashtbl.find env ide with
    | Store_sreact_fun (lhs, rhs, exp, forms) ->
      eval_sreact lhs rhs exp (_scope env forms acts) p
    | Store_react _ -> args_err ide 0 (List.length acts) p  
    | v -> type_err v "react" p
  with
  | Not_found -> unbound_err ide p

let store_params env =
  let store_param p env =
    match p with
    | Int_param (ide, [exp], _) -> Hashtbl.add env ide (Store_int_param [eval_int exp env])
    | Int_param (ide, exps, _) -> 
      Hashtbl.add env ide (Store_int_param (List.map (fun exp ->
	eval_int exp env) exps))
    | Float_param (ide, [exp], _) -> Hashtbl.add env ide (Store_float_param [eval_float exp env])
    | Float_param (ide, exps, _) -> 
      Hashtbl.add env ide (Store_float_param (List.map (fun exp ->
	eval_float exp env) exps)) in
  List.iter (fun p -> store_param p env)

let eval_init init env =
  match init with
  | Init (ide, p) -> eval_big (Big_ide (ide, p)) env
  | Init_fun (ide, acts, p) -> eval_big (Big_ide_fun (ide, acts, p)) env

(* return a list of store_val: the possible values of ide *)
let get_param_m ide p env =
  try 
    match Hashtbl.find env ide with
    | Store_int_param ints -> List.map (fun x -> Store_int x) ints 
    | Store_float_param	 floats -> List.map (fun x -> Store_float x) floats
    | _ -> raise WRONG_TYPE
  with
  | Not_found -> unbound_err ide p

(*ide -> params -> ide(params)*)
let _build_react_ide ide params =
  sprintf "%s(%s)" ide (String.concat "," (List.map (fun p ->
  match p with
  | Store_int x -> sprintf "%d" x 
  | Store_float x -> sprintf "%g" x
  | _ -> raise WRONG_TYPE) params)) 

let _build_params ides env p =
  Utils.par_comb (List.map (fun ide -> 
    try 
      get_param_m ide p env
    with
    | WRONG_TYPE -> 
      try
	[Store_int (get_int_m ide p env)]
      with
      | WRONG_TYPE -> [Store_float (get_float ide p env)]) ides)

let store_rule r env =
  match r with
  | Rul_fun (ide, ides, p) -> begin
    (* get param values *)
    let params =_build_params ides env p in
    (* get react_fun *)
    let reacts_store =
      List.map (fun acts ->
	_build_react_ide ide acts, get_react_fun ide acts p env) params in
    (* Update store  - check performances *)
    List.iter (fun (ide, v) -> Hashtbl.add env ide (Store_react v)) reacts_store;
      fst (List.split reacts_store)
     end
  | Rul (ide, p) -> ignore (get_react ide p env); [ide]

let store_srule r env =
 match r with
  | Rul_fun (ide, ides, p) -> begin
    (* get param values *)
    let params = _build_params ides env p in
    (* get react_fun *)
    let reacts_store =
      List.map (fun acts ->
	_build_react_ide ide acts, get_sreact_fun ide acts p env) params in
    (* Update store - check performances *)
    List.iter (fun (ide, v) -> Hashtbl.add env ide (Store_sreact v)) reacts_store;
    fst (List.split reacts_store)
  end
  | Rul (ide, p) -> ignore (get_sreact ide p env); [ide]

let store_pclass (c : pri_class) env =
  let _aux c p =
    if Brs.is_valid_p_ide (fun ide -> get_react ide p env) (p_to_brs c) then c
    else pri_err p in
  match c with
  | Pri_class (rules, p) -> 
    (let c = P_class_ide (List.fold_left (fun acc r ->
    acc @ (store_rule r env)) [] rules) in
     _aux c p)
  | Pri_classr (rules, p) -> 
    (let c = P_rclass_ide (List.fold_left (fun acc r ->
      acc @ (store_rule r env)) [] rules) in
     _aux c p)

(* check validity of classes *)
let store_spclass c env =
  let _aux c p =
    if Sbrs.is_valid_p_ide (fun ide -> get_sreact ide p env) (p_to_sbrs c) then c
    else pri_err p in
  match c with
  | Pri_class (rules, p) -> 
    (let c = P_class_ide (List.fold_left (fun acc r ->
      acc @ (store_srule r env)) [] rules) in
     _aux c p)
  | Pri_classr (rules, p) -> 
    (let c = P_rclass_ide (List.fold_left (fun acc r ->
      acc @ (store_srule r env)) [] rules) in
     _aux c p)

let store_brs brs env  =
  match brs with
  | Brs (params, init, p_classes, p) -> begin
    store_params env params;
    let p_ide_list = List.fold_left (fun acc c -> 
      acc @ [store_pclass c env]) [] p_classes in
    if is_valid_p_ide (fun ide -> get_react ide p env) p_ide_list 
    then eval_init init env, false, p_ide_list
    else pri_err p
					
  end
  | Sbrs (params, init, p_classes, _) -> begin
    store_params env params;
    eval_init init env, true, List.fold_left (fun acc c ->
      acc @ [store_spclass c env]) [] p_classes
  end



  


