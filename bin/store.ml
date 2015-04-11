open Format
open Ast
open Unify
       
type store_type =
  [ `num_val of num_type
  | `big_val of fun_type
  | `lambda of lambda ]

let string_of_store_t = function
  | `num_val t -> string_of_num_t t
  | `big_val t -> string_of_fun_t t
  | `lambda t -> string_of_lambda t

let dom_of_lambda = function
  | `num_val _ | `big_val _ -> assert false
  | `lambda t -> fst t
		     
type store_val =
  | Int of int
  | Float of float
  | Big of Big.bg
  | Big_fun of big_exp * Id.t list
  | Ctrl of Base.Ctrl.t
  | Ctrl_fun of int * Id.t list
  | A_ctrl of Base.Ctrl.t
  | A_ctrl_fun of int * Id.t list
  | React of Brs.react
  | React_fun of big_exp * big_exp * int Fun.t option * Id.t list
  | Sreact of Sbrs.sreact
  | Sreact_fun of big_exp * big_exp * int Fun.t option * float_exp * Id.t list
  | Int_param of int list
  | Float_param of float list

let string_of_store_val = function
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | Big x -> Big.to_string x
  | Big_fun _ -> "<fun big>" 
  | A_ctrl c | Ctrl c -> Base.Ctrl.to_string c
  | A_ctrl_fun _ | Ctrl_fun _ -> "<fun ctrl>"   
  | React r -> Brs.string_of_react r
  | React_fun _ -> "<fun react>"
  | Sreact r -> Sbrs.string_of_sreact r
  | Sreact_fun _ -> "<fun sreact>"
  | Int_param p -> "("
		   ^ (String.concat "," (List.map string_of_int p))
		   ^ ")"
  | Float_param p -> "("
		     ^ (String.concat "," (List.map string_of_float p))
		     ^ ")"

let def_val = function
  | `g _ -> Num_int_val (0, Loc.dummy_loc)
  | `b `int -> Num_int_val (0, Loc.dummy_loc)
  | `b `float -> Num_float_val (0.0, Loc.dummy_loc)

type typed_store_val = store_val * store_type * Loc.t

let assign_type (v : store_val) env_t =
  let assign_type_forms =
    List.map (fun _ -> next_gen ()) in
  let update forms t =
    let fresh_t = assign_type_forms forms in 
    (`lambda (box_gen_types fresh_t, t), add_types fresh_t env_t) in
  match v with
  | Int _ -> (`num_val (`b `int), env_t)
  | Float _ -> (`num_val (`b `float), env_t)
  | Big _ -> (`big_val `big, env_t)
  | Big_fun (_, forms) -> update forms `big
  | Ctrl c | A_ctrl c -> (`big_val (`ctrl (Base.Ctrl.arity c)), env_t)
  | Ctrl_fun (arity, forms)
  | A_ctrl_fun (arity, forms) -> update forms (`ctrl arity)
  | React _ -> (`big_val `react, env_t)
  | React_fun (_, _, _, forms) -> update forms `react
  | Sreact _ -> (`big_val `sreact, env_t)
  | Sreact_fun (_, _, _, _, forms) -> update forms `sreact
  | Int_param _ -> (`num_val (`b `int), env_t)
  | Float_param _ -> (`num_val (`b `float), env_t)

type store = (Id.t, typed_store_val) Hashtbl.t

let get_val ((v, _, _) : typed_store_val) = v
					      
let get_type ((_ , t, _) : typed_store_val) = t
						
let get_pos ((_, _, p) : typed_store_val) = p

let bindings = Hashtbl.length
					      
type p_class_list = P of Brs.p_class list | S of Sbrs.p_class list
							      
type error =
  | Wrong_type of store_type * store_type    (*  (current, expected)     *)
  | Atomic_ctrl of Id.t
  | Arity of string * int * int              (*  (id, current, expected) *)
  | Unbound_variable of Id.t
  | Div_by_zero
  | Comp of Big.inter * Big.inter
  | Tens of Link.Face.t * Link.Face.t        (* (in , out) *)
  | Share
  | Unknown_big of int
  | Reaction of string		             (* error message *) 
		  
type warning =
  | Multiple_declaration of Id.t * Loc.t * Loc.t

exception ERROR of error * Loc.t

let report_error_aux fmt = function
  | Wrong_type (curr, exp) ->
     fprintf fmt "This expression has type %s but an expression was expected of type %s"
	     (string_of_store_t curr) (string_of_store_t exp)
  | Atomic_ctrl id ->
     fprintf fmt "Control %s is atomic but it is here used in a nesting expression" id
  | Arity (id, ar, ar_dec) ->
     fprintf fmt "Control %s has arity %d but a control of arity %d was expected" id ar ar_dec
  | Unbound_variable s -> fprintf fmt "Unbound variable %s" s
  | Div_by_zero -> fprintf fmt "Division by zero"
  | Comp (i, j) ->
     fprintf fmt "Interfaces %s and %s do not match in the composition"
	     (Big.string_of_inter i) (Big.string_of_inter j)
  | Tens (inner, outer) ->
     fprintf fmt "Tensor product has common inner names %s and outer names %s"
	     (Link.string_of_face inner) (Link.string_of_face outer)
  | Share -> fprintf fmt "Invalid sharing expression"
  | Unknown_big v ->
     fprintf fmt "Expression %d is not a valid bigraph" v
  | Reaction msg -> fprintf fmt "%s" msg

let report_error fmt err =
  fprintf fmt "@[%s: %a@]@." Utils.err report_error_aux err
  
let report_warning fmt = function
  | Multiple_declaration (id, p, p') ->
     fprintf fmt "%a@[%s: Identifier %s was already used at %s@]@."
	     Loc.print_loc p' Utils.warn id (Loc.string_of_pos p)

(******** SCOPE *********)

module ScopeMap = Map.Make(struct
			      type t = Id.t
			      let compare = Id.compare
			    end)

type scope = typed_store_val ScopeMap.t
			     
(******** GET FUNCTIONS *********)
			     
let fetch id p (aux : typed_store_val -> 'a) (scope : scope) (env : store) =
  try aux (ScopeMap.find id scope) with
  | Not_found ->
     try aux (Hashtbl.find env id) with
     | Not_found -> raise (ERROR (Unbound_variable id, p))
			  
let get_int id p scope env =
  let aux = function
    | (Int v, _, _) -> v
    | (Float _,t,_)
    | (Big _,t,_)
    | (Big_fun (_,_),t,_)
    | (Ctrl _,t,_)
    | (Ctrl_fun (_,_),t,_)
    | (A_ctrl _,t,_)
    | (A_ctrl_fun (_,_),t,_)
    | (React _,t,_)
    | (React_fun (_,_,_,_),t,_)
    | (Sreact _,t,_)
    | (Sreact_fun (_,_,_,_,_),t,_)
    | (Int_param _,t,_)
    | (Float_param _,t,_) ->
       raise (ERROR (Wrong_type (t, `num_val (`b `int)), p)) in
  fetch id p aux scope env
	
let get_float id p scope env =
  let aux = function
    | (Float v, _, _) -> v
    | (Int _,t,_)
    | (Big _,t,_)
    | (Big_fun (_,_),t,_)
    | (Ctrl _,t,_)
    | (Ctrl_fun (_,_),t,_)
    | (A_ctrl _,t,_)
    | (A_ctrl_fun (_,_),t,_)
    | (React _,t,_)
    | (React_fun (_,_,_,_),t,_)
    | (Sreact _,t,_)
    | (Sreact_fun (_,_,_,_,_),t,_)
    | (Int_param _,t,_)
    | (Float_param _,t,_) ->
       raise (ERROR (Wrong_type (t, `num_val (`b `float)), p)) in
  fetch id p aux scope env

let get_num id p scope env =
  let aux = function 
    | (Float _ as v, _, _) 
    | (Int _ as v, _, _) -> v			       
    | (Big _,t,_)
    | (Big_fun (_,_),t,_)
    | (Ctrl _,t,_)
    | (Ctrl_fun (_,_),t,_)
    | (A_ctrl _,t,_)
    | (A_ctrl_fun (_,_),t,_)
    | (React _,t,_)
    | (React_fun (_,_,_,_),t,_)
    | (Sreact _,t,_)
    | (Sreact_fun (_,_,_,_,_),t,_)
    | (Int_param _,t,_)
    | (Float_param _,t,_) ->
       raise (ERROR (Wrong_type (t, `num_val (`g int_or_float)), p)) in
  fetch id p aux scope env
	
let get_ctrl id arity p env =
  try
    match Hashtbl.find env id with
    | (A_ctrl c, _, _)
    | (Ctrl c, _, _) ->
       (let a = Base.Ctrl.arity c in
	if a = arity then c
	else raise (ERROR (Arity (id, a, arity), p)))
    | (Int _,t,_)
    | (Float _,t,_)
    | (Big _,t,_)
    | (Big_fun (_,_),t,_)
    | (Ctrl_fun (_,_),t,_)
    | (A_ctrl_fun (_,_),t,_)
    | (React _,t,_)
    | (React_fun (_,_,_,_),t,_)
    | (Sreact _,t,_)
    | (Sreact_fun (_,_,_,_,_),t,_)
    | (Int_param _,t,_)
    | (Float_param _,t,_) -> raise (ERROR (Wrong_type (t, `big_val (`ctrl arity)), p)) 
  with
  | Not_found -> raise (ERROR (Unbound_variable id, p))

let get_ctrl_fun id arity act_types p env =
  try
    match Hashtbl.find env id with
    | (A_ctrl_fun (a, forms), t, _)
    | (Ctrl_fun (a, forms), t, _) ->
       (if a = arity then (a, forms, t) else raise (ERROR (Arity (id, a, arity), p))) 
    | (Int _,t,_)
    | (Float _,t,_)
    | (Big _,t,_)
    | (Big_fun (_,_),t,_)
    | (Ctrl _,t,_)
    | (A_ctrl _,t,_)
    | (React _,t,_)
    | (React_fun (_,_,_,_),t,_)
    | (Sreact _,t,_)
    | (Sreact_fun (_,_,_,_,_),t,_)
    | (Int_param _,t,_)
    | (Float_param _,t,_) -> raise (ERROR (Wrong_type (t, `lambda (act_types, `ctrl arity)), p)) 
  with
  | Not_found -> raise (ERROR (Unbound_variable id, p))		       
		       
let is_atomic id p env =
  try 
    match get_val (Hashtbl.find env id) with
    | A_ctrl _ |  A_ctrl_fun _ -> true
    | Int _
    | Float _
    | Big _
    | Big_fun (_,_)
    | Ctrl _
    | Ctrl_fun (_,_)
    | React _
    | React_fun (_,_,_,_)
    | Sreact _
    | Sreact_fun (_,_,_,_,_)
    | Int_param _
    | Float_param _ -> false
  with
  | Not_found -> raise (ERROR (Unbound_variable id, p))

let get_big id p env =
  try
    match Hashtbl.find env id with
    | (Big b, _, _) -> b
    | (Int _,t,_)
    | (Float _,t,_)
    | (Big_fun (_,_),t,_)
    | (Ctrl _,t,_)
    | (Ctrl_fun (_,_),t,_)
    | (A_ctrl _,t,_)
    | (A_ctrl_fun (_,_),t,_)
    | (React _,t,_)
    | (React_fun (_,_,_,_),t,_)
    | (Sreact _,t,_)
    | (Sreact_fun (_,_,_,_,_),t,_)
    | (Int_param _,t,_)
    | (Float_param _,t,_) -> raise (ERROR (Wrong_type (t, `big_val `big), p))
  with
  | Not_found -> raise (ERROR (Unbound_variable id, p))

let get_big_fun id arg_types p env =
  try
    match Hashtbl.find env id with
    | (Big_fun (exp, forms), t, _) -> (exp, forms, t)
    | (Int _,t,_)
    | (Float _,t,_)
    | (Big _,t,_)
    | (Ctrl _,t,_)
    | (Ctrl_fun (_,_),t,_)
    | (A_ctrl _,t,_)
    | (A_ctrl_fun (_,_),t,_)
    | (React _,t,_)
    | (React_fun (_,_,_,_),t,_)
    | (Sreact _,t,_)
    | (Sreact_fun (_,_,_,_,_),t,_)
    | (Int_param _,t,_)
    | (Float_param _,t,_) -> raise (ERROR (Wrong_type (t, `lambda (arg_types, `big)), p)) 
  with
  | Not_found -> raise (ERROR (Unbound_variable id, p))		       

let get_react id p (env : store) =
  try
    match Hashtbl.find env id with
    | (React r, _, _) -> r
    | (Int _,t,_)
    | (Float _,t,_)
    | (Big _,t,_)
    | (Big_fun (_,_),t,_)
    | (Ctrl _,t,_)
    | (Ctrl_fun (_,_),t,_)
    | (A_ctrl _,t,_)
    | (A_ctrl_fun (_,_),t,_)
    | (React_fun (_,_,_,_),t,_)
    | (Sreact _,t,_)
    | (Sreact_fun (_,_,_,_,_),t,_)
    | (Int_param _,t,_)
    | (Float_param _,t,_) -> raise (ERROR (Wrong_type (t, `big_val `react), p))
  with
  | Not_found -> raise (ERROR (Unbound_variable id, p))

let get_react_fun id arg_types p env =
  try
    match Hashtbl.find env id with
    | (React_fun (l, r, eta, forms), t, _) -> (l, r, eta, forms, t)
    | (Int _,t,_)
    | (Float _,t,_)
    | (Big _,t,_)
    | (Big_fun (_,_),t,_)
    | (Ctrl _,t,_)
    | (Ctrl_fun (_,_),t,_)
    | (A_ctrl _,t,_)
    | (A_ctrl_fun (_,_),t,_)
    | (React _,t,_)
    | (Sreact _,t,_)
    | (Sreact_fun (_,_,_,_,_),t,_)
    | (Int_param _,t,_)
    | (Float_param _,t,_) -> raise (ERROR (Wrong_type (t, `lambda (arg_types, `react)), p)) 
  with
  | Not_found -> raise (ERROR (Unbound_variable id, p))		       
		       
let get_sreact id p (env : store) =
  try
    match Hashtbl.find env id with
    | (Sreact r, _, _) -> r
    | (Int _,t,_)
    | (Float _,t,_)
    | (Big _,t,_)
    | (Big_fun (_,_),t,_)
    | (Ctrl _,t,_)
    | (Ctrl_fun (_,_),t,_)
    | (A_ctrl _,t,_)
    | (A_ctrl_fun (_,_),t,_)
    | (React _,t,_)
    | (React_fun (_,_,_,_),t,_)
    | (Sreact_fun (_,_,_,_,_),t,_)
    | (Int_param _,t,_)
    | (Float_param _,t,_) -> raise (ERROR (Wrong_type (t, `big_val `sreact), p))
  with
  | Not_found -> raise (ERROR (Unbound_variable id, p))

let get_sreact_fun id arg_types p env =
  try
    match Hashtbl.find env id with
    | (Sreact_fun (l, r, eta, rate, forms), t, _) -> (l, r, eta, rate, forms, t)
    | (Int _,t,_)
    | (Float _,t,_)
    | (Big _,t,_)
    | (Big_fun (_,_),t,_)
    | (Ctrl _,t,_)
    | (Ctrl_fun (_,_),t,_)
    | (A_ctrl _,t,_)
    | (A_ctrl_fun (_,_),t,_)
    | (React _,t,_)
    | (React_fun (_,_,_,_),t,_)
    | (Sreact _,t,_)
    | (Int_param _,t,_)
    | (Float_param _,t,_) -> raise (ERROR (Wrong_type (t, `lambda (arg_types, `sreact)), p)) 
  with
  | Not_found -> raise (ERROR (Unbound_variable id, p))		       
		       
(******** EVAL FUNCTIONS *********)
		       
let div_int l r p =
  match r with
  | 0 -> raise (ERROR (Div_by_zero, p))
  | d -> l / d

let rec pow_int_aux base exp acc =
  match exp with
  | 0 -> 1
  | _ -> pow_int_aux base (exp - 1) (acc * base) 

let pow_int b e p =
  if e < 0 then
    raise (ERROR (Wrong_type (`num_val (`b `float), `num_val (`b `int)), p))
  else pow_int_aux b e 1

let rec eval_int (exp : int_exp) (scope : scope) (env : store) =
  match exp with
  | Int_val (v, _) -> v
  | Int_var (ide, p) -> get_int ide p scope env
  | Int_plus (l, r, _) -> (eval_int l scope env) + (eval_int r scope env)
  | Int_minus (l, r, _) -> (eval_int l scope env) - (eval_int r scope env)
  | Int_prod (l, r, _) -> (eval_int l scope env) * (eval_int r scope env)
  | Int_div (l, r, p) -> div_int (eval_int r scope env) (eval_int l scope env) p
  | Int_pow (l, r, p) -> pow_int (eval_int l scope env) (eval_int r scope env) p

let rec eval_float (exp : float_exp) (scope : scope) env =
  match exp with
  | Float_val (v, _) -> v
  | Float_var (ide, p) -> get_float ide p scope env
  | Float_plus (l, r, _) -> (eval_float l scope env) +. (eval_float r scope env)
  | Float_minus (l, r, _) -> (eval_float l scope env) -. (eval_float r scope env)
  | Float_prod (l, r, _) -> (eval_float l scope env) *. (eval_float r scope env)
  | Float_div (l, r, _) -> (eval_float l scope env) /. (eval_float r scope env)
  | Float_pow (l, r, _) -> (eval_float l scope env) ** (eval_float r scope env)

let rec eval_num (exp : num_exp) (scope : scope) (env : store) =
  let aux val0 val1 fun_int fun_float p =
    match (val0, val1) with
    | (Int v, Int v') -> Int (fun_int v v')
    | (Float v, Float v') -> Float (fun_float v v')
    | (Int _, Float _) ->
       raise (ERROR (Wrong_type (`num_val (`b `float), `num_val (`b `int)), p))
    | (Float _, Int _) ->
       raise (ERROR (Wrong_type (`num_val (`b `int), `num_val (`b `float)), p))
    | (Int _,_)
    | (Float _,_)
    | (Big _,_)
    | (Big_fun (_,_),_)
    | (Ctrl _,_)
    | (Ctrl_fun (_,_),_)
    | (A_ctrl _,_)
    | (A_ctrl_fun (_,_),_)
    | (React _,_)
    | (React_fun (_,_,_,_),_)
    | (Sreact _,_)
    | (Sreact_fun (_,_,_,_,_),_)
    | (Int_param _,_)
    | (Float_param _,_) -> assert false in
  match exp with
  | Num_int_val (v, _) -> Int v
  | Num_float_val (v, _) -> Float v
  | Num_var (id, p) -> get_num id p scope env
  | Num_plus (l, r, p) ->
     aux (eval_num l scope env) (eval_num r scope env) (+) (+.) p     
  | Num_minus (l, r, p) ->
     aux (eval_num l scope env) (eval_num r scope env) (-) (-.) p
  | Num_prod (l, r, p) ->
     aux (eval_num l scope env) (eval_num r scope env) ( * ) ( *. ) p
  | Num_div (l, r, p) ->
     (let f_int l r = div_int l r p in
      aux (eval_num l scope env) (eval_num r scope env) f_int (/.) p)
  | Num_pow (l, r, p) ->
     (let f_int l r = pow_int l r p in
      aux (eval_num l scope env) (eval_num r scope env) f_int ( ** ) p)

let eval_nums exps (scope : scope) (env : store) =
  List.map (fun e ->
	    match eval_num e scope env with
	    | Int _ as v -> (v, `b `int)
	    | Float _ as v -> (v, `b `float)
	    | Big _ 
	    | Big_fun (_,_)
	    | Ctrl _
	    | Ctrl_fun (_,_)
	    | A_ctrl _
	    | A_ctrl_fun (_,_)
	    | React _
	    | React_fun (_,_,_,_)
	    | Sreact _
	    | Sreact_fun (_,_,_,_,_)
	    | Int_param _ 
	    | Float_param _ -> assert false
	   ) exps
  |> List.split
       
let extend_scope (scope: scope) (forms : Id.t list) (nums : store_val list)
		 (args_t : num_type list) (p : Loc.t) =
  let aux id v t = ScopeMap.add id (v, t, p) in
  List.combine forms (List.combine nums args_t)
  |> List.fold_left (fun s (id, (v, t)) ->
		     aux id v (`num_val t) s) scope

let eval_ctrl_fun id nums arity =
  let id' =
    id ^ "("
    ^ (String.concat "," (List.map string_of_store_val nums))
    ^ ")" in 
  Base.Ctrl.Ctrl (id', arity)

let check_atomic id p env face c = function
  | true ->
     (if is_atomic id p env then Big.atom face c
      else Big.ion face c)
  | false ->
     (if is_atomic id p env then raise (ERROR (Atomic_ctrl id, p))
      else Big.ion face c)
       
(* flag=true  Atomic controls allowed *)
let eval_ion scope env env_t flag = function
  | Big_ion_exp (id, names, p) ->
     (let c = get_ctrl id (List.length names) p env
      and face = Link.parse_face names in
      (check_atomic id p env face c flag, env_t))
  | Big_ion_fun_exp (id, args, names, p) ->
     (let (nums, args_t) = eval_nums args scope env
      and face = Link.parse_face names in
      let (a, _, t) =
	get_ctrl_fun id (List.length names) args_t p env in
      try
	let env_t' = app_exn env_t (dom_of_lambda t) args_t in
	let c = eval_ctrl_fun id nums a in
	(check_atomic id p env face c flag, env_t')
      with
      | UNIFICATION ->
	 raise (ERROR (Wrong_type (`lambda (args_t, `big), t), p)))
       
let rec eval_big (exp : big_exp) (scope : scope)
		 (env : store) (env_t : store_t) =
  let binary_eval l r scope env env_t f =
    let (l_v, env_t') = eval_big l scope env env_t in
    let (r_v, env_t'') = eval_big r scope env env_t' in
    (f l_v r_v, env_t'') in
  match exp with
  | Big_var (id, p) -> (get_big id p env, env_t)
  | Big_var_fun (id, args, p) ->
     (* fun b(x,y) = 1; fun big a(x,y) = b(x + y, 5); a(3, 2 + 3.4); *) 
     (let (nums, args_t) =
	(* id = a --> ([Int 3; Float 5], [`b `int; `b `float]) *)
	eval_nums args scope env in
      (* (exp, [x; y], [`g 0; `g 1]) *)
      let (exp, forms, t) =
	get_big_fun id args_t p env in
      try
	(* Unification: `g 0 -> `b `int ; `g 1 -> `b `float *)
	let env_t' = app_exn env_t (dom_of_lambda t) args_t in
	(* Extend scope:  x -> Int 3 y -> Float 5 *)
	let scope' = extend_scope scope forms nums args_t p in
	eval_big exp scope' env env_t' 
      with
      | UNIFICATION ->
	 raise (ERROR (Wrong_type (`lambda (args_t, `big), t), p)))
  | Big_new_name (n, _) -> 
     (Big.intro (Link.Face.singleton (Link.Nam n)), env_t)
  | Big_comp (l, r, p) -> 
     (try binary_eval l r scope env env_t Big.comp with
      | Big.COMP_ERROR (i, j) -> raise (ERROR (Comp (i, j), p)))
  | Big_tens (l, r, p) -> 
     (try (binary_eval l r scope env env_t Big.tens) with
      | Link.FACES_MISMATCH (i, o) -> raise (ERROR (Tens (i, o), p)))
  | Big_par (l, r, _) -> 
     binary_eval l r scope env env_t Big.par
  | Big_ppar (l, r, _) -> 
     binary_eval l r scope env env_t Big.ppar
  | Big_share (a, psi, b, _) -> 
     (try
	 let (a_v, env_t') = eval_big a scope env env_t in
	 let (psi_v, env_t'') = eval_big psi scope env env_t' in
	 let (b_v, env_t''') = eval_big b scope env env_t'' in
	 (Big.share a_v psi_v b_v, env_t''')
       with
       | Big.SHARING_ERROR -> raise (ERROR (Share, loc_of_big_exp psi)))
  | Big_num (v, p) -> 
     (match v with
      | 0 -> (Big.zero, env_t)
      | 1 -> (Big.one, env_t)
      | _ -> raise (ERROR (Unknown_big v, p)))
  | Big_id exp ->
     (Big.id (Big.Inter (exp.id_place, Link.parse_face (exp.id_link))),
      env_t)
  | Big_plc exp ->
     (Big.placing exp.plc_parents exp.plc_roots Link.Face.empty,
      env_t)
  | Big_nest (ion, b, _) -> (* No atomic controls allowed here *)
     (let (i_v, env_t') = eval_ion scope env env_t false ion in
      let (b_v, env_t'') = eval_big b scope env env_t' in
      (Big.nest i_v b_v, env_t''))
  | Big_ion ion -> eval_ion scope env env_t true ion
  | Big_close exp ->
     (Big.closure (Link.parse_face [exp.cl_name]), env_t)
  | Big_closures (c, b, _) ->
     (let (b_v, env_t') = eval_big b scope env env_t in
      (Big.close (Link.parse_face (names_of_closures c)) b_v,
       env_t'))
       
let eval_eta = function
  | Some (l, _) -> Some (Fun.parse l)
  | None -> None

(* Similar to binary eval *)	      
let eval_react_aux lhs rhs scope env env_t =
  let (lhs_v, env_t') = eval_big lhs scope env env_t in
  let (rhs_v, env_t'') = eval_big rhs scope env env_t' in
  (lhs_v, rhs_v, env_t'')
    
let eval_react lhs rhs eta scope env env_t p =
  let (lhs_v, rhs_v, env_t') =
    eval_react_aux lhs rhs scope env env_t in
  let r = { Brs.rdx = lhs_v;
	    Brs.rct = rhs_v;
	    (* Brs.eta = eval_eta eta; *)
	  } in
  (* Get more informative messages from Brs *)
  if Brs.is_valid_react r then (r, env_t') 
  else raise (ERROR (Reaction "Invalid reaction", p))

let eval_sreact lhs rhs eta rate scope env env_t p =
  let (lhs_v, rhs_v, env_t') =
    eval_react_aux lhs rhs scope env env_t in 
  let r = { Sbrs.rdx = lhs_v;
	    Sbrs.rct = rhs_v;
	    Sbrs.rate = eval_float rate scope env;
	  } in
  (* Get more informative messages from Sbrs *)
  if Sbrs.is_valid_sreact r then (r, env_t') 
  else raise (ERROR (Reaction "Invalid stochastic reaction", p))

(* Compute all the combinations of input values *)	  
let rec param_comb (pars : typed_store_val list list) = 
  match pars with
  | [x] -> List.map (fun v -> [v]) x
  | x :: xs ->
     (let aux1 v ls =
	List.map (fun l -> v :: l) ls
      in let rec aux2 l ls = 
	   match l with
	   | [] -> []
	   | x :: xs -> (aux1 x ls) @ (aux2 xs ls)
	 in aux2 x (param_comb xs))
  | [] -> []

let param_to_vals = function
  | (Int_param vals, _, p) ->
     List.map (fun v -> (Int v, `num_val (`b `int), p)) vals
  | (Float_param vals, _, p) ->
     List.map (fun v -> (Float v, `num_val (`b `float), p)) vals
  | (Int _, _, _)
  | (Float _, _, _)
  | (Big _, _, _)
  | (Big_fun (_, _), _, _)
  | (Ctrl _, _, _)
  | (Ctrl_fun (_, _) ,_, _)
  | (A_ctrl _,_,_)
  | (A_ctrl_fun (_,_),_,_)
  | (React _,_,_)
  | (React_fun (_,_,_,_),_,_)
  | (Sreact _,_,_)
  | (Sreact_fun (_,_,_,_,_),_,_) -> assert false
					   
let is_param id env p =
  try
    match get_val (Hashtbl.find env id) with
    | Int_param _ 
    | Float_param _ -> true
    | Int _
    | Float _
    | Big _
    | Big_fun (_,_)
    | Ctrl _
    | Ctrl_fun (_,_)
    | A_ctrl _
    | A_ctrl_fun (_,_)
    | React _
    | React_fun (_,_,_,_)
    | Sreact _
    | Sreact_fun (_,_,_,_,_) -> false
  with
  | Not_found -> raise (ERROR (Unbound_variable id, p))

module IdSet = Set.Make(struct
			   type t = Id.t
			   let compare = Id.compare
			 end)

(* [a + 3.0; c] -> [a , c] *)		 
let scan_for_params env args =
  let rec aux acc = function
    | Num_int_val _ 
    | Num_float_val _ -> acc
    | Num_var (id, p) ->
       if is_param id env p then IdSet.add id acc else acc
    | Num_plus (l, r, _)
    | Num_minus (l, r, _)
    | Num_prod (l, r, _)
    | Num_div (l, r, _)
    | Num_pow (l, r, _) -> aux (aux acc l) r in
  List.fold_left aux IdSet.empty args
  |> IdSet.elements

(* a = [1.0; 2.0] c = [4; 8] 
   [(a = 1.0; c = 4); (a = 1.0; c = 8); (a = 2.0; c = 4); (a = 2.0; c = 8)] *)		 
let param_scopes env ids =
  List.map (fun id -> param_to_vals (Hashtbl.find env id)) ids
  |> param_comb
  |> List.map (fun comb ->
	       List.fold_left2 (fun acc id v ->
				ScopeMap.add id v acc
			       ) ScopeMap.empty ids comb)

let eval_react_fun_app id args env env_t p =
  scan_for_params env args
  |> param_scopes env
  |> List.fold_left (fun (acc, env_t) scope ->
		     let (nums, args_t) = eval_nums args scope env in
		     let (l, r, eta, forms, t) = get_react_fun id args_t p env in
		     try
		       let env_t' = app_exn env_t (dom_of_lambda t) args_t in
		       let scope' = extend_scope scope forms nums args_t p in
		       let (r, env_t'') = eval_react l r eta scope' env env_t' p in
		       (r :: acc, env_t'')
		     with
		     | UNIFICATION ->
			raise (ERROR (Wrong_type (`lambda (args_t, `react), t), p))
		    ) ([], env_t)

let eval_sreact_fun_app id args env env_t p =
  scan_for_params env args
  |> param_scopes env
  |> List.fold_left (fun (acc, env_t) scope ->
		     let (nums, args_t) = eval_nums args scope env in
		     let (l, r, eta, rate, forms, t) = get_sreact_fun id args_t p env in
		     try
		       let env_t' = app_exn env_t (dom_of_lambda t) args_t in
		       let scope' = extend_scope scope forms nums args_t p in
		       let (r, env_t'') = eval_sreact l r eta rate scope' env env_t' p in
		       (r :: acc, env_t'')
		     with
		     | UNIFICATION ->
			raise (ERROR (Wrong_type (`lambda (args_t, `react), t), p))
		    ) ([], env_t)

let eval_pr env env_t pr =
  let aux env_t = function
    | Rul_id (id, p) -> ([get_react id p env], env_t)
    | Rul_id_fun (id, args, p) -> eval_react_fun_app id args env env_t p in
  let aux' (acc, env_t) id =
    let (rs, env_t') = aux env_t id in
    (acc @ rs, env_t') in 
  match pr with
  | Pr (ids, _) -> let (rs, env_t') = List.fold_left aux' ([], env_t) ids in
		   (Brs.P_class rs, env_t') 
  | Pr_red (ids, _) -> let (rs, env_t') = List.fold_left aux' ([], env_t) ids in
		       (Brs.P_rclass rs, env_t')

let eval_p_list eval_f env env_t =
  List.fold_left (fun (acc, env_t) pr ->
		  let (pr_class, env_t') = eval_f env env_t pr in
		  (pr_class :: acc, env_t')
		 ) ([], env_t)
		 
let eval_prs = eval_p_list eval_pr
			   
let eval_spr env env_t pr =
  let aux env_t = function
    | Srul_id (id, p) -> ([get_sreact id p env], env_t)
    | Srul_id_fun (id, args, p) -> eval_sreact_fun_app id args env env_t p in
  let aux' (acc, env_t) id =
    let (rs, env_t') = aux env_t id in
    (acc @ rs, env_t') in 
  match pr with
  | Spr (ids, _) -> let (rs, env_t') = List.fold_left aux' ([], env_t) ids in
		    (Sbrs.P_class rs, env_t')
  | Spr_red (ids, _) -> let (rs, env_t') = List.fold_left aux' ([], env_t) ids in
			(Sbrs.P_rclass rs, env_t')

let eval_sprs = eval_p_list eval_spr
			    
let eval_init exp env env_t =
  match exp with
  | Init (id, p) ->
     eval_big (Big_var (id, p)) ScopeMap.empty env env_t
  | Init_fun (id, args, p) ->
     eval_big (Big_var_fun (id, args, p)) ScopeMap.empty env env_t

(******** ADD TO STORE FUNCTIONS *********)
	      
let add_to_store fmt env id (v : typed_store_val) =
  (try
      let p = get_pos (Hashtbl.find env id) in
      report_warning fmt (Multiple_declaration (id, p, get_pos v));
    with
    | Not_found -> ());
  Hashtbl.replace env id v
		  
let update fmt id (v : store_val) p env env_t =
  let (t, env_t') = assign_type v env_t in
  add_to_store fmt env id (v, t, p);
  env_t'

let store_decs fmt decs env env_t =
  let aux env_t d =
    let upd id v p =
      update fmt id v p env env_t in
    match d with
    | Dctrl (Atomic (Ctrl_exp (id, ar, _), p)) ->
       upd id (A_ctrl (Base.Ctrl.Ctrl (id, ar))) p
    | Dctrl (Atomic (Ctrl_fun_exp (id, forms, ar, _), p)) ->
       (upd id (A_ctrl_fun (ar, forms)) p)       
    | Dctrl (Non_atomic (Ctrl_exp (id, ar, _), p)) ->
       upd id (Ctrl (Base.Ctrl.Ctrl (id, ar))) p
    | Dctrl (Non_atomic (Ctrl_fun_exp (id, forms, ar, _), p)) ->
       upd id (Ctrl_fun (ar, forms)) p
    | Dint d ->
       upd d.dint_id
	   (Int (eval_int d.dint_exp ScopeMap.empty env))
	   d.dint_loc
    | Dfloat d ->
       upd d.dfloat_id
	   (Float (eval_float d.dfloat_exp ScopeMap.empty env))
	   d.dfloat_loc
    | Dbig (Big_exp (id, exp, p)) ->
       (let (b_v, env_t') =
	  eval_big exp ScopeMap.empty env env_t in 
	update fmt id (Big b_v) p env env_t')
    | Dbig (Big_fun_exp (id, forms, exp, p)) ->
       upd id (Big_fun (exp, forms)) p
    | Dreact (React_exp (id, lhs, rhs, eta, p)) ->
       (let (r_v, env_t') =
	  eval_react lhs rhs eta ScopeMap.empty env env_t p in
	update fmt id (React r_v) p env env_t')
    | Dreact (React_fun_exp (id, forms, lhs, rhs, eta, p)) ->
       upd id (React_fun (lhs, rhs, eval_eta eta, forms)) p
    | Dsreact (Sreact_exp (id, lhs, rhs, eta, rate, p)) ->
       (let (r_v, env_t') =
	  eval_sreact lhs rhs eta rate ScopeMap.empty env env_t p in
	update fmt id (Sreact r_v) p env env_t')
    | Dsreact (Sreact_fun_exp (id, forms, lhs, rhs, eta, rate, p)) ->
       upd id (Sreact_fun (lhs, rhs, eval_eta eta, rate, forms)) p in
  List.fold_left aux env_t decs

let store_consts fmt consts (env : store) =
  let aux = function
    | Cint d ->
       let v = Int (eval_int d.dint_exp ScopeMap.empty env) in
       ignore (update fmt d.dint_id v d.dint_loc env [])
    | Cfloat d ->
       let v = Float (eval_float d.dfloat_exp ScopeMap.empty env) in
       ignore (update fmt d.dfloat_id v d.dfloat_loc env []) in
  List.iter aux consts

(* Store simple Ints or Floats when the list of values has only one element *)	    
let store_params fmt (params : param_exp list) env =
  let rec eval_int_range start incr stop acc =
    let start' = start + incr in
    if start' <= stop then eval_int_range start' incr stop (start' :: acc)
    else acc in
  let rec eval_float_range start incr stop acc =
    let start' = start +. incr in
    if start' <= stop then eval_float_range start' incr stop (start' :: acc)
    else acc in
  let flatten_int = function
    | [v] -> Int v
    | v -> Int_param v
  and flatten_float = function
    | [v] -> Float v
    | v -> Float_param v in
  let aux  = function
    | Param_int (id, Param_int_val (exp, _), p) ->
       let v = Int (eval_int exp ScopeMap.empty env) in
       add_to_store fmt env id (v, fst (assign_type v []), p)
    | Param_int (id, Param_int_range (start, incr, stop, _), p) ->
       (let s = eval_int start ScopeMap.empty env in
	let v = flatten_int
		  (eval_int_range s (eval_int incr ScopeMap.empty env)
				  (eval_int stop ScopeMap.empty env) [s]) in
	add_to_store fmt env id (v, fst (assign_type v []), p))
    | Param_int (id, Param_int_set (exps, _), p) ->
       let v =
	 List.map (fun e -> eval_int e ScopeMap.empty env) exps
	 |> List.sort_uniq (fun a b -> a - b)
 	 |> flatten_int in
       add_to_store fmt env id (v, fst (assign_type v []), p)
    | Param_float (id, Param_float_val (exp, _), p) ->
       let v = Float (eval_float exp ScopeMap.empty env) in
       add_to_store fmt env id (v, fst (assign_type v []), p)
    | Param_float (id, Param_float_range (start, incr, stop, _), p) ->
       let s = eval_float start ScopeMap.empty env in
       let v = flatten_float
		 (eval_float_range s (eval_float incr ScopeMap.empty env)
				   (eval_float stop ScopeMap.empty env)
				   [s]) in
       add_to_store fmt env id (v, fst (assign_type v []), p)
    | Param_float (id, Param_float_set (exps, _), p) ->
       let v =
	 List.map (fun e -> eval_float e ScopeMap.empty env) exps
	 |> List.sort_uniq compare
	 |> flatten_float in
       add_to_store fmt env id (v, fst (assign_type v []), p) in
  List.iter aux params

(******** INSTANTIATE REACTIVE SYSTEM *********)

let init_env fmt consts =
  let store = Hashtbl.create 1000 in
  store_consts fmt consts store;
  store
    
let eval_model fmt m env =
  let env_t = store_decs fmt m.model_decs env [] in  
  store_params fmt (params_of_ts m.model_rs) env;
  let (b, env_t') = eval_init (init_of_ts m.model_rs) env env_t in
  match m.model_rs with
  | Dbrs rbs -> let (p, env_t'') = eval_prs env env_t' rbs.dbrs_pri in
		(b, P p, env_t'')
  | Dsbrs sbrs -> let (p, env_t'') = eval_sprs env env_t' sbrs.dsbrs_pri in
		  (b, S p, env_t'')  
		    
let export decs (env : store) (env_t : store_t) path verb = 
  let svg = ".svg" in
  let write_pair id lhs rhs =
    Export.write_big lhs (id ^ "_lhs" ^ svg) path verb;
    Export.write_big rhs (id ^ "_rhs" ^ svg) path verb in
  let dummy_args (args_t : num_type list) =
    resolve_types env_t args_t
    |> List.map def_val  in
  let aux id = 
    let args_t = dom_of_lambda (get_type (Hashtbl.find env id)) in
    dummy_args args_t  in
  let aux' eval_f id args p =
    eval_f id args env env_t p |> fst |> List.hd in
  List.iter (fun d ->
	     match d with
	     | Dctrl _
	     | Dint _
	     | Dfloat _ -> ()
	     | Dbig (Big_exp (id, _, p)) ->
		Export.write_big (get_big id p env) (id ^ svg) path verb
	     | Dbig (Big_fun_exp (id, _, _, p)) ->
		(let args = aux id in
		 let b = fst (eval_big (Big_var_fun (id, args, p))
				       ScopeMap.empty env env_t) in
		 Export.write_big b (id ^ svg) path verb)
	     | Dreact (React_exp (id, _, _, _, p)) ->
		(let r = get_react id p env in
		 write_pair id r.Brs.rdx r.Brs.rct)
	     | Dreact (React_fun_exp (id, _, _, _, _, p)) ->
		(let args = aux id in
		 let r = aux' eval_react_fun_app id args p in
		 write_pair id r.Brs.rdx r.Brs.rct)
	     | Dsreact (Sreact_exp (id, _, _, _, _, p)) ->
		(let r = get_sreact id p env in
		 write_pair id r.Sbrs.rdx r.Sbrs.rct)
	     | Dsreact (Sreact_fun_exp (id, _, _, _, _, _, p)) ->
		(let args = aux id in
		 let r = aux' eval_sreact_fun_app id args p in
		 write_pair id r.Sbrs.rdx r.Sbrs.rct)
	    ) decs
