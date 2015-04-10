open Store
       
(******** EVAL FUNCTIONS *********)
		       
let div_int l r p =
  match r with
  | 0 -> raise (ERROR (Division_by_zero, p))
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
  | Float_div (l, r, p) -> (eval_float l scope env) /. (eval_float r scope env)
  | Float_pow (l, r, p) -> (eval_float l scope env) ** (eval_float r scope env)

let rec eval_num (exp : num_exp) (scope : scope) (env : store) =
  let aux val0 val1 fun_int fun_float p =
    match (val0, val1) with
    | (Int v, Int v') -> Int (fun_int v v')
    | (Float v, Float v') -> Float (fun_float v v')
    | (Int _, Float _) ->
       raise (ERROR (Wrong_type (`num_val (`b `float), `num_val (`b `int)), p))
    | (Float _, Int _) ->
       raise (ERROR (Wrong_type (`num_val (`b `int), `num_val (`b `float)), p))
    | (_, _) -> assert false in
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
	    | _ -> assert false
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
      let (a, forms, t) =
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
  | Big_par (l, r, p) -> 
     binary_eval l r scope env env_t Big.par
  | Big_ppar (l, r, p) -> 
     binary_eval l r scope env env_t Big.ppar
  | Big_share (a, psi, b, p) -> 
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
  | Big_nest (ion, b, p) -> (* No atomic controls allowed here *)
     (let (i_v, env_t') = eval_ion scope env env_t false ion in
      let (b_v, env_t'') = eval_big b scope env env_t' in
      (Big.nest i_v b_v, env_t''))
  | Big_ion ion -> eval_ion scope env env_t true ion
  | Big_close exp ->
     (Big.closure (Link.parse_face [exp.cl_name]), env_t)
  | Big_closures (c, b, p) ->
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
  else raise (ERROR (React "Invalid reaction", p))

let eval_sreact lhs rhs eta rate scope env env_t p =
  let (lhs_v, rhs_v, env_t') =
    eval_react_aux lhs rhs scope env env_t in 
  let r = { Sbrs.rdx = lhs_v;
	    Sbrs.rct = rhs_v;
	    Sbrs.rate = eval_float rate scope env;
	  } in
  (* Get more informative messages from Sbrs *)
  if Sbrs.is_valid_sreact r then (r, env_t') 
  else raise (ERROR (React "Invalid stochastic reaction", p))

let eval_pr store env = function
  | Pr (x, _) -> failwith "TODO"
  | Pr_red (x, _) -> failwith "TODO"

let eval_spr store env = function
  | Spr (x, _) -> failwith "TODO"
  | Spr_red (x, _) -> failwith "TODO"
    
let eval_init exp env =
  match exp with
  | Init (id, p) ->
     eval_big (Big_var (id, p)) ScopeMap.empty env env_t
  | Init_fun (id, acts, p) ->
     eval_big (Big_var_fun (id, acts, p)) env
