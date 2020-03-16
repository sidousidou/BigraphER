open Syntax
open Lexing
open Str
open Printf

exception INVALID_CONSTS
exception INVALID_VAL
exception INVALID_PRI
exception WRONG_TYPE
exception NO_IDE

type form = Ctrl.ide

type store_val =
  | Int of int
  | Float of float
  | Float_fun of num_exp * form list
  | Big of Big.bg
  | Big_fun of Big.bg * form list
  | Ctrl of Ctrl.t
  | Ctrl_fun of Ctrl.t * form list
  | A_ctrl of Ctrl.t
  | A_ctrl_fun of Ctrl.t * form list
  | React of Brs.react
  | React_fun of Big.bg * Big.bg * form list
  | Sreact of Sbrs.sreact
  | Sreact_fun of Big.bg * Big.bg * num_exp * form list
  | Int_param of int list
  | Float_param of float list

(* type p_class_ide =  *)
(*   | P_class_ide of string list  (\** Priority class *\) *)
(*   | P_rclass_ide of string list (\** Reducable priority class *\) *)

(* let p_to_brs c = *)
(*   match c with *)
(*   | P_class_ide x -> Brs.P_class_ide x  *)
(*   | P_rclass_ide x -> Brs.P_rclass_ide x *)

(* let p_to_sbrs c = *)
(*   match c with *)
(*   | P_class_ide x -> Sbrs.P_class_ide x  *)
(*   | P_rclass_ide x -> Sbrs.P_rclass_ide x *)

let init_env decs =
  Hashtbl.create (List.length decs)

let get_type v = 
  match v with
  | Int _ -> "int"
  | Float _ -> "float"
  | Float_fun _ -> "float"
  | Big _ -> "big"
  | Big_fun _ -> "big"
  | Ctrl _ -> "ctrl"
  | Ctrl_fun _ -> "ctrl"
  | A_ctrl _ -> "ctrl"
  | A_ctrl_fun _ -> "ctrl"
  | React _ -> "react"
  | React_fun _ -> "react"
  | Sreact _ -> "sreact"
  | Sreact_fun _ -> "sreact"
  | Int_param _ -> "int param"
  | Float_param _ -> "float param"

(* let _is_int f =  *)
(*   f -. (float (int_of_float f)) = 0.0  *)

let print_err msg p =
  print_pos p;
  prerr_endline msg

let type_err_exn v f ?(verb = true) p =
  if verb then (
    print_pos p;
    eprintf "Error: This expression has type %s\n\t\
             but an expression was expected of type %s\n!" (get_type v) f;
  );
  raise WRONG_TYPE

let unbound_err_exn ide ?(verb = true) p =
  if verb then (
    print_pos p;
    eprintf "Error: Unbound value %s\n!" ide;
  );
  raise NO_IDE

let args_err_exn ide forms acts ?(verb = true) p =
  if verb then (
    print_pos p;
    eprintf "Error: %s expects %d argument(s)\n\t\
             but is here used with %d argument(s)\n!" ide forms acts;
  );
  raise WRONG_TYPE

let pri_err_exn ?(verb = true) p =
   if verb then (
     print_pos p;
     prerr_endline "Error: Invalid priority classes";
   );
  raise INVALID_PRI

(* -consts a=2,b=3.4,c=inf*)
let parse_consts_exn pairs env =
  List.iter (fun (ide, v) -> 
      try
	Hashtbl.add env ide (Int (int_of_string v))
      with
      | Failure _ -> (
	  try
            Hashtbl.add env ide (Float (float_of_string v))
          with
          | Failure _ -> prerr_endline ("Error: invalid constant \"" ^ ide ^ "\""); 
            raise INVALID_CONSTS
        )
    ) pairs

let get_int_exn ide p ?(verb = true) env =
  let v = (try
             Hashtbl.find env ide
           with
           | Not_found -> unbound_err_exn ide ~verb:verb p
          ) in
  match v with
  | Int v -> v
  | Float _ | Float_fun _ | Big _ 
  | Big_fun _ | A_ctrl _ | A_ctrl_fun _ | React _ 
  | React_fun _ | Sreact _ | Sreact_fun _ | Int_param _ 
  | Float_param _ | Ctrl _ 
  | Ctrl_fun _ as v -> type_err_exn v "int" ~verb:verb p

let get_float_exn ide p ?(verb = true) env =
  let v = (try
             Hashtbl.find env ide
           with
           | Not_found -> unbound_err_exn ide ~verb:verb p
          ) in
  match v with
  | Float v -> v
  | Int _ | Float_fun _ | Big _ | Big_fun _ | A_ctrl _ | A_ctrl_fun _ 
  | React _ | React_fun _ | Sreact _ | Sreact_fun _ | Int_param _ 
  | Float_param _ | Ctrl _ 
  | Ctrl_fun _ as v -> type_err_exn v "float" ~verb:verb p

let get_ctrl_exn ide p ?(verb = true) env =
  let v = (try
             Hashtbl.find env ide
           with
           | Not_found -> unbound_err_exn ide ~verb:verb p
          ) in
  match v with
  | A_ctrl c | Ctrl c -> c
  | Int _ | Float_fun _ | Big _ 
  | Big_fun _ | Float _ | A_ctrl_fun _ | React _ 
  | React_fun _ | Sreact _ | Sreact_fun _ | Int_param _ 
  | Float_param _ | Ctrl_fun _ as v -> type_err_exn v "ctrl" ~verb:verb p

let is_atomic_exn ide p ?(verb = true) env =
  let v = (try
             Hashtbl.find env ide
           with
           | Not_found -> unbound_err_exn ide ~verb:verb p
          ) in
  match v with
  | A_ctrl _ |  A_ctrl_fun _ -> true
  | Int _ | Float_fun _ | Big _ 
  | Big_fun _ | Float _ | Ctrl _ | React _ 
  | React_fun _ | Sreact _ | Sreact_fun _ | Int_param _ 
  | Float_param _ | Ctrl_fun _ -> false

let get_big_exn ide p ?(verb = true) env =
  let v = (try
             Hashtbl.find env ide
           with
           | Not_found -> unbound_err_exn ide ~verb:verb p
          ) in
  match v with
  | Big v -> v
  | Int _ | Float_fun _ | A_ctrl _ 
  | Big_fun _ | Float _ | A_ctrl_fun _ | React _ 
  | React_fun _ | Sreact _ | Sreact_fun _ | Int_param _ 
  | Float_param _ | Ctrl _ 
  | Ctrl_fun _ as v -> type_err_exn v "big" ~verb:verb p
 
let rec eval_int_exn exp ?(verb = true) env =
  match exp with
  | Num_val (v, _) -> int_of_float v
  | Num_ide (ide, p) -> 
    (try get_int_m ide p env
     with
     | WRONG_TYPE -> 
       let v = get_float ide p env in
       if _is_int v then int_of_float v
       else type_err (Float 0.0) "int" p) 
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
    | A_ctrl_fun (ar, forms) | Ctrl_fun (ar, forms) -> 
      (let f_l = List.length forms
       and a_l = List.length acts in
       if f_l <> a_l then args_err ide f_l a_l p
       else let acts_s = String.concat "," (List.map (fun exp ->
	   try
	     sprintf "%d" (eval_int_m exp env) (* MUTE *)
	   with
	   | WRONG_TYPE -> sprintf "%g" (eval_float exp env)) acts) in
	 Ctrl.Ctrl (sprintf "%s(%s)" ide acts_s, ar))
    | Int _ | Int_fun _ | Float_fun _ | Big _ 
    | Big_fun _ | Float _ | A_ctrl _ | React _ 
    | React_fun _ | Sreact _ | Sreact_fun _ | Int_param _ 
    | Float_param _  | Ctrl _ as v -> type_err v "ctrl" p
  with
  | Not_found -> unbound_err ide p

let scope env forms acts =
  let h = Hashtbl.copy env in
  List.iter (fun (ide, exp) ->
      let v = eval_float exp env in (* precision? *)
      if _is_int v then Hashtbl.add h ide (Int (int_of_float v))
      else Hashtbl.add h ide (Float v)) (List.combine forms acts);
  h

let eval_ion_aux f c names p =
  try 
    f (Link.parse_face names) c
  with
  | Big.CTRL_ERROR (n, _) -> 
    (print_err (sprintf "Error: ctrl %s has arity %d\n\t\
                         but is here used with %d name(s)"
		  (Ctrl.name c) n (List.length names)) p; 
     raise INVALID_VAL)
 
let eval_ion = eval_ion_aux Big.ion

let eval_atom = eval_ion_aux Big.atom

let rec eval_big exp env =
  let get_big_fun ide acts p env =
    try (
      match Hashtbl.find env ide with
      | Big_fun (exp, forms) -> 
	(try eval_big exp (scope env forms acts)
	 with
	 | Invalid_argument _ -> 
	   args_err ide (List.length forms) (List.length acts) p) 
      | Int _ | Int_fun _ | Float_fun _ | Big _ 
      | A_ctrl_fun _ | Float _ | A_ctrl _ | React _ 
      | React_fun _ | Sreact _ | Sreact_fun _ | Int_param _ 
      | Float_param _  | Ctrl _ | Ctrl_fun _ as v -> type_err v "big" p)
    with
    | Not_found -> unbound_err ide p in
  match exp with
  | Big_ide (ide, p) -> get_big ide p env
  | Big_ide_fun(ide, acts, p) -> get_big_fun ide acts p env
  | Big_plac (l, roots, p) -> 
    (try Big.placing l roots Link.Face.empty
     with
     | _ -> 
       print_err ("Error: Invalid placing expression") p; 
       raise INVALID_VAL)
  | Big_name (n, _) -> 
    Big.sub (Link.Face.empty) (Link.Face.singleton (Link.Nam n))
  | Big_comp (l, r, p) -> 
    (try Big.comp (eval_big l env) (eval_big r env)
     with
     | _ -> 
       print_err ("Error: Invalid composition expression") p; 
       raise INVALID_VAL)
  | Big_comp_c (closures, b, p) -> 
    (match closures with
    | Big_close (names, _) -> 
      (try Big.close (Link.parse_face names) (eval_big b env)
       with
       | _ -> 
	 print_err ("Error: Invalid composition expression") p; 
	 raise INVALID_VAL)
    | Big_ide _ | Big_ide_fun _ | Big_plac _ | Big_comp_c _ | Big_comp _
    | Big_par _ | Big_ppar _ | Big_nest _ | Big_el _ | Big_id _ | Big_ion _
    | Big_ion_fun _ | Big_share _ | Big_tens _ | Big_name _ -> 
      print_err ("Error: Invalid composition expression") p; 
      raise INVALID_VAL)			
  | Big_close (names, p) -> 
    (try Big.ppar_of_list (List.map (fun n ->
      Big.closure (Link.Face.singleton (Link.Nam n))) names)
     with
     | _ -> 
       print_err ("Error: Invalid closure") p; 
       raise INVALID_VAL)
  | Big_tens (l, r, p) -> 
    (try Big.tens (eval_big l env) (eval_big r env)
     with
     | _ -> 
       print_err ("Error: Invalid tensor product expression") p; 
       raise INVALID_VAL)
  | Big_par (l, r, p) -> 
    (try Big.par (eval_big l env) (eval_big r env)
     with
     | _ -> 
       print_err ("Error: Invalid merge product expression") p; 
       raise INVALID_VAL)
  | Big_ppar (l, r, p) -> 
    (try Big.ppar (eval_big l env) (eval_big r env)
     with
     | _ -> 
       print_err ("Error: Invalid parallel product expression") p; 
       raise INVALID_VAL)
  | Big_nest (l, r, p) -> 
    (try match l with
    | Big_ion _ -> Big.nest (eval_big l env) (eval_big r env)
    | Big_ion_fun _ -> Big.nest (eval_big l env) (eval_big r env)
    | Big_ide _ | Big_ide_fun _ | Big_plac _ | Big_comp_c _ | Big_comp _ 
    | Big_close _ | Big_par _ | Big_ppar _ | Big_nest _ | Big_el _ | Big_id _ 
    | Big_share _ | Big_tens _ | Big_name _ ->
      print_err ("Error: Invalid nesting expression: Left-hand side is not an ion") p; 
      raise INVALID_VAL
     with
     | _ -> 
       print_err ("Error: Invalid nesting expression") p; 
       raise INVALID_VAL)
  | Big_el (v, p) -> 
    (match v with
    | 0 -> Big.zero
    | 1 -> Big.one
    | _ -> 
      print_err (sprintf "Error: Expression %d is not a valid bigraph" v) p; 
      raise INVALID_VAL)
  | Big_id (n, names, _) -> Big.id (Big.Inter (n, (Link.parse_face names)))
  | Big_ion (c, names, p) -> (
      if is_atomic c p env then
        eval_atom (get_ctrl c p env) names p
      else eval_ion (get_ctrl c p env) names p) 
  | Big_ion_fun (c, names, acts, p) -> (
      if is_atomic c p env then
        eval_atom (get_ctrl_fun c acts p env) names p
      else eval_ion (get_ctrl_fun c acts p env) names p)
  | Big_share (a, psi, b, p) -> 
    (try Big.share (eval_big a env) (eval_big psi env) (eval_big b env)
     with
     | _ -> 
       print_err ("Error: Invalid sharing expression") p; 
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

let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)

let store_decs decs env =
  let _dummy_acts =
    List.map (fun _ -> Num_val (0.0, dummy_pos)) in
  List.iter (fun d ->
      match d with
      | Atomic c -> (
          match c with
          | Ctrl_dec (ide, ar, _) ->
            Hashtbl.add env ide (A_ctrl (Ctrl.Ctrl (ide, ar)))
          | Ctrl_dec_f (ide, forms, ar, _) ->
            Hashtbl.add env ide (A_ctrl_fun (ar, forms)))
      | Non_atomic c -> (
          match c with
          | Ctrl_dec (ide, ar, _) ->
            Hashtbl.add env ide (Ctrl (Base.Ctrl.Ctrl (ide, ar)))
          | Ctrl_dec_f (ide, forms, ar, _) ->
            Hashtbl.add env ide (Ctrl_fun (ar, forms)))
      | Int_dec (ide, exp, _) ->
        Hashtbl.add env ide (Int (eval_int exp env))
      | Float_dec (ide, exp, _) ->
        Hashtbl.add env ide (Float (eval_float exp env))
      | Big_dec (ide, exp, _) ->
        Hashtbl.add env ide (Big (eval_big exp env))
      | Big_dec_f (ide, forms, exp, _) ->
        ignore (eval_big exp (scope env forms (_dummy_acts forms)));
        Hashtbl.add env ide (Big_fun (exp, forms))
      | React_dec (ide, lhs, rhs, p) ->
        Hashtbl.add env ide (React (eval_react lhs rhs env p))
      | React_dec_f (ide, forms, lhs, rhs, p) ->
        ignore (eval_react lhs rhs (scope env forms (_dummy_acts forms)) p);
        Hashtbl.add env ide (React_fun (lhs, rhs, forms))
      | Sreact_dec (ide, lhs, rhs, exp, p) ->
        Hashtbl.add env ide (Sreact (eval_sreact lhs rhs exp env p))
      | Sreact_dec_f (ide, forms, lhs, rhs, exp, p) ->
        ignore (eval_sreact lhs rhs exp (scope env forms (_dummy_acts forms)) p);
        Hashtbl.add env ide (Sreact_fun (lhs, rhs, exp, forms))) decs

let get_react p env ide =
  try 
    match Hashtbl.find env ide with
    | React r -> r
    | Int _ | Int_fun _ | Float_fun _ | Big _ 
    | Big_fun _ | Float _ | Ctrl _ | Ctrl_fun _ | A_ctrl _ | A_ctrl_fun _ 
    | React_fun _ | Sreact _ | Sreact_fun _ | Int_param _ 
    | Float_param _ as v -> type_err v "react" p
  with
  | Not_found -> unbound_err ide p

let _scope env forms acts =
    let h = Hashtbl.copy env in
    List.iter (fun (ide, v) ->
      Hashtbl.add h ide v) (List.combine forms acts);
    h 
  
let get_react_fun acts p env ide =
  try 
    match Hashtbl.find env ide with
    | React_fun (lhs, rhs, forms) ->
      eval_react lhs rhs (_scope env forms acts) p
    | React _ -> 
      args_err ide 0 (List.length acts) p  
    | Int _ | Int_fun _ | Float_fun _ | Big _ 
    | Big_fun _ | Float _ | Ctrl _ | Ctrl_fun _ 
    | Sreact _ | Sreact_fun _ | Int_param _ | A_ctrl _ | A_ctrl_fun _
    | Float_param _ as v -> type_err v "react" p
  with
  | Not_found -> unbound_err ide p

let get_sreact p env ide =
  try 
    match Hashtbl.find env ide with
    | Sreact r -> r
    | Int _ | Int_fun _ | Float_fun _ | Big _ 
    | Big_fun _ | Float _ | Ctrl _ | Ctrl_fun _ 
    | React_fun _ | React _ | Sreact_fun _ | Int_param _ 
    | Float_param _ | A_ctrl _ 
    | A_ctrl_fun _ as v -> type_err v "sreact" p
  with
  | Not_found -> unbound_err ide p

let get_sreact_fun acts p env ide =
  try 
    match Hashtbl.find env ide with
    | Sreact_fun (lhs, rhs, exp, forms) ->
      eval_sreact lhs rhs exp (_scope env forms acts) p
    | Sreact _ -> 
      args_err ide 0 (List.length acts) p  
    | Int _ | Int_fun _ | Float_fun _ | Big _ 
    | Big_fun _ | Float _ | Ctrl _ | Ctrl_fun _ 
    | React _ | React_fun _ | Int_param _ 
    | Float_param _ | A_ctrl _ 
    | A_ctrl_fun _ as v -> type_err v "react" p
  with
  | Not_found -> unbound_err ide p

let store_params env =
  let store_param p env =
    match p with
    | Syntax.Int_param (ide, [exp], _) -> 
      Hashtbl.add env ide (Int_param [eval_int exp env])
    | Syntax.Int_param (ide, exps, _) -> 
      Hashtbl.add env ide (Int_param (List.map (fun exp ->
	eval_int exp env) exps))
    | Syntax.Float_param (ide, [exp], _) -> 
      Hashtbl.add env ide (Float_param [eval_float exp env])
    | Syntax.Float_param (ide, exps, _) -> 
      Hashtbl.add env ide (Float_param (List.map (fun exp ->
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
    | Int_param ints -> 
      List.map (fun x -> Int x) ints 
    | Float_param	 floats -> 
      List.map (fun x -> Float x) floats
    | Int _ | Int_fun _ | Float_fun _ | Big _ 
    | Big_fun _ | Float _ | Ctrl _ | Ctrl_fun _ 
    | React _ | React_fun _ | Sreact _ 
    | Sreact_fun _ | A_ctrl _ | A_ctrl_fun _ ->
      raise WRONG_TYPE
  with
  | Not_found -> unbound_err ide p

(*ide -> params -> ide(params)*)
let _build_react_ide ide params =
  sprintf "%s(%s)" ide (String.concat "," (List.map (fun p ->
  match p with
  | Int x -> sprintf "%d" x 
  | Float x -> sprintf "%g" x
  | Int_param _ | Int_fun _ | Float_fun _ | Big _ 
  | Big_fun _ | Float_param _ | Ctrl _ | Ctrl_fun _ | A_ctrl _ | A_ctrl_fun _ 
  | React _ | React_fun _ | Sreact _ | Sreact_fun _ ->
    raise WRONG_TYPE) params)) 

let rec par_comb pars = 
  match pars with
  | [x] -> List.map (fun v -> [v]) x
  | x :: xs -> ( 
      let aux1 v ls =
	List.map (fun l -> v :: l) ls
      in let rec aux2 l ls = 
	match l with
	| [] -> []
	| x :: xs -> (aux1 x ls) @ (aux2 xs ls)
      in aux2 x (par_comb xs) 
    )
  | [] -> []

let _build_params ides env p =
  par_comb (List.map (fun ide -> 
      try 
        get_param_m ide p env
      with
      | WRONG_TYPE -> 
        try
	  [Int (get_int_m ide p env)]
        with
        | WRONG_TYPE -> [Float (get_float ide p env)]) ides)

let store_rule r env =
  match r with
  | Rul_fun (ide, ides, p) -> begin
    (* get param values *)
    let params =_build_params ides env p in
    (* get react_fun *)
    let reacts_store =
      List.map (fun acts ->
	_build_react_ide ide acts, get_react_fun acts p env ide) params in
    (* Update store  - check performances *)
    List.iter (fun (ide, v) -> Hashtbl.add env ide (React v)) reacts_store;
      fst (List.split reacts_store)
     end
  | Rul (ide, p) -> ignore (get_react p env ide); [ide]

let store_srule r env =
 match r with
  | Rul_fun (ide, ides, p) -> begin
    (* get param values *)
    let params = _build_params ides env p in
    (* get react_fun *)
    let reacts_store =
      List.map (fun acts ->
	_build_react_ide ide acts, get_sreact_fun acts p env ide) params in
    (* Update store - check performances *)
    List.iter (fun (ide, v) -> Hashtbl.add env ide (Sreact v)) reacts_store;
    fst (List.split reacts_store)
  end
  | Rul (ide, p) -> ignore (get_sreact p env ide); [ide]

let store_pclass (c : pri_class) env =
  let _aux c p =
    if Brs.is_valid_p_ide (get_react p env) (p_to_brs c) then c
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

let store_spclass c env =
  let _aux c p =
    if Sbrs.is_valid_p_ide (get_sreact p env) (p_to_sbrs c) then c
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
    if Brs.is_valid_p_ide_l (List.map p_to_brs p_ide_list)
    then (eval_init init env, false, p_ide_list)
    else pri_err p
  end
  | Sbrs (params, init, p_classes, p) -> begin
    store_params env params;
    let p_ide_list = List.fold_left (fun acc c -> 
      acc @ [store_spclass c env]) [] p_classes in
    if Sbrs.is_valid_p_ide_l (List.map p_to_sbrs p_ide_list)
    then (eval_init init env, true, p_ide_list)
    else pri_err p
  end

let export decs env path verb = 
  let _dummy_acts =
    List.map (fun _ -> Num_val (0.0, dummy_pos))
  and svg = ".svg" in 
  List.iter (fun d ->
    match d with
  | Atomic _ | Non_atomic _ | Int_dec _ | Float_dec _ -> ()
  | Big_dec (ide, exp, _) -> 
    Export.write_big (eval_big exp env) (ide ^ svg) path verb
  | Big_dec_f (ide, forms, exp, _) ->
    let b = eval_big exp (scope env forms (_dummy_acts forms)) in
    Export.write_big b (ide ^ svg) path verb
  | React_dec (ide, lhs, rhs, _) ->
    Export.write_big (eval_big lhs env) (ide ^ "_lhs" ^ svg) path verb;
    Export.write_big (eval_big rhs env) (ide ^ "_rhs" ^ svg) path verb
  | React_dec_f (ide, forms, lhs, rhs, _) ->
    let l = eval_big lhs (scope env forms (_dummy_acts forms)) 
    and r = eval_big rhs (scope env forms (_dummy_acts forms)) in
    Export.write_big l (ide ^ "_lhs" ^ svg) path verb;
    Export.write_big r (ide ^ "_rhs" ^ svg) path verb
  | Sreact_dec (ide, lhs, rhs, _, _) ->
    Export.write_big (eval_big lhs env) (ide ^ "_lhs" ^ svg) path verb;
    Export.write_big (eval_big rhs env) (ide ^ "_rhs" ^ svg) path verb
  | Sreact_dec_f (ide, forms, lhs, rhs, _, _) ->
    let l = eval_big lhs (scope env forms (_dummy_acts forms)) 
    and r = eval_big rhs (scope env forms (_dummy_acts forms)) in
    Export.write_big l (ide ^ "_lhs" ^ svg) path verb;
    Export.write_big r (ide ^ "_rhs" ^ svg) path verb) decs
  

  


