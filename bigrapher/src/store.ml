open Format
open Ast
open Unify
open Bigraph

module Make
    (T : Rs.RS with type ac := AppCond.t) (P : sig
      val parse_react :
        string ->
        Big.t ->
        Big.t ->
        ?conds:AppCond.t list ->
        [ `E of unit | `F of float | `P of string * int * float ] ->
        Fun.t option ->
        T.react option
    end) =
struct
  type store_type =
    [ `core_val of core_type
    | `big_val of fun_type
    | `lambda of lambda
    | `param of base_type ]

  let string_of_store_t = function
    | `core_val t -> string_of_core_t t
    | `big_val t -> string_of_fun_t t
    | `lambda t -> string_of_lambda t
    | `param `int -> "int param"
    | `param `float -> "float param"
    | `param `string -> "string param"

  let dom_of_lambda = function
    | `core_val _ | `big_val _ | `param _ -> assert false
    | `lambda t -> fst t

  let resolve_t (env : store_t) = function
    | `core_val t -> `core_val (resolve_type env t)
    | `lambda (t, t') -> `lambda (resolve_types env t, t')
    | t -> t

  type store_val =
    | Int of int
    | Float of float
    | Str of string
    | Big of Big.t
    | Big_fun of big_exp * Id.t list
    | Ctrl of Ctrl.t
    | Ctrl_fun of int * Id.t list
    | A_ctrl of Ctrl.t
    | A_ctrl_fun of int * Id.t list
    | React of T.react
    | React_fun of
        Id.t
        * int
        * big_exp
        * big_exp
        * Fun.t option
        * cond_exp list
        * exp option
        * Id.t list
    | Int_param of int list
    | Float_param of float list
    | Str_param of string list

  let string_of_store_val = function
    | Int x -> string_of_int x
    | Float x -> string_of_float x
    | Str s -> s
    | Big x -> Big.to_string x
    | Big_fun _ -> "<fun big>"
    | A_ctrl c | Ctrl c -> Ctrl.to_string c
    | A_ctrl_fun _ | Ctrl_fun _ -> "<fun ctrl>"
    | React r -> T.string_of_react r
    | React_fun _ -> "<fun react>"
    | Int_param p -> "(" ^ String.concat "," (List.map string_of_int p) ^ ")"
    | Float_param p ->
        "(" ^ String.concat "," (List.map string_of_float p) ^ ")"
    | Str_param p -> "(" ^ String.concat "," p ^ ")"

  let def_val = function
    | `g _ -> ENum (Num_int_val (0, Loc.dummy_loc))
    | `b `int -> ENum (Num_int_val (0, Loc.dummy_loc))
    | `b `float -> ENum (Num_float_val (0.0, Loc.dummy_loc))
    | `b `string -> EStr (Str_val ("", Loc.dummy_loc))

  type typed_store_val = store_val * store_type * Loc.t

  let assign_type (v : store_val) env_t =
    let assign_type_forms = List.map (fun _ -> next_gen ()) in
    let update forms t =
      let fresh_t = assign_type_forms forms in
      (`lambda (box_gen_types fresh_t, t), add_types fresh_t env_t)
    in
    match v with
    | Int _ -> (`core_val (`b `int), env_t)
    | Float _ -> (`core_val (`b `float), env_t)
    | Str _ -> (`core_val (`b `string), env_t)
    | Big _ -> (`big_val `big, env_t)
    | Big_fun (_, forms) -> update forms `big
    | Ctrl c | A_ctrl c -> (`big_val (`ctrl (Ctrl.arity c)), env_t)
    | Ctrl_fun (arity, forms) | A_ctrl_fun (arity, forms) ->
        update forms (`ctrl arity)
    | React _ -> (`big_val `react, env_t)
    | React_fun (_, _, _, _, _, _, _, forms) -> update forms `react
    | Int_param _ -> (`param `int, env_t)
    | Float_param _ -> (`param `float, env_t)
    | Str_param _ -> (`param `string, env_t)

  type store = typed_store_val Base.H_string.t

  let get_val ((v, _, _) : typed_store_val) = v

  let get_type ((_, t, _) : typed_store_val) = t

  let get_pos ((_, _, p) : typed_store_val) = p

  let bindings = Base.H_string.length

  type p_class_list = T.p_class list

  type error =
    | Wrong_type of store_type * store_type (* (current, expected) *)
    | Atomic_ctrl of Id.t
    | Arity of string * int * int (* (id, current, expected) *)
    | Unbound_variable of Id.t
    | Div_by_zero
    | Comp of Big.inter * Big.inter
    | Invalid_class
    | Invalid_priorities
    | Tens of Link.Face.t * Link.Face.t (* (in , out) *)
    | Share
    | Unknown_big of int
    | Reaction of string (* error message *)
    | Init_not_ground
    | Not_face of Id.t list
    | Invalid_iter_op of int

  type warning = Multiple_declaration of Id.t * Loc.t * Loc.t

  exception ERROR of error * Loc.t

  let report_error_aux fmt = function
    | Wrong_type (curr, exp) ->
        fprintf fmt
          "This expression has type %s but an expression was expected of \
           type %s"
          (string_of_store_t curr) (string_of_store_t exp)
    | Atomic_ctrl id ->
        fprintf fmt
          "Control %s is atomic but it is here used in a nesting expression"
          id
    | Arity (id, ar, ar_dec) ->
        fprintf fmt
          "Control %s has arity %d but a control of arity %d was expected" id
          ar ar_dec
    | Unbound_variable s -> fprintf fmt "Unbound variable %s" s
    | Div_by_zero -> fprintf fmt "Division by zero"
    | Comp (i, j) ->
        fprintf fmt "Interfaces %s and %s do not match in the composition"
          (Big.string_of_inter i) (Big.string_of_inter j)
    | Tens (inner, outer) ->
        fprintf fmt
          "Tensor product has common inner names %s and common outer names \
           %s"
          (Link.string_of_face inner)
          (Link.string_of_face outer)
    | Share -> fprintf fmt "Invalid sharing expression"
    | Unknown_big v -> fprintf fmt "Expression %d is not a valid bigraph" v
    | Reaction msg -> fprintf fmt "%s" msg
    | Invalid_class -> fprintf fmt "Invalid epression for a priority class"
    | Invalid_priorities ->
        fprintf fmt "Invalid expression for a priority structure"
    | Init_not_ground -> fprintf fmt "init bigraph is not ground"
    | Not_face ns ->
        fprintf fmt
          "Expression {%s} is not a valid interface as it contains \
           duplicate names"
          (String.concat "," ns)
    | Invalid_iter_op n ->
        fprintf fmt
          "Argument of iterated operator evaluates to %d but a value >= 0 \
           was expected"
          n

  let report_error fmt c err =
    fprintf fmt "@[%s: %a@]@," (Utils.err_opt c) report_error_aux err

  let report_warning fmt c = function
    | Multiple_declaration (id, p, p') ->
        fprintf fmt "%a@[%s: Identifier %s was already used at %s@]@,"
          Loc.print_loc p' (Utils.warn_opt c) id (Loc.string_of_pos p)

  (******** SCOPE *********)

  module ScopeMap = Map.Make (struct
    type t = Id.t

    let compare = Id.compare
  end)

  type scope = typed_store_val ScopeMap.t

  (******** GET FUNCTIONS *********)

  let fetch id p (aux : typed_store_val -> 'a) (scope : scope) (env : store)
      =
    try aux (ScopeMap.find id scope)
    with Not_found -> (
      match Base.H_string.find env id with
      | Some v -> aux v
      | None -> raise (ERROR (Unbound_variable id, p)) )

  let store_val_type = function
    | Int _, t, _
    | Float _, t, _
    | Str _, t, _
    | Big _, t, _
    | Big_fun (_, _), t, _
    | Ctrl _, t, _
    | Ctrl_fun (_, _), t, _
    | A_ctrl _, t, _
    | A_ctrl_fun (_, _), t, _
    | React _, t, _
    | React_fun (_, _, _, _, _, _, _, _), t, _
    | Int_param _, t, _
    | Str_param _, t, _
    | Float_param _, t, _ ->
        t

  let get_int id p scope env =
    let aux = function
      | Int v, _, _ -> v
      | x ->
          raise
            (ERROR (Wrong_type (store_val_type x, `core_val (`b `int)), p))
    in
    fetch id p aux scope env

  let get_float id p scope env =
    let aux = function
      | Float v, _, _ -> v
      | x ->
          raise
            (ERROR (Wrong_type (store_val_type x, `core_val (`b `float)), p))
    in
    fetch id p aux scope env

  let get_var id p scope env =
    let aux = function
      | (Float _ as v), _, _ | (Int _ as v), _, _ | (Str _ as v), _, _ -> v
      | x ->
          raise
            (ERROR
               ( Wrong_type (store_val_type x, `core_val (`g core_type_str)),
                 p ))
    in
    fetch id p aux scope env

  let get_num id p scope env =
    let aux = function
      | (Float _ as v), _, _ | (Int _ as v), _, _ -> v
      | x ->
          raise
            (ERROR
               (Wrong_type (store_val_type x, `core_val (`g int_or_float)), p))
    in
    fetch id p aux scope env

  let get_str id p scope env =
    let aux = function
      | Str s, _, _ -> s
      | x ->
          raise
            (ERROR (Wrong_type (store_val_type x, `core_val (`b `string)), p))
    in
    fetch id p aux scope env

  let get_ctrl id arity p env =
    match Base.H_string.find env id with
    | None -> raise (ERROR (Unbound_variable id, p))
    | Some (A_ctrl c, _, _) | Some (Ctrl c, _, _) ->
        let a = Ctrl.arity c in
        if a = arity then c else raise (ERROR (Arity (id, a, arity), p))
    | Some x ->
        raise
          (ERROR (Wrong_type (store_val_type x, `big_val (`ctrl arity)), p))

  let get_ctrl_fun id arity act_types p env =
    match Base.H_string.find env id with
    | None -> raise (ERROR (Unbound_variable id, p))
    | Some (A_ctrl_fun (a, forms), t, _) | Some (Ctrl_fun (a, forms), t, _)
      ->
        if a = arity then (a, forms, t)
        else raise (ERROR (Arity (id, a, arity), p))
    | Some x ->
        raise
          (ERROR
             ( Wrong_type (store_val_type x, `lambda (act_types, `ctrl arity)),
               p ))

  let is_atomic id p env =
    match Base.H_string.find env id with
    | None -> raise (ERROR (Unbound_variable id, p))
    | Some v -> (
        match get_val v with
        | A_ctrl _ | A_ctrl_fun _ -> true
        | Int _ | Float _ | Str _ | Big _
        | Big_fun (_, _)
        | Ctrl _
        | Ctrl_fun (_, _)
        | React _
        | React_fun (_, _, _, _, _, _, _, _)
        | Int_param _ | Str_param _ | Float_param _ ->
            false )

  let get_big id p env =
    match Base.H_string.find env id with
    | None -> raise (ERROR (Unbound_variable id, p))
    | Some (Big b, _, _) -> b
    | Some x ->
        raise (ERROR (Wrong_type (store_val_type x, `big_val `big), p))

  let get_big_fun id arg_types p env =
    match Base.H_string.find env id with
    | None -> raise (ERROR (Unbound_variable id, p))
    | Some (Big_fun (exp, forms), t, _) -> (exp, forms, t)
    | Some x ->
        raise
          (ERROR (Wrong_type (store_val_type x, `lambda (arg_types, `big)), p))

  let get_react id p (env : store) =
    match Base.H_string.find env id with
    | None -> raise (ERROR (Unbound_variable id, p))
    | Some (React r, _, _) -> r
    | Some x ->
        raise (ERROR (Wrong_type (store_val_type x, `big_val `react), p))

  let get_react_fun id arg_types p env =
    match Base.H_string.find env id with
    | None -> raise (ERROR (Unbound_variable id, p))
    | Some (React_fun (a, rew, l, r, eta, conds, label, forms), t, _) ->
        (a, rew, l, r, label, eta, conds, forms, t)
    | Some x ->
        raise
          (ERROR
             (Wrong_type (store_val_type x, `lambda (arg_types, `react)), p))

  (******** EVAL FUNCTIONS *********)

  let div_int l r p =
    match r with 0 -> raise (ERROR (Div_by_zero, p)) | d -> l / d

  let rec pow_int_aux base exp acc =
    match exp with 0 -> 1 | _ -> pow_int_aux base (exp - 1) (acc * base)

  let pow_int b e p =
    if e < 0 then
      raise
        (ERROR (Wrong_type (`core_val (`b `float), `core_val (`b `int)), p))
    else pow_int_aux b e 1

  let eval_str (exp : str_exp) (_scope : scope) (_env : store) =
    match exp with Str_val (v, _) -> Str v

  let eval_num (exp : num_exp) (_scope : scope) (_env : store) =
    match exp with
    | Num_int_val (v, _) -> Int v
    | Num_float_val (v, _) -> Float v

  let eval_var (exp : var_exp) (scope : scope) (env : store) =
    match exp with Var (id, p) -> get_var id p scope env

  let eval_plus (a : store_val) (b : store_val) p =
    match (a, b) with
    | Float f, Float g -> Float (f +. g)
    | Int f, Float g -> Float (float f +. g)
    | Float f, Int g -> Float (f +. float g)
    | Int f, Int g -> Int (f + g)
    | _, Float _ ->
        raise
          (ERROR
             (Wrong_type (fst (assign_type a []), `core_val (`b `float)), p))
    | Float _, _ ->
        raise
          (ERROR
             (Wrong_type (fst (assign_type b []), `core_val (`b `float)), p))
    | _, Int _ ->
        raise
          (ERROR (Wrong_type (fst (assign_type a []), `core_val (`b `int)), p))
    | Int _, _ ->
        raise
          (ERROR (Wrong_type (fst (assign_type b []), `core_val (`b `int)), p))
    | _ -> assert false

  let eval_minus (a : store_val) (b : store_val) p =
    match (a, b) with
    | Float f, Float g -> Float (f -. g)
    | Int f, Float g -> Float (float f -. g)
    | Float f, Int g -> Float (f -. float g)
    | Int f, Int g -> Int (f - g)
    | _, Float _ ->
        raise
          (ERROR
             (Wrong_type (fst (assign_type a []), `core_val (`b `float)), p))
    | Float _, _ ->
        raise
          (ERROR
             (Wrong_type (fst (assign_type b []), `core_val (`b `float)), p))
    | _, Int _ ->
        raise
          (ERROR (Wrong_type (fst (assign_type a []), `core_val (`b `int)), p))
    | Int _, _ ->
        raise
          (ERROR (Wrong_type (fst (assign_type b []), `core_val (`b `int)), p))
    | _ -> assert false

  let eval_uminus (a : store_val) _p =
    match a with
    | Float f -> Float (-.f)
    | Int i -> Int (-i)
    | _ -> assert false

  let eval_prod (a : store_val) (b : store_val) p =
    match (a, b) with
    | Float f, Float g -> Float (f *. g)
    | Int f, Float g -> Float (float f *. g)
    | Float f, Int g -> Float (f *. float g)
    | Int f, Int g -> Int (f * g)
    | _, Float _ ->
        raise
          (ERROR
             (Wrong_type (fst (assign_type a []), `core_val (`b `float)), p))
    | Float _, _ ->
        raise
          (ERROR
             (Wrong_type (fst (assign_type b []), `core_val (`b `float)), p))
    | _, Int _ ->
        raise
          (ERROR (Wrong_type (fst (assign_type a []), `core_val (`b `int)), p))
    | Int _, _ ->
        raise
          (ERROR (Wrong_type (fst (assign_type b []), `core_val (`b `int)), p))
    | _ -> assert false

  let eval_div (a : store_val) (b : store_val) p =
    match (a, b) with
    | Float f, Float g -> Float (f /. g)
    | Int f, Float g -> Float (float f /. g)
    | Float f, Int g -> Float (f /. float g)
    | Int f, Int g -> Int (div_int f g p)
    | _, Float _ ->
        raise
          (ERROR
             (Wrong_type (fst (assign_type a []), `core_val (`b `float)), p))
    | Float _, _ ->
        raise
          (ERROR
             (Wrong_type (fst (assign_type b []), `core_val (`b `float)), p))
    | _, Int _ ->
        raise
          (ERROR (Wrong_type (fst (assign_type a []), `core_val (`b `int)), p))
    | Int _, _ ->
        raise
          (ERROR (Wrong_type (fst (assign_type b []), `core_val (`b `int)), p))
    | _ -> assert false

  let eval_pow (a : store_val) (b : store_val) p =
    match (a, b) with
    | Float f, Float g -> Float (f ** g)
    | Int f, Float g -> Float (float f ** g)
    | Float f, Int g -> Float (f ** float g)
    | Int f, Int g -> Int (pow_int f g p)
    | _, Float _ ->
        raise
          (ERROR
             (Wrong_type (fst (assign_type a []), `core_val (`b `float)), p))
    | Float _, _ ->
        raise
          (ERROR
             (Wrong_type (fst (assign_type b []), `core_val (`b `float)), p))
    | _, Int _ ->
        raise
          (ERROR (Wrong_type (fst (assign_type a []), `core_val (`b `int)), p))
    | Int _, _ ->
        raise
          (ERROR (Wrong_type (fst (assign_type b []), `core_val (`b `int)), p))
    | _ -> assert false

  let rec eval_op (exp : op) (scope : scope) (env : store) =
    let eval' e = eval_exp e scope env in
    match exp with
    | Plus (e1, e2, loc) -> eval_plus (eval' e1) (eval' e2) loc
    | Minus (e1, e2, loc) -> eval_minus (eval' e1) (eval' e2) loc
    | UMinus (e1, loc) -> eval_uminus (eval' e1) loc
    | Prod (e1, e2, loc) -> eval_prod (eval' e1) (eval' e2) loc
    | Div (e1, e2, loc) -> eval_div (eval' e1) (eval' e2) loc
    | Pow (e1, e2, loc) -> eval_pow (eval' e1) (eval' e2) loc

  and eval_exp (exp : exp) (scope : scope) (env : store) =
    match exp with
    | ENum ne -> eval_num ne scope env
    | EStr se -> eval_str se scope env
    | EVar ve -> eval_var ve scope env
    | EOp op -> eval_op op scope env

  let as_int (e : store_val) p =
    match e with
    | Int v -> v
    | _ ->
        raise
          (ERROR (Wrong_type (fst (assign_type e []), `core_val (`b `int)), p))

  let as_float (e : store_val) p =
    match e with
    | Float v -> v
    | Int v -> float v
    | _ ->
        raise
          (ERROR
             (Wrong_type (fst (assign_type e []), `core_val (`b `float)), p))

  let as_str (e : store_val) p =
    match e with
    | Str v -> v
    | _ ->
        raise
          (ERROR
             (Wrong_type (fst (assign_type e []), `core_val (`b `string)), p))

  let cast_int (e : store_val) p = Int (as_int e p)

  let cast_float (e : store_val) p = Float (as_float e p)

  let cast_str (e : store_val) p = Str (as_str e p)

  let eval_exps exps (scope : scope) (env : store) =
    List.map
      (fun e ->
        match eval_exp e scope env with
        | Int _ as v -> (v, `b `int)
        | Float _ as v -> (v, `b `float)
        | Str _ as s -> (s, `b `string)
        | Big _
        | Big_fun (_, _)
        | Ctrl _
        | Ctrl_fun (_, _)
        | A_ctrl _
        | A_ctrl_fun (_, _)
        | React _
        | React_fun (_, _, _, _, _, _, _, _)
        | Int_param _ | Str_param _ | Float_param _ ->
            assert false)
      exps
    |> List.split

  let extend_scope (scope : scope) (forms : Id.t list)
      (nums : store_val list) (args_t : core_type list) (p : Loc.t) =
    let aux id v t = ScopeMap.add id (v, t, p) in
    List.combine forms (List.combine nums args_t)
    |> List.fold_left (fun s (id, (v, t)) -> aux id v (`core_val t) s) scope

  let eval_ctrl_fun id nums arity =
    let params =
      List.map
        (function
          | Float v -> Ctrl.F v
          | Int v -> Ctrl.I v
          | Str s -> Ctrl.S s
          | Big _
          | Big_fun (_, _)
          | Ctrl _
          | Ctrl_fun (_, _)
          | A_ctrl _
          | A_ctrl_fun (_, _)
          | React _
          | React_fun (_, _, _, _, _, _, _, _)
          | Int_param _ | Float_param _ | Str_param _ ->
              assert false)
        nums
    in
    Ctrl.C (id, params, arity)

  let check_atomic id p env face c = function
    | true -> if is_atomic id p env then Big.atom face c else Big.ion face c
    | false ->
        if is_atomic id p env then raise (ERROR (Atomic_ctrl id, p))
        else Big.ion face c

  (* Checking for duplicates. *)
  let parse_face ns p =
    let f = Link.parse_face ns in
    if List.length ns <> Link.Face.cardinal f then
      raise (ERROR (Not_face ns, p))
    else f

  (* flag=true Atomic controls allowed *)
  let eval_ion scope env env_t flag = function
    | Big_ion_exp (id, names, p) ->
        let c = get_ctrl id (List.length names) p env
        and face = parse_face names p in
        (check_atomic id p env face c flag, env_t)
    | Big_ion_fun_exp (id, args, names, p) -> (
        let exps, args_t = eval_exps args scope env
        and face = parse_face names p in
        let a, _, t = get_ctrl_fun id (List.length names) args_t p env in
        try
          let env_t' = app_exn env_t (dom_of_lambda t) args_t in
          let c = eval_ctrl_fun id exps a in
          (check_atomic id p env face c flag, env_t')
        with UNIFICATION ->
          raise
            (ERROR
               (Wrong_type (`lambda (args_t, `ctrl a), resolve_t env_t t), p))
        )

  let rec eval_big (exp : big_exp) (scope : scope) (env : store)
      (env_t : store_t) =
    let binary_eval l r scope env env_t f =
      let l_v, env_t' = eval_big l scope env env_t in
      let r_v, env_t'' = eval_big r scope env env_t' in
      (f l_v r_v, env_t'')
    in
    match exp with
    | Big_var (id, p) -> (get_big id p env, env_t)
    | Big_var_fun (id, args, p) -> (
        (* fun b(x,y) = 1; fun big a(x,y) = b(x + y, 5); a(3, 2 + 3.4); *)
        let exps, args_t =
          (* id = a --> ([Int 3; Float 5], [`b `int; `b `float]) *)
          eval_exps args scope env
        in
        (* (exp, [x; y], [`g 0; `g 1]) *)
        let exp, forms, t = get_big_fun id args_t p env in
        try
          (* Unification: `g 0 -> `b `int ; `g 1 -> `b `float *)
          let env_t' = app_exn env_t (dom_of_lambda t) args_t in
          (* Extend scope: x -> Int 3 y -> Float 5 *)
          let scope' = extend_scope scope forms exps args_t p in
          eval_big exp scope' env env_t'
        with UNIFICATION ->
          raise
            (ERROR (Wrong_type (`lambda (args_t, `big), resolve_t env_t t), p))
        )
    | Big_new_name (n, _) ->
        (Big.intro (Link.Face.singleton (Link.Name n)), env_t)
    | Big_comp (l, r, p) -> (
        try binary_eval l r scope env env_t Big.comp
        with Big.COMP_ERROR (i, j) -> raise (ERROR (Comp (i, j), p)) )
    | Big_tens (l, r, p) -> (
        try binary_eval l r scope env env_t Big.tens
        with Link.NAMES_ALREADY_DEFINED (i, o) ->
          raise (ERROR (Tens (i, o), p)) )
    | Big_par (l, r, _) -> binary_eval l r scope env env_t Big.par
    | Big_ppar (l, r, _) -> binary_eval l r scope env env_t Big.ppar
    | Big_share (a, psi, b, p) -> (
        try
          let a_v, env_t' = eval_big a scope env env_t in
          let psi_v, env_t'' = eval_big psi scope env env_t' in
          let b_v, env_t''' = eval_big b scope env env_t'' in
          (Big.share a_v psi_v b_v, env_t''')
        with
        | Big.COMP_ERROR (i, j) -> raise (ERROR (Comp (i, j), p))
        | Big.SHARING_ERROR -> raise (ERROR (Share, loc_of_big_exp psi)) )
    | Big_num (v, p) -> (
        match v with
        | 0 -> (Big.zero, env_t)
        | 1 -> (Big.one, env_t)
        | _ -> raise (ERROR (Unknown_big v, p)) )
    | Big_id exp ->
        ( Big.id (Big.Inter (exp.id_place, parse_face exp.id_link exp.id_loc)),
          env_t )
    | Big_merge (n, _) -> (Big.merge n, env_t)
    | Big_split (n, _) -> (Big.split n, env_t)
    | Big_plc exp ->
        (Big.placing exp.plc_parents exp.plc_roots Link.Face.empty, env_t)
    | Big_nest (ion, b, p) -> (
        try
          (* No atomic controls allowed here *)
          let i_v, env_t' = eval_ion scope env env_t false ion in
          let b_v, env_t'' = eval_big b scope env env_t' in
          (Big.nest i_v b_v, env_t'')
        with Big.COMP_ERROR (i, j) -> raise (ERROR (Comp (i, j), p)) )
    | Big_ion ion -> eval_ion scope env env_t true ion
    | Big_close exp -> (Big.closure (Link.parse_face [ exp.cl_name ]), env_t)
    | Big_sub exp ->
        ( Big.sub
            ~inner:(parse_face exp.in_names exp.sub_loc)
            ~outer:(Link.parse_face [ exp.out_name ]),
          env_t )
    | Big_wire (c, b, _) -> (
        let b_v, env_t' = eval_big b scope env env_t in
        match c with
        | Close_exp cs ->
            (Big.close (Link.parse_face (names_of_closures cs)) b_v, env_t')
        | Sub_exp s ->
            ( Big.rename
                ~inner:(parse_face s.in_names s.sub_loc)
                ~outer:(Link.parse_face [ s.out_name ])
                b_v,
              env_t' )
        | Merge_close_exp cs ->
            let outer = Link.parse_face [ "~0" ] in
            ( Big.rename ~inner:(Link.parse_face cs.m_cl_names) ~outer b_v
              |> Big.close outer,
              env_t' ) )
    | Big_par_fn (n, b, p) ->
        let n' = as_int (eval_exp n scope env) p in
        if n' < 0 then raise (ERROR (Invalid_iter_op n', p))
        else
          let b_v, env_t' = eval_big b scope env env_t in
          (Big.par_seq ~start:0 ~stop:n' (fun _ -> b_v), env_t')
    | Big_ppar_fn (n, b, p) ->
        let n' = as_int (eval_exp n scope env) p in
        if n' < 0 then raise (ERROR (Invalid_iter_op n', p))
        else
          let b_v, env_t' = eval_big b scope env env_t in
          (Big.ppar_seq ~start:0 ~stop:n' (fun _ -> b_v), env_t')

  let eval_eta = function Some (l, _) -> Some (Fun.parse l) | None -> None

  let is_some = function Some _ -> true | None -> false

  let eval_conds conds scope env env_t =
    let eval_cond (acc, e) c =
      (* TODO need to accumulate env_t? *)
      let p, e' = eval_big c.pred scope env e in
      let n = is_some c.neg in
      let w =
        match c.place with
        | Cond_Ctx -> AppCond.Ctx
        | Cond_Param -> AppCond.Param
      in
      ({ AppCond.neg = n; AppCond.pred = p; AppCond.where = w } :: acc, e')
    in
    List.fold_left eval_cond ([], env_t) conds

  let eval_reward env loc = function
    | None -> 0
    | Some rew -> as_int (eval_exp rew ScopeMap.empty env) loc

  (* Similar to binary eval *)
  let eval_react_aux lhs rhs conds scope env env_t =
    let lhs_v, env_t' = eval_big lhs scope env env_t in
    let rhs_v, env_t'' = eval_big rhs scope env env_t' in
    let conds', env_t''' = eval_conds conds scope env env_t'' in
    (lhs_v, rhs_v, conds', env_t''')

  let eval_react name action reward lhs rhs eta conds l scope env env_t p =
    let lhs_v, rhs_v, conds', env_t' =
      eval_react_aux lhs rhs conds scope env env_t
    in
    match
      P.parse_react name lhs_v rhs_v ~conds:conds'
        ( match l with
        | Some f_exp ->
            if String.equal action "" then
              `F (as_float (eval_exp f_exp scope env) p)
            else `P (action, reward, as_float (eval_exp f_exp scope env) p)
        | None -> `E () )
        eta
    with
    | None -> raise (ERROR (Reaction "Invalid reaction", p))
    | Some r -> (r, env_t')

  (* Compute all the combinations of input values *)
  let rec param_comb (pars : typed_store_val list list) =
    match pars with
    | [ x ] -> List.map (fun v -> [ v ]) x
    | x :: xs ->
        let aux1 v ls = List.map (fun l -> v :: l) ls in
        let rec aux2 l ls =
          match l with [] -> [] | x :: xs -> aux1 x ls @ aux2 xs ls
        in
        aux2 x (param_comb xs)
    | [] -> []

  let param_to_vals = function
    | Int_param vals, _, p ->
        List.map (fun v -> (Int v, `core_val (`b `int), p)) vals
    | Float_param vals, _, p ->
        List.map (fun v -> (Float v, `core_val (`b `float), p)) vals
    | Str_param vals, _, p ->
        List.map (fun v -> (Str v, `core_val (`b `string), p)) vals
    | Int _, _, _
    | Float _, _, _
    | Str _, _, _
    | Big _, _, _
    | Big_fun (_, _), _, _
    | Ctrl _, _, _
    | Ctrl_fun (_, _), _, _
    | A_ctrl _, _, _
    | A_ctrl_fun (_, _), _, _
    | React _, _, _
    | React_fun (_, _, _, _, _, _, _, _), _, _ ->
        assert false

  let is_param id env p =
    match Base.H_string.find env id with
    | None -> raise (ERROR (Unbound_variable id, p))
    | Some v -> (
        match get_val v with
        | Int_param _ | Str_param _ | Float_param _ -> true
        | Int _ | Float _ | Str _ | Big _
        | Big_fun (_, _)
        | Ctrl _
        | Ctrl_fun (_, _)
        | A_ctrl _
        | A_ctrl_fun (_, _)
        | React _
        | React_fun (_, _, _, _, _, _, _, _) ->
            false )

  module IdSet = Set.Make (struct
    type t = Id.t

    let compare = Id.compare
  end)

  (* [a + 3.0; c] -> [a , c] *)
  let scan_for_params env args =
    let auxV acc = function
      | Var (id, p) -> if is_param id env p then IdSet.add id acc else acc
    in
    let aux acc = function EVar v -> auxV acc v | _ -> acc in
    List.fold_left aux IdSet.empty args |> IdSet.elements

  (* a = [1.0; 2.0] c = [4; 8] [(a = 1.0; c = 4); (a = 1.0; c = 8); (a = 2.0;
     c = 4); (a = 2.0; c = 8)] *)
  let param_scopes env = function
    | [] -> [ ScopeMap.empty ]
    | ids ->
        List.map
          (fun id -> param_to_vals (Base.safe @@ Base.H_string.find env id))
          ids
        |> param_comb
        |> List.map (fun comb ->
               List.fold_left2
                 (fun acc id v -> ScopeMap.add id v acc)
                 ScopeMap.empty ids comb)

  let eval_react_fun_app id args env env_t p =
    scan_for_params env args |> param_scopes env
    |> List.fold_left
         (fun (acc, env_t) scope ->
           let exps, args_t = eval_exps args scope env in
           let action, reward, l, r, label, eta, conds, forms, t =
             get_react_fun id args_t p env
           in
           try
             let env_t' = app_exn env_t (dom_of_lambda t) args_t in
             let scope' = extend_scope scope forms exps args_t p in
             let r, env_t'' =
               eval_react id action reward l r eta conds label scope' env
                 env_t' p
             in
             (r :: acc, env_t'')
           with UNIFICATION ->
             raise
               (ERROR
                  ( Wrong_type (`lambda (args_t, `react), resolve_t env_t t),
                    p )))
         ([], env_t)

  let eval_pr env env_t pr =
    let aux env_t = function
      | Rul_id (id, p) -> ([ get_react id p env ], env_t)
      | Rul_id_fun (id, args, p) -> eval_react_fun_app id args env env_t p
    in
    let aux' (acc, env_t) id =
      let rs, env_t' = aux env_t id in
      (acc @ rs, env_t')
    in
    let pr_class, p =
      match pr with
      | Pr (ids, p) ->
          let rs, env_t' = List.fold_left aux' ([], env_t) ids in
          ((T.P_class rs, env_t'), p)
      | Pr_red (ids, p) ->
          let rs, env_t' = List.fold_left aux' ([], env_t) ids in
          ((T.P_rclass rs, env_t'), p)
    in
    if T.is_valid_priority (fst pr_class) then pr_class
    else raise (ERROR (Invalid_class, p))

  let eval_p_list eval_f chk_f env env_t l p =
    ( List.fold_left
        (fun (acc, env_t) pr ->
          let pr_class, env_t' = eval_f env env_t pr in
          (pr_class :: acc, env_t'))
        ([], env_t) l
    |> fun (l, e) -> (List.rev l, e) )
    |> fun x -> if chk_f x then x else raise (ERROR (Invalid_priorities, p))

  let eval_prs =
    let chk_fn (pl, _) =
      match pl with [] -> true | ps -> T.is_valid_priority_list ps
    in
    eval_p_list eval_pr chk_fn

  let eval_pred_fun_app id args env env_t p =
    let print_id id exps =
      let exp_s =
        List.map
          (function
            | (Int _ | Float _ | Str _) as v -> string_of_store_val v
            | _ -> assert false)
          exps
      in
      id ^ "(" ^ String.concat "," exp_s ^ ")"
    in
    scan_for_params env args |> param_scopes env
    |> List.fold_left
         (fun (acc, env_t) scope ->
           let exps, args_t = eval_exps args scope env in
           let exp, forms, t = get_big_fun id args_t p env in
           try
             let env_t' = app_exn env_t (dom_of_lambda t) args_t in
             let scope' = extend_scope scope forms exps args_t p in
             let b, env_t'' = eval_big exp scope' env env_t' in
             ((print_id id exps, b) :: acc, env_t'')
           with UNIFICATION ->
             raise
               (ERROR
                  (Wrong_type (`lambda (args_t, `big), resolve_t env_t t), p)))
         ([], env_t)

  let eval_preds env env_t preds =
    let aux env_t = function
      | Pred_id (id, reward, p) ->
          ([ ((id, eval_reward env p reward), get_big id p env) ], env_t)
      | Pred_id_fun (id, args, reward, p) ->
          let reward = eval_reward env p reward in
          let ps, env_t = eval_pred_fun_app id args env env_t p in
          (List.map (fun (id, bigraph) -> ((id, reward), bigraph)) ps, env_t)
    in
    let aux' (acc, env_t) id =
      let ps, env_t' = aux env_t id in
      (acc @ ps, env_t')
    in
    List.fold_left aux' ([], env_t) preds

  let eval_init exp env env_t =
    let (b, store), p =
      match exp with
      | Init (id, p) ->
          (eval_big (Big_var (id, p)) ScopeMap.empty env env_t, p)
      | Init_fun (id, args, p) ->
          (eval_big (Big_var_fun (id, args, p)) ScopeMap.empty env env_t, p)
    in
    if Big.is_ground b then (b, store)
    else raise (ERROR (Init_not_ground, p))

  (******** ADD TO STORE FUNCTIONS *********)

  let add_to_store fmt c env id (v : typed_store_val) =
    ( match Base.H_string.find env id with
    | Some x ->
        report_warning fmt c
          (Multiple_declaration (id, get_pos x, get_pos v))
    | None -> () );
    Base.H_string.replace env id v

  let update fmt c id (v : store_val) p env env_t =
    let t, env_t' = assign_type v env_t in
    add_to_store fmt c env id (v, t, p);
    env_t'

  (* Update a list of reaction rules with a new action and reward *)
  let update_action env action reward =
    List.map (fun d ->
        match d with
        | React_exp (id, _, _, lhs, rhs, label, eta, conds, p) ->
            Dreact
              (React_exp
                 ( id,
                   action,
                   eval_reward env p reward,
                   lhs,
                   rhs,
                   label,
                   eta,
                   conds,
                   p ))
        | React_fun_exp (id, _, _, forms, lhs, rhs, label, eta, conds, p) ->
            Dreact
              (React_fun_exp
                 ( id,
                   action,
                   eval_reward env p reward,
                   forms,
                   lhs,
                   rhs,
                   label,
                   eta,
                   conds,
                   p )))

  let store_decs fmt c decs env env_t =
    let rec aux env_t d =
      let unpack_cnds = function Some cnds -> cnds | None -> [] in
      let upd id v p = update fmt c id v p env env_t in
      match d with
      | Dctrl (Atomic (Ctrl_exp (id, ar, _), p)) ->
          upd id (A_ctrl (Ctrl.C (id, [], ar))) p
      | Dctrl (Atomic (Ctrl_fun_exp (id, forms, ar, _), p)) ->
          upd id (A_ctrl_fun (ar, forms)) p
      | Dctrl (Non_atomic (Ctrl_exp (id, ar, _), p)) ->
          upd id (Ctrl (Ctrl.C (id, [], ar))) p
      | Dctrl (Non_atomic (Ctrl_fun_exp (id, forms, ar, _), p)) ->
          upd id (Ctrl_fun (ar, forms)) p
      | Dint d ->
          upd d.d_id
            (cast_int (eval_exp d.d_exp ScopeMap.empty env) d.d_loc)
            d.d_loc
      | Dfloat d ->
          upd d.d_id
            (cast_float (eval_exp d.d_exp ScopeMap.empty env) d.d_loc)
            d.d_loc
      | Dstr d ->
          upd d.d_id
            (cast_str (eval_exp d.d_exp ScopeMap.empty env) d.d_loc)
            d.d_loc
      | Dbig (Big_exp (id, exp, p)) ->
          let b_v, env_t' = eval_big exp ScopeMap.empty env env_t in
          update fmt c id (Big b_v) p env env_t'
      | Dbig (Big_fun_exp (id, forms, exp, p)) ->
          upd id (Big_fun (exp, forms)) p
      | Dreact
          (React_exp (id, action, reward, lhs, rhs, label, eta, conds, p)) ->
          let r_v, env_t' =
            eval_react id action reward lhs rhs (eval_eta eta)
              (unpack_cnds conds) label ScopeMap.empty env env_t p
          in
          update fmt c id (React r_v) p env env_t'
      | Dreact
          (React_fun_exp
            (id, action, reward, forms, lhs, rhs, label, eta, conds, p)) ->
          upd id
            (React_fun
               ( action,
                 reward,
                 lhs,
                 rhs,
                 eval_eta eta,
                 unpack_cnds conds,
                 label,
                 forms ))
            p
      | Daction { action_id; action_rules = rules; action_reward = reward }
        ->
          update_action env action_id reward rules
          |> List.fold_left aux env_t
    in
    List.fold_left aux env_t decs

  let store_consts fmt c consts (env : store) =
    let aux = function
      | Cint d ->
          let v = cast_int (eval_exp d.d_exp ScopeMap.empty env) d.d_loc in
          ignore (update fmt c d.d_id v d.d_loc env [])
      | Cfloat d ->
          let v = cast_float (eval_exp d.d_exp ScopeMap.empty env) d.d_loc in
          ignore (update fmt c d.d_id v d.d_loc env [])
      | Cstr s ->
          let v = cast_str (eval_exp s.d_exp ScopeMap.empty env) s.d_loc in
          ignore (update fmt c s.d_id v s.d_loc env [])
    in
    List.iter aux consts

  (* Store simple Ints or Floats when the list of values has only one element *)
  let store_params fmt c (params : param_exp list) env =
    let rec eval_int_range start incr stop acc =
      let start' = start + incr in
      if start' <= stop then eval_int_range start' incr stop (start' :: acc)
      else acc
    in
    let rec eval_float_range start incr stop acc =
      let start' = start +. incr in
      if start' <= stop then eval_float_range start' incr stop (start' :: acc)
      else acc
    in
    let flatten_int = function [ v ] -> Int v | v -> Int_param v
    and flatten_float = function [ v ] -> Float v | v -> Float_param v
    and flatten_str = function [ v ] -> Str v | v -> Str_param v in
    (* let eval_int_param exp = *)
    (* try Int (eval_int exp ScopeMap.empty env) with *)
    (* | ERROR (Wrong_type _, _) -> get_int_param *)
    let aux = function
      | Param_int (ids, Param_int_val (exp, _), p) ->
          let v = cast_int (eval_exp exp ScopeMap.empty env) p in
          List.iter
            (fun id ->
              add_to_store fmt c env id (v, fst (assign_type v []), p))
            ids
      | Param_int (ids, Param_int_range (start, incr, stop, _), p) ->
          let s = as_int (eval_exp start ScopeMap.empty env) p in
          let v =
            flatten_int
              (eval_int_range s
                 (as_int (eval_exp incr ScopeMap.empty env) p)
                 (as_int (eval_exp stop ScopeMap.empty env) p)
                 [ s ])
          in
          List.iter
            (fun id ->
              add_to_store fmt c env id (v, fst (assign_type v []), p))
            ids
      | Param_int (ids, Param_int_set (exps, _), p) ->
          let v =
            List.map (fun e -> as_int (eval_exp e ScopeMap.empty env) p) exps
            |> List.sort_uniq (fun a b -> a - b)
            |> flatten_int
          in
          List.iter
            (fun id ->
              add_to_store fmt c env id (v, fst (assign_type v []), p))
            ids
      | Param_float (ids, Param_float_val (exp, _), p) ->
          let v = cast_float (eval_exp exp ScopeMap.empty env) p in
          List.iter
            (fun id ->
              add_to_store fmt c env id (v, fst (assign_type v []), p))
            ids
      | Param_float (ids, Param_float_range (start, incr, stop, _), p) ->
          let s = as_float (eval_exp start ScopeMap.empty env) p in
          let v =
            flatten_float
              (eval_float_range s
                 (as_float (eval_exp incr ScopeMap.empty env) p)
                 (as_float (eval_exp stop ScopeMap.empty env) p)
                 [ s ])
          in
          List.iter
            (fun id ->
              add_to_store fmt c env id (v, fst (assign_type v []), p))
            ids
      | Param_float (ids, Param_float_set (exps, _), p) ->
          let v =
            List.map
              (fun e -> as_float (eval_exp e ScopeMap.empty env) p)
              exps
            |> List.sort_uniq compare |> flatten_float
          in
          List.iter
            (fun id ->
              add_to_store fmt c env id (v, fst (assign_type v []), p))
            ids
      | Param_str (ids, Param_str_val (exp, _), p) ->
          let s = cast_str (eval_exp exp ScopeMap.empty env) p in
          List.iter
            (fun id ->
              add_to_store fmt c env id (s, fst (assign_type s []), p))
            ids
      | Param_str (ids, Param_str_set (exps, _), p) ->
          let v =
            List.map (fun e -> as_str (eval_exp e ScopeMap.empty env) p) exps
            |> List.sort_uniq String.compare
            |> flatten_str
          in
          List.iter
            (fun id ->
              add_to_store fmt c env id (v, fst (assign_type v []), p))
            ids
    in
    List.iter aux params

  (******** INSTANTIATE REACTIVE SYSTEM *********)

  let init_env fmt c consts =
    let store = Base.H_string.create 1000 in
    store_consts fmt c consts store;
    store

  let eval_model fmt c m env =
    let env_t = store_decs fmt c m.model_decs env [] in
    store_params fmt c m.model_rs.dbrs_params env;
    let b, env_t' = eval_init m.model_rs.dbrs_init env env_t in
    let p, env_t'' =
      eval_prs env env_t' m.model_rs.dbrs_pri m.model_rs.dbrs_loc
    in
    let preds, env_t''' = eval_preds env env_t'' m.model_rs.dbrs_preds in
    (b, p, preds, env_t''')

  (******** EXPORT STORE *********)

  let export decs (env : store) (env_t : store_t) path formats fmt c
      (print_fun : string -> int -> unit) =
    let concat = Filename.concat path in
    let write_pair id lhs rhs (f, ext) =
      let lhs_n, rhs_n = (id ^ "_lhs" ^ ext, id ^ "_rhs" ^ ext) in
      f lhs ~name:lhs_n ~path |> print_fun (concat lhs_n);
      f rhs ~name:rhs_n ~path |> print_fun (concat rhs_n)
    in
    let dummy_args (args_t : core_type list) =
      resolve_types env_t args_t |> List.map def_val
    in
    let aux id =
      let args_t =
        Base.safe @@ Base.H_string.find env id |> get_type |> dom_of_lambda
      in
      dummy_args args_t
    in
    let aux' eval_f id args p =
      eval_f id args env env_t p |> fst |> List.hd
    in
    let rec write f_write ext = function
      | Dctrl _ | Dint _ | Dstr _ | Dfloat _ -> ()
      | Dbig (Big_exp (id, _, p)) ->
          f_write (get_big id p env) ~name:(id ^ ext) ~path
          |> print_fun (concat (id ^ ext))
      | Dbig (Big_fun_exp (id, _, _, p)) ->
          let args = aux id in
          let b =
            fst
              (eval_big (Big_var_fun (id, args, p)) ScopeMap.empty env env_t)
          in
          f_write b ~name:(id ^ ext) ~path |> print_fun (concat (id ^ ext))
      | Dreact (React_exp (id, _, _, _, _, _, _, _, p)) ->
          let r = get_react id p env in
          write_pair id (T.lhs r) (T.rhs r) (f_write, ext)
      | Dreact (React_fun_exp (id, _, _, _, _, _, _, _, _, p)) ->
          let args = aux id in
          let r = aux' eval_react_fun_app id args p in
          write_pair id (T.lhs r) (T.rhs r) (f_write, ext)
      | Daction { action_id; action_rules = rules; action_reward = reward }
        ->
          update_action env action_id reward rules
          |> List.iter (write f_write ext)
    in
    List.iter
      (fun (f_write, ext) ->
        List.iter
          (fun d ->
            try write f_write ext d
            with Failure msg ->
              pp_print_flush fmt ();
              fprintf err_formatter "@[<v>@[%s: %s@]@." (Utils.err_opt c) msg)
          decs)
      formats

  (**************** EXPORT TO ML ********************)

  (*  let ml_of_dec id params exp =
   *   match params with
   *   (\* "let id =" *\)
   *   | [] -> "let " ^ id ^ " =\n  " ^ exp
   *   (\* "let id a b c =" *\)
   *   | params -> "let " ^ id ^ " " ^ String.concat " " params ^ " =\n  " ^ exp
   * 
   * let ml_of_ctrl exp =
   *   let aux id params c ar =
   *     ml_of_dec ("ctrl_" ^ id) params "Ctrl.C ("
   *     ^ c ^ ", [" ^ String.concat "; " params ^ "], " ^ string_of_int ar
   *     ^ ")"
   *   in
   *   match exp with
   *   | Ctrl_exp (id, ar, _) -> aux id [] ("\"" ^ id ^ "\"") ar
   *   | Ctrl_fun_exp (id, params, ar, _) -> aux id params ("\"" ^ id ^ "\"") ar
   * 
   * let ml_of_list f l = "[" ^ String.concat "; " (List.map f l) ^ "]"
   * 
   * let ml_of_ids = ml_of_list (fun x -> "\"" ^ x ^ "\"")
   * 
   * let ml_of_ints = ml_of_list string_of_int
   * 
   * let ml_of_var = function Var (id, _) -> id
   * 
   * let ml_of_str = function Str_val (v, _) -> v
   * 
   * (\* TO BE FIXED *\)
   * let ml_of_num = function
   *   | Num_int_val (v, _) -> string_of_int v
   *   | Num_float_val (v, _) -> string_of_float v
   * 
   * let ml_of_op = function
   *   | Plus (_, _, _) -> " + "
   *   | Minus (_, _, _) -> " - "
   *   | UMinus (_, _) -> "-"
   *   | Prod (_, _, _) -> " * "
   *   | Div (_, _, _) -> " / "
   *   | Pow (_, _, _) -> " ^ "
   * 
   * let ml_of_params p =
   *   List.map
   *     (fun e ->
   *       match e with
   *       | ENum n -> ml_of_num n
   *       | EStr s -> ml_of_str s
   *       | EVar v -> ml_of_var v
   *       | EOp op -> ml_of_op op)
   *     p
   *   |> String.concat " "
   * 
   * let ml_of_exp = function
   *   | ENum n -> ml_of_num n
   *   | EStr s -> ml_of_str s
   *   | EVar v -> ml_of_var v
   *   | EOp op -> ml_of_op op
   * 
   * let ml_of_face = function
   *   | [] -> "Link.Face.empty"
   *   | [ n ] -> "Link.Face.singleton (Link.Name \"" ^ n ^ "\")"
   *   | names -> "Link.parse_face " ^ ml_of_ids names
   * 
   * let rec ml_of_big = function
   *   | Big_var (id, _) -> (id : string)
   *   | Big_var_fun (id, params, _) ->
   *       (id : string) ^ " " ^ ml_of_params params
   *   | Big_new_name (n, _) -> "Big.intro (" ^ ml_of_face [ n ] ^ ")"
   *   | Big_num (v, _) -> (
   *       match v with 0 -> "Big.zero" | 1 -> "Big.one" | _ -> assert false )
   *   | Big_id exp ->
   *       "Big.id (Big.Inter ("
   *       ^ string_of_int exp.id_place
   *       ^ ", " ^ ml_of_face exp.id_link ^ "))"
   *   | Big_merge (n, _) -> "Big.merge " ^ string_of_int n
   *   | Big_split (n, _) -> "Big.split " ^ string_of_int n
   *   | Big_close exp -> "Big.closure (" ^ ml_of_face [ exp.cl_name ] ^ ")"
   *   | Big_sub exp ->
   *       "Big.sub (" ^ ml_of_face exp.in_names ^ ") ("
   *       ^ ml_of_face [ exp.out_name ]
   *       ^ ")"
   *   | Big_comp (a, b, _) ->
   *       "Big.comp\n(" ^ ml_of_big a ^ ")\n(" ^ ml_of_big b ^ ")"
   *   | Big_tens (a, b, _) ->
   *       "Big.tens\n(" ^ ml_of_big a ^ ")\n(" ^ ml_of_big b ^ ")"
   *   | Big_par (a, b, _) ->
   *       "Big.par\n(" ^ ml_of_big a ^ ")\n(" ^ ml_of_big b ^ ")"
   *   | Big_ppar (a, b, _) ->
   *       "Big.ppar\n(" ^ ml_of_big a ^ ")\n(" ^ ml_of_big b ^ ")"
   *   | Big_share (a, psi, b, _) ->
   *       "Big.share\n(" ^ ml_of_big a ^ ")\n(" ^ ml_of_big psi ^ ")\n("
   *       ^ ml_of_big b ^ ")"
   *   | Big_plc exp ->
   *       "Big.placing "
   *       ^ ml_of_list ml_of_ints exp.plc_parents
   *       ^ " "
   *       ^ string_of_int exp.plc_roots
   *       ^ " Link.Face.empty"
   *   | Big_ion (Big_ion_exp (id, names, _)) ->
   *       "Big.ion (" ^ ml_of_face names ^ ") ctrl_" ^ id
   *   | Big_ion (Big_ion_fun_exp (id, params, names, _)) ->
   *       "Big.ion (" ^ ml_of_face names ^ ") " ^ "(ctrl_" ^ id ^ " "
   *       ^ ml_of_params params ^ ")"
   *   | Big_nest (i, b, _) ->
   *       "Big.nest\n(" ^ ml_of_big (Big_ion i) ^ ")\n(" ^ ml_of_big b ^ ")"
   *   | Big_wire (c, b, _) -> (
   *       match c with
   *       | Close_exp cs ->
   *           "Big.close\n("
   *           ^ ml_of_face (names_of_closures cs)
   *           ^ ")\n(" ^ ml_of_big b ^ ")"
   *       | Sub_exp s ->
   *           "Big.rename ~inner:(" ^ ml_of_face s.in_names ^ ") ~outer:("
   *           ^ ml_of_face [ s.out_name ]
   *           ^ ") (" ^ ml_of_big b ^ ")"
   *       | Merge_close_exp cs ->
   *           let outer = ml_of_face [ "~0" ] in
   *           "Big.rename ~inner:(" ^ ml_of_face cs.m_cl_names ^ ") ~" ^ outer
   *           ^ " (" ^ ml_of_big b ^ ") |> Big.close " ^ outer )
   *   | Big_par_fn (n, b, _) ->
   *       "Big.par_seq ~start:0 ~stop:" ^ ml_of_exp n ^ "(fun _ -> "
   *       ^ ml_of_big b ^ ")"
   *   | Big_ppar_fn (n, b, _) ->
   *       "Big.ppar_seq ~start:0 ~stop:" ^ ml_of_exp n ^ "(fun _ -> "
   *       ^ ml_of_big b ^ ")"
   * 
   * let ml_of_eta = function
   *   | Some (l, _) -> "Some (Fun.parse " ^ ml_of_ints l ^ ")"
   *   | None -> "None"
   * 
   * let ml_of_react action reward lhs rhs l eta _conds =
   *   match T.typ with
   *   | Rs.BRS ->
   *       "Brs.parse_react_unsafe\n~lhs:(" ^ ml_of_big lhs ^ ")\n~rhs:("
   *       ^ ml_of_big rhs ^ ")\n(" ^ ml_of_eta eta ^ ")"
   *   | Rs.PBRS ->
   *       "Pbrs.parse_react_unsafe\n~lhs:(" ^ ml_of_big lhs ^ ")\n~rhs:("
   *       ^ ml_of_big rhs ^ ")\n("
   *       ^ (ml_of_exp @@ Base.safe l)
   *       ^ ")\n(" ^ ml_of_eta eta ^ ")"
   *   | Rs.SBRS ->
   *       "Sbrs.parse_react_unsafe\n~lhs:(" ^ ml_of_big lhs ^ ")\n~rhs:("
   *       ^ ml_of_big rhs ^ ")\n("
   *       ^ (ml_of_exp @@ Base.safe l)
   *       ^ ")\n(" ^ ml_of_eta eta ^ ")"
   *   | Rs.NBRS ->
   *       "Nbrs.parse_react_unsafe\n~action:(" ^ action ^ ")\n~reward:("
   *       ^ string_of_int reward ^ ")\n~lhs:(" ^ ml_of_big lhs ^ ")\n~rhs:("
   *       ^ ml_of_big rhs ^ ")\n("
   *       ^ (ml_of_exp @@ Base.safe l)
   *       ^ ")\n(" ^ ml_of_eta eta ^ ")"
   * 
   * let ml_of_pred = function
   *   | Pred_id (id, _, _) -> (id : string)
   *   | Pred_id_fun (id, params, _, _) ->
   *       (id : string) ^ " " ^ ml_of_params params
   * 
   * let ml_of_init = function
   *   | Init (id, _) -> (id : string)
   *   | Init_fun (id, params, _) -> (id : string) ^ " " ^ ml_of_params params
   * 
   * let ml_of_rul = function
   *   | Rul_id (id, _) -> (id : string)
   *   | Rul_id_fun (id, params, _) -> (id : string) ^ " " ^ ml_of_params params
   * 
   * let ml_of_rules ids = List.map ml_of_rul ids |> String.concat "; "
   * 
   * let ml_of_pri = function
   *   | Pr_red (ids, _) ->
   *       Rs.module_id T.typ ^ ".P_rclass [" ^ ml_of_rules ids ^ "]"
   *   | Pr (ids, _) ->
   *       Rs.module_id T.typ ^ ".P_class [" ^ ml_of_rules ids ^ "]"
   * 
   * (\* TO BE FIXED *\)
   * let ml_of_param = function
   *   | Param_int (ids, Param_int_val (exp, _), _) ->
   *       List.map (fun (id : string) -> ml_of_dec id [] (ml_of_exp exp)) ids
   *       |> String.concat " in\n"
   *   | Param_int (_, Param_int_range (_, _, _, _), _) -> ""
   *   | Param_int (_, Param_int_set (_, _), _) -> ""
   *   | Param_float (_, Param_float_val (_, _), _) -> ""
   *   | Param_float (_, Param_float_range (_, _, _, _), _) -> ""
   *   | Param_float (ids, Param_float_set (exps, _), _) ->
   *       List.map
   *         (fun (id : string) -> ml_of_dec id [] (ml_of_list ml_of_exp exps))
   *         ids
   *       |> String.concat " in\n"
   *   | Param_str (ids, Param_str_val (exp, _), _) ->
   *       List.map (fun (id : string) -> ml_of_dec id [] (ml_of_exp exp)) ids
   *       |> String.concat " in\n"
   *   | Param_str (_, Param_str_set (_, _), _) -> ""
   * 
   * let ml_of_dec = function
   *   | Dctrl (Atomic (exp, _)) | Dctrl (Non_atomic (exp, _)) -> ml_of_ctrl exp
   *   | Dbig (Big_exp (id, exp, _)) -> ml_of_dec id [] (ml_of_big exp)
   *   | Dbig (Big_fun_exp (id, params, exp, _)) ->
   *       ml_of_dec id params (ml_of_big exp)
   *   | Dreact (React_exp (id, action, reward, lhs, rhs, l, eta, conds, _)) ->
   *       ml_of_dec id [] (ml_of_react action reward lhs rhs l eta conds)
   *   | Dreact
   *       (React_fun_exp
   *         (id, action, reward, params, lhs, rhs, l, eta, conds, _)) ->
   *       ml_of_dec id params (ml_of_react action reward lhs rhs l eta conds)
   *   | Dint exp ->
   *       ml_of_dec exp.d_id [] ("Ctrl.I (" ^ ml_of_exp exp.d_exp ^ ")")
   *   | Dfloat exp ->
   *       ml_of_dec exp.d_id [] ("Ctrl.F (" ^ ml_of_exp exp.d_exp ^ ")")
   *   | Dstr exp ->
   *       ml_of_dec exp.d_id [] ("Ctrl.S (" ^ ml_of_exp exp.d_exp ^ ")")
   *   | Daction _ -> ""
   * 
   * (\*TODO*\)
   * 
   * let ml_of_model m file =
   *   let file_id = Filename.basename file |> Filename.chop_extension in
   *   "(\* Generated by BigraphER " ^ Version.version ^ " *\)\n(\* " ^ file
   *   ^ " *\)\n\
   *      open Bigraph\n\n\
   *      let int_of_param = function\n\
   *     \  | Ctrl.I i -> i\n\
   *     \  | Ctrl.F _ | Ctrl.S _ -> failwith \"cast error\"\n\n\
   *      let float_of_param = function\n\
   *     \  | Ctrl.F f -> f\n\
   *     \  | Ctrl.I _ | Ctrl.S _ -> failwith \"Cast error\"\n\n\
   *      let string_of_parm = function\n\
   *     \  | Ctrl.S s -> s\n\
   *     \  | Ctrl.I _ | Ctrl.F _ -> failwith \"Cast error\"\n\n"
   *   ^ ( List.map ml_of_dec m.model_decs
   *       @ List.map ml_of_param m.model_rs.dbrs_params
   *     |> String.concat "\n\n" )
   *   ^ "\n\nlet preds_" ^ file_id ^ "_big =\n  " ^ "[ "
   *   ^ (List.map ml_of_pred m.model_rs.dbrs_preds |> String.concat "; ")
   *   ^ " ]\n\n" ^ "let init_" ^ file_id ^ "_big =\n  "
   *   ^ ml_of_init m.model_rs.dbrs_init
   *   ^ "\n\n" ^ "let pri_" ^ file_id ^ "_big =\n  " ^ "[ "
   *   ^ (List.map ml_of_pri m.model_rs.dbrs_pri |> String.concat ";\n")
   *   ^ "\n]\n" *)
end
