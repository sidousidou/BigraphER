open Syntax
  
open Pretty
  
open Printf
  
(******************************* ERROR MESSAGES *******************************)
let err file = sprintf "File \"%s\":\nError: " file
  
let type_error i l r =
  sprintf "Expression %s has type %s, but is here used with type %s\n" i l r
  
let type_error_pri i l =
  sprintf "Identifier %s of type %s cannot be used in a priority class\n" i l
  
let class_error ide =
  sprintf "Stochastic reaction rule %s cannot be used in this class\n" ide
  
let unbound_error s = sprintf "Identifier %s not defined\n" s
  
let arity_error s a fs =
  sprintf "Control %s has arity %d, but is here used with %d name(s)\n" s a
    fs
  
let num_error exp = sprintf "\"%s\" Invalid numerical expression\n" exp
  
let bide_error s f a =
  sprintf
    "Identifier %s expects %d argument(s), but is here applied to %d argument(s)\n"
    s f a
  
let num_type_error i = sprintf "Identifier %s has invalid type\n" i
  
let placing_err e =
  sprintf "Expression %s is not a valid placing\n" (string_of_bexp e)
  
let comp_f a b e =
  sprintf
    "Expression %s is not a valid composition: names %s and %s do not match\n"
    (string_of_bexp e) (Link.string_of_face a) (Link.string_of_face b)
  
let comp_p a b e =
  sprintf
    "Expression %s is not a valid composition: interfaces %d and %d do not match\n"
    (string_of_bexp e) a b
  
let par_err e =
  sprintf "Expression %s is not a valid merge product\n" (string_of_bexp e)
  
let ppar_err e =
  sprintf "Expression %s is not a valid parallel composition\n"
    (string_of_bexp e)
  
let sharing_err e =
  sprintf "Expression %s is not a valid sharing\n" (Pretty.string_of_bexp e)
  
let react_error ide react =
  let l = Brs.redex react and r = Brs.reactum react in
  let i_l = Big.inner l
  and i_r = Big.inner r
  and o_l = Big.outer l
  and o_r = Big.outer r
  in
    if not (Big.inter_equals i_l i_r)
    then
      sprintf
        "Redex in reaction %s has inner face %s, while reactum has inner face %s\n"
        ide (Big.string_of_inter i_l) (Big.string_of_inter i_r)
    else
      if not (Big.inter_equals o_l o_r)
      then
        sprintf
          "Redex in reaction %s has outer face %s, while reactum has outer face %s\n"
          ide (Big.string_of_inter o_l) (Big.string_of_inter o_r)
      else
        if not (Big.is_epi l)
        then sprintf "Redex in reaction %s is not epimorphic\n" ide
        else sprintf "Redex in reaction %s is not monomorphic\n" ide
  
let sreact_error ide react =
  let l = Brs.sredex react and r = Brs.sreactum react
  and lambda = Brs.rate react in
  let i_l = Big.inner l
  and i_r = Big.inner r
  and o_l = Big.outer l
  and o_r = Big.outer r
  in
    if not (Big.inter_equals i_l i_r)
    then
      sprintf
        "Redex in stochastic reaction %s has inner face %s, while reactum has inner face %s\n"
        ide (Big.string_of_inter i_l) (Big.string_of_inter i_r)
    else
      if ( != ) o_l o_r
      then
        sprintf
          "Redex in stochastic reaction %s has outer face %s, while reactum has outer face %s\n"
          ide (Big.string_of_inter o_l) (Big.string_of_inter o_r)
      else
        if lambda <= 0.0
        then
          sprintf
            "Rate in stochastic reaction %s is %f, which is not greater than %f\n"
            ide lambda 0.0
        else
          if not (Big.is_epi l)
          then
            sprintf "Redex in stochastic reaction %s is not epimorphic\n" ide
          else
            sprintf "Redex in stochastic reaction %s is not monomorphic\n"
              ide
  
let wrong_init i = sprintf "Identifier %s cannot be declared init\n" i
  
type store_val =
  | V_big of Big.bg | V_react of Brs.react 
  | V_sreact of Brs.sreact

let type_of_val v =
  match v with
  | V_big _ -> "big"
  | V_react _ -> "react"
  | V_sreact _ -> "sreact"
  
type num_val = | V_int of int | V_float of float

type pri = | V_class of string list | V_classr of string list

let string_of_num v =
  match v with | V_int i -> string_of_int i | V_float f -> string_of_float f
  
let hash_of_list l =
  let h = Hashtbl.create (List.length l)
  in (List.iter (fun (a, b) -> Hashtbl.add h a b) l; h)
  
let init l =
  hash_of_list
    (List.map
       (fun d ->
          match d with
          | Ctrl_dec (i, _) -> (i, d)
          | Ctrl_dec_f (i, _, _) -> (i, d)
          | Big_dec (i, _) -> (i, d)
          | Big_dec_f (i, _, _) -> (i, d)
          | Rate_dec (i, _) -> (i, d)
          | Rate_dec_f (i, _, _) -> (i, d)
          | React_dec (i, _, _) -> (i, d)
          | React_dec_f (i, _, _, _) -> (i, d)
          | Sreact_dec (i, _, _, _) -> (i, d)
          | Sreact_dec_f (i, _, _, _, _) -> (i, d)
          | Fexp_dec (i, _) -> (i, d)
          | Iexp_dec (i, _) -> (i, d))
       l)
  
let get e ide e_string =
  try Hashtbl.find e ide
  with | Not_found -> failwith (e_string ^ (unbound_error ide))
  
let to_float v = match v with | V_int x -> V_float (float_of_int x) | _ -> v
  
let to_int v = match v with | V_float x -> V_int (int_of_float x) | _ -> v
  
let rec eval_num_exp exp acts env e_string =
  match exp with
  | Num_v s ->
      (try V_int (int_of_string s)
       with
       | _ ->
           (try V_float (float_of_string s)
            with
            | _ -> failwith (e_string ^ (num_error (string_of_num_exp exp)))))
  | Num_ide ide ->
      (* check actuals (ide,V_..) first then environment (ide, Fexp)*)
      (try List.assoc ide acts
       with
       | _ ->
           (match get env ide e_string with
            | Fexp_dec (_, exp) ->
                (try to_float (eval_num_exp exp [] env e_string)
                 with
                 | _ ->
                     failwith
                       (e_string ^ (num_error (string_of_num_exp exp))))
            | Iexp_dec (_, exp) ->
                (try to_int (eval_num_exp exp [] env e_string)
                 with
                 | _ ->
                     failwith
                       (e_string ^ (num_error (string_of_num_exp exp))))
            | _ -> failwith (e_string ^ (num_type_error ide))))
  | Plus (l, r) ->
      (match ((eval_num_exp l acts env e_string),
              (eval_num_exp r acts env e_string))
       with
       | (V_int x, V_int y) -> V_int (x + y)
       | (V_float x, V_float y) -> V_float (x +. y)
       | (V_int x, _) ->
           failwith (e_string ^ (type_error (string_of_int x) "int" "float"))
       | (V_float x, _) ->
           failwith
             (e_string ^ (type_error (string_of_float x) "float" "int")))
  | Minus (l, r) ->
      (match ((eval_num_exp l acts env e_string),
              (eval_num_exp r acts env e_string))
       with
       | (V_int x, V_int y) -> V_int (x - y)
       | (V_float x, V_float y) -> V_float (x -. y)
       | (V_int x, _) ->
           failwith (e_string ^ (type_error (string_of_int x) "int" "float"))
       | (V_float x, _) ->
           failwith
             (e_string ^ (type_error (string_of_float x) "float" "int")))
  | Prod (l, r) ->
      (match ((eval_num_exp l acts env e_string),
              (eval_num_exp r acts env e_string))
       with
       | (V_int x, V_int y) -> V_int (x * y)
       | (V_float x, V_float y) -> V_float (x *. y)
       | (V_int x, _) ->
           failwith (e_string ^ (type_error (string_of_int x) "int" "float"))
       | (V_float x, _) ->
           failwith
             (e_string ^ (type_error (string_of_float x) "float" "int")))
  | Div (l, r) ->
      (match ((eval_num_exp l acts env e_string),
              (eval_num_exp r acts env e_string))
       with
       | (V_int x, V_int y) -> V_int (x / y)
       | (V_float x, V_float y) -> V_float (x /. y)
       | (V_int x, _) ->
           failwith (e_string ^ (type_error (string_of_int x) "int" "float"))
       | (V_float x, _) ->
           failwith
             (e_string ^ (type_error (string_of_float x) "float" "int")))
  | Pow (l, r) ->
      (match ((eval_num_exp l acts env e_string),
              (eval_num_exp r acts env e_string))
       with
       | (V_float x, V_float y) -> V_float (x ** y)
       | (V_int x, _) ->
           failwith (e_string ^ (type_error (string_of_int x) "int" "float"))
       | (V_float x, _) ->
           failwith
             (e_string ^ (type_error (string_of_float x) "float" "int")))
  
let check_types types_h ide acts e_string =
  let type_of_acts l =
    List.map
      (fun x -> match x with | V_int _ -> "int" | V_float _ -> "float") l
  and aux l = sprintf "(%s)" (String.concat "," l)
  in
    try
      let t_forms = get types_h ide e_string
      in
        if
          List.exists (fun (a, b) -> ( != ) a b)
            (List.combine t_forms (type_of_acts acts))
        then
          failwith
            (e_string ^
               (type_error ide (aux (type_of_acts acts)) (aux t_forms)))
        else ()
    with | _ -> Hashtbl.add types_h ide (type_of_acts acts)
  
let rec eval_bexp exp acts env types_h e_string =
  match exp with
  | Big_ide (ide, params) ->
      (match get env ide e_string with
       | Big_dec (ide, bexp) ->
           if (List.length params) = 0
           then eval_bexp bexp [] env types_h e_string
           else failwith (e_string ^ (bide_error ide 0 (List.length params)))
       | Big_dec_f (ide, forms, bexp) ->
           if (List.length params) = (List.length forms)
           then
             (let evals =
                List.map (fun exp -> eval_num_exp exp acts env e_string)
                  params
              in
                (check_types types_h ide evals e_string;
                 let acts_bind = List.combine forms evals
                 in eval_bexp bexp acts_bind env types_h e_string))
           else
             failwith
               (e_string ^
                  (bide_error ide (List.length forms) (List.length params)))
       | _ -> failwith (e_string ^ (num_type_error ide)))
  | Big_plac (parents, roots) ->
      (try Big.placing parents roots Link.Face.empty
       with | _ -> failwith (e_string ^ (placing_err exp)))
  | Big_comp (closures, bexp) ->
      let b = eval_bexp bexp acts env types_h e_string
      and c_names = Link.parse_face closures in
      let (Big.Inter (i, o_face)) = Big.outer b in
      let id = Big.id (Big.Inter (i, (Link.Face.diff o_face c_names))) in
      let par = (* id || /x || /y ... *)
        Big.ppar_of_list
          (id ::
            (List.map (fun n -> Big.closure (Link.parse_face [ n ])) closures))
      in
        (try Big.comp par b
         with
         | Link.INTERFACES_MISMATCH (a, b) ->
             failwith (e_string ^ (comp_f a b exp))
         | Place.INTERFACES_MISMATCH (a, b) ->
             failwith (e_string ^ (comp_p a b exp)))
  | Big_close closure -> Big.closure (Link.parse_face [ closure ])
  | Big_par (l, r) ->
      (try
         Big.par (eval_bexp l acts env types_h e_string)
           (eval_bexp r acts env types_h e_string)
       with | _ -> failwith (e_string ^ (par_err exp)))
  | Big_ppar (l, r) ->
      (try
         Big.ppar (eval_bexp l acts env types_h e_string)
           (eval_bexp r acts env types_h e_string)
       with | _ -> failwith (e_string ^ (ppar_err exp)))
  | Big_nest (l, r) ->
      Big.nest (eval_bexp l acts env types_h e_string)
        (eval_bexp r acts env types_h e_string)
  | Big_el i ->
      (match i with
       | 0 -> Big.zero
       | 1 -> Big.one
       | _ ->
           failwith (e_string ^ (type_error (string_of_int i) "int" "big")))
  | Big_id (i, names) -> Big.id (Big.Inter (i, (Link.parse_face names)))
  | Big_ion (c, params, names) ->
      (match get env c e_string with
       | Ctrl_dec (c, i) ->
           if (List.length params) = 0
           then
             if (List.length names) = i
             then Big.ion (Link.parse_face names) (Base.Ctrl c)
             else failwith (e_string ^ (arity_error c i (List.length names)))
           else failwith (e_string ^ (bide_error c 0 (List.length params)))
       | Ctrl_dec_f (c, forms, i) ->
           if (List.length names) = i
           then
             (let a = List.length params
              and b = List.length forms
              in
                if a = b
                then
                  (let evals =
                     List.map (fun n -> eval_num_exp n acts env e_string)
                       params
                   in
                     (check_types types_h c evals e_string;
                      let x =
                        sprintf "%s(%s)" c
                          (String.concat ","
                             (List.map (fun v -> string_of_num v) evals))
                      in Big.ion (Link.parse_face names) (Base.Ctrl x)))
                else failwith (e_string ^ (bide_error c b a)))
           else failwith (e_string ^ (arity_error c i (List.length names)))
       | _ -> failwith (e_string ^ (num_type_error c)))
  | Big_share (a, b, c) ->
      (try
         Big.share (eval_bexp a acts env types_h e_string)
           (eval_bexp b acts env types_h e_string)
           (eval_bexp c acts env types_h e_string)
       with | Big.SHARING_ERROR -> failwith (e_string ^ (sharing_err exp)))
  
let eval_init store =
  function
  | Init (ide, acts) ->
      (fun env types_h e_string ->
         let dec = get env ide e_string
         in
           match dec with
           | Big_dec (_, bexp) ->
               if (List.length acts) = 0
               then
                 (Hashtbl.add store ide
                    (V_big (eval_bexp bexp [] env types_h e_string));
                  ide)
               else
                 failwith (e_string ^ (bide_error ide 0 (List.length acts)))
           | Big_dec_f (_, forms, bexp) ->
               let a = List.length acts
               and b = List.length forms
               in
                 if a = b
                 then
                   (let eval_acts =
                      List.map (fun exp -> eval_num_exp exp [] env e_string)
                        acts in
                    let ide_store =
                      sprintf "%s(%s)" ide
                        (String.concat ","
                           (List.map (fun v -> string_of_num v) eval_acts))
                    and acts_bind = List.combine forms eval_acts
                    in
                      (Hashtbl.add store ide_store
                         (V_big
                            (eval_bexp bexp acts_bind env types_h e_string));
                       ide_store))
                 else failwith (e_string ^ (bide_error ide b a))
           | _ -> failwith (e_string ^ (wrong_init ide)))
  
(* output = (ide_rul, [V_int; V_float; ...])*)
let gen_rules rules vars e_string =
  let tmp =
    List.map
      (fun x ->
         match x with
         | Int_dec (ide, l) -> (ide, (List.map (fun x -> V_int x) l))
         | Float_dec (ide, l) -> (ide, (List.map (fun x -> V_float x) l)))
      vars
  in
    List.fold_left
      (fun acc ->
         function
         | Rul (ide, forms) ->
             (match forms with
             | [] -> (ide, [])::acc
             | _ -> (List.map (fun x -> (ide, x))
                (Base.par_comb
                   (List.map
                      (fun x ->
                         try List.assoc x tmp
                         with
                         | _ -> failwith (e_string ^ (num_type_error ide)))
                      forms)))
               @ acc))
      [] rules
  
let eval_rules store vars rules env types_h e_string =
  let r_list = gen_rules rules vars e_string
  in
    List.iter
      (fun (ide, acts) ->
         let dec = get env ide e_string
         in
           match dec with
           | React_dec (ide, l, r) ->
               if (List.length acts) = 0
               then
                 (let lb = eval_bexp l [] env types_h e_string
                  and rb = eval_bexp r [] env types_h e_string in
                  let react = Brs.React (lb, rb)
                  in
                    if Brs.is_valid_react react
                    then Hashtbl.add store ide (V_react react)
                    else failwith (e_string ^ (react_error ide react)))
               else
                 failwith (e_string ^ (bide_error ide 0 (List.length acts)))
           | React_dec_f (ide, forms, l, r) ->
               let a = List.length acts
               and b = List.length forms
               in
                 if a = b
                 then
                   (let acts_bind = List.combine forms acts in
                    let lb = eval_bexp l acts_bind env types_h e_string
                    and rb = eval_bexp r acts_bind env types_h e_string in
                    let react = Brs.React (lb, rb)
                    in
                      if Brs.is_valid_react react
                      then
                        (let ide_store =
                           sprintf "%s(%s)" ide
                             (String.concat ","
                                (List.map (fun v -> string_of_num v) acts))
                         in Hashtbl.add store ide_store (V_react react))
                      else failwith (e_string ^ (react_error ide react)))
                 else failwith (e_string ^ (bide_error ide b a))
           | _ -> failwith (e_string ^ (num_type_error ide)))
      r_list
  
let eval_rate =
  function
  | Rate (ide, params) ->
      (fun acts env e_string ->
         let dec = get env ide e_string
         in
           match dec with
           | Rate_dec (ide, fexp) ->
               if (List.length params) = 0
               then
                 (match eval_num_exp fexp [] env e_string with
                  | V_float f -> f
                  | _ ->
                      failwith
                        (type_error (string_of_num_exp fexp) "int" "float"))
               else failwith (bide_error ide 0 (List.length params))
           | Rate_dec_f (ide, forms, fexp) ->
               let a = List.length params
               and b = List.length forms
               in
                 if a = b
                 then
                   (let acts_bind =
                      List.combine forms
                        (List.map
                           (fun exp -> eval_num_exp exp acts env e_string)
                           params)
                    in
                      match eval_num_exp fexp acts_bind env e_string with
                      | V_float f -> f
                      | _ ->
                          failwith
                            (e_string ^
                               (type_error (string_of_num_exp fexp) "int"
                                  "float")))
                 else failwith (e_string ^ (bide_error ide b a))
           | _ -> failwith (e_string ^ (num_type_error ide)))
  
let eval_srules store vars rules env types_h e_string =
  let r_list = gen_rules rules vars e_string
  in
    List.iter
      (fun (ide, acts) ->
         let dec = get env ide e_string
         in
           match dec with
           | Sreact_dec (ide, l, r, rate) ->
               if (List.length acts) = 0
               then
                 (let lb = eval_bexp l [] env types_h e_string
                  and rb = eval_bexp r [] env types_h e_string in
                  let react =
                    Brs.Sreact (lb, rb, (eval_rate rate [] env e_string))
                  in
                    if Brs.is_valid_sreact react
                    then Hashtbl.add store ide (V_sreact react)
                    else failwith (e_string ^ (sreact_error ide react)))
               else
                 failwith (e_string ^ (bide_error ide 0 (List.length acts)))
           | Sreact_dec_f (ide, forms, l, r, rate) ->
               let a = List.length acts
               and b = List.length forms
               in
                 if a = b
                 then
                   (let acts_bind = List.combine forms acts in
                    let lb = eval_bexp l acts_bind env types_h e_string
                    and rb = eval_bexp r acts_bind env types_h e_string in
                    let react =
                      Brs.Sreact (lb, rb,
                        (eval_rate rate acts_bind env e_string))
                    in
                      if Brs.is_valid_sreact react
                      then
                        (let ide_store =
                           sprintf "%s(%s)" ide
                             (String.concat ","
                                (List.map (fun v -> string_of_num v) acts))
                         in Hashtbl.add store ide_store (V_sreact react))
                      else failwith (e_string ^ (sreact_error ide react)))
                 else failwith (e_string ^ (bide_error ide b a))
           | _ -> failwith (e_string ^ (num_type_error ide)))
      r_list
  
let eval_model (decs, brs) file =
  let store = Hashtbl.create 100
  and types = Hashtbl.create 100
  and env = init decs
  and e_string = err file
  in
    match brs with
    | Brs (params, s0, rules) ->
        let ide = eval_init store s0 env types e_string
        in
          (eval_rules store params rules env types e_string;
           (store, ide, false))
    | Sbrs (params, s0, rules) ->
        let ide = eval_init store s0 env types e_string
        in
          (eval_srules store params rules env types e_string;
           (store, ide, true))
  
let eval_priorities ps store ctmc file = (* pri list *)
  let e_string = err file
  and init_vars vars = (* (ide, string list)... *)
    List.map
      (fun d ->
         match d with
         | Int_dec (ide, xs) -> (ide, (List.map string_of_int xs))
         | Float_dec (ide, xs) -> (ide, (List.map string_of_float xs)))
      vars in
  let aux rs e flag = (* ide list *)
    List.fold_left
      (fun acc ->
         function
         | Rul (ide, params) ->
             let vals =
               List.map
                 (fun p -> (*string list list*)
                    try List.assoc p e
                    with | _ -> failwith (e_string ^ (num_type_error p)))
                 params
             in
               (match vals with
                | [] ->
                    ((match get store ide e_string with
                      | V_big _ ->
                          failwith (e_string ^ (type_error_pri ide "big"))
                      | V_sreact v ->
                          if flag
                          then
                            if Brs.is_instant v
                            then ()
                            else failwith (e_string ^ (class_error ide))
                          else ()
                      | _ -> ());
                     [ ide ] @ acc)
                | _ ->
                    (List.map
                       (fun l ->
                          let res =
                            sprintf "%s(%s)" ide (String.concat "," l)
                          in
                            ((match get store res e_string with
                              | V_big _ ->
                                  failwith
                                    (e_string ^ (type_error_pri res "big"))
                              | V_sreact v ->
                                  if flag
                                  then
                                    if Brs.is_instant v
                                    then ()
                                    else
                                      failwith (e_string ^ (class_error res))
                                  else ()
                              | _ -> ());
                             res))
                       (Base.par_comb vals))
                      @ acc))
      [] rs
  in
    match ps with
    | [] -> (* create a class with all the rules in store *)
        let get_rules h =
          Hashtbl.fold
            (fun ide v acc -> match v with | V_big _ -> acc | _ -> ide :: acc)
            h []
        in [ V_class (get_rules store) ]
    | _ -> (* build the classes *)
        List.map
          (fun p ->
             match p with
             | Pri_class (vars, rules) ->
                 V_class (aux rules (init_vars vars) false)
             | Pri_classr (vars, rules) ->
                 V_classr (aux rules (init_vars vars) ctmc))
          ps

(* output = (ide_pred, [V_int; V_float; ...])*)
let gen_preds preds vars e_string =
  let tmp =
    List.map
      (fun x ->
         match x with
         | Int_dec (ide, l) -> (ide, (List.map (fun x -> V_int x) l))
         | Float_dec (ide, l) -> (ide, (List.map (fun x -> V_float x) l)))
      vars
  in
    List.fold_left (fun acc (Pred (ide, forms)) ->
            (match forms with
              | [] -> (ide, [])::acc
              | _ -> (List.map (fun x -> (ide, x))
                  (Base.par_comb
                   (List.map
                      (fun x ->
                         try List.assoc x tmp
                         with
                         | _ -> failwith (e_string ^ (num_type_error ide)))
                      forms))) @ acc)
            )
      [] preds

let eval_preds vars preds env e_string types_h =
  let p_list = gen_preds preds vars e_string
  in
    List.map
      (fun (ide, acts) ->
         let dec = get env ide e_string
         in
           match dec with
           | Big_dec (ide, bexp) ->
              if (List.length acts) = 0
               then
                 (ide, eval_bexp bexp [] env types_h e_string)
               else
                 failwith (e_string ^ (bide_error ide 0 (List.length acts)))
           | Big_dec_f (ide, forms, bexp) ->
              let a = List.length acts
              and b = List.length forms 
              in
                if a = b
                then
                   (let acts_bind = List.combine forms acts in
                    let b = eval_bexp bexp acts_bind env types_h e_string
                    in
                      (let ide_store = sprintf "%s(%s)" ide
                          (String.concat ","
                                (List.map (fun v -> string_of_num v) acts))
                         in (ide_store, b)))
                else failwith (e_string ^ (bide_error ide b a))
           | _ -> failwith (e_string ^ (num_type_error ide)))
      p_list

let eval_properties (decs, bilog) file = (* (string * bg) list *)
  let e_string = err file
  and env = init decs
  and types = Hashtbl.create 100
  in match bilog with
    | B_null -> [] 
    | Bilog (params, preds) -> eval_preds params preds env e_string types

let print_store store =
  (printf "%d items stored in memory:\n" (Hashtbl.length store);
   Hashtbl.iter (fun i v -> printf "%s %s\n" (type_of_val v) i) store;
   printf "\n")
  
let print_pri q =
  let rec aux xs i =
    match xs with
    | x :: xs ->
        (match x with
         | V_class rs ->
             (printf "Priority %d: %d rules\n%s\n" i (List.length rs)
                (String.concat "\n" rs);
              aux xs (i - 1))
         | V_classr rs ->
             (printf "Priority* %d: %d rules\n%s\n" i (List.length rs)
                (String.concat "\n" rs);
              aux xs (i - 1)))
    | [] -> ()
  in aux q ((List.length q) - 1)

let print_pred l = 
  List.iter (fun (ide, _) ->
    printf "bilog %s\n" ide) l
  

