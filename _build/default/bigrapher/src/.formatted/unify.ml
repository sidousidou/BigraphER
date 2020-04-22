(* Type constraints and unification algorithm *)

exception UNIFICATION

type gen_type = string

type base_type = [ `int | `float | `string ]

type core_type = [ `g of gen_type | `b of base_type ]

let current_alpha = ref (97, 0)

let next_gen () =
  let next_alpha = function
    | 122, i ->
        current_alpha := (97, i + 1);
        (122, i)
    | n, i ->
        current_alpha := (n + 1, i);
        (n, i)
  in
  match next_alpha !current_alpha with
  | n, 0 -> "'" ^ String.make 1 (char_of_int n)
  | n, i -> "'" ^ String.make 1 (char_of_int n) ^ string_of_int i

let string_of_core_t = function
  | `b `int -> "int"
  | `b `float -> "float"
  | `b `string -> "string"
  | `g s -> s

let int_or_float = "[ int | float ]"

let core_type_str = "[ int | float | string ]"

let string_of_core_t_list l =
  "[" ^ String.concat "," (List.map string_of_core_t l) ^ "]"

type fun_type = [ `big | `ctrl of int | `react | `sreact ]

let string_of_fun_t = function
  | `big -> "big"
  | `ctrl arity -> "ctrl{" ^ string_of_int arity ^ "}"
  | `react -> "react"
  | `sreact -> "sreact"

type lambda = core_type list * fun_type (* [`int; `float] -> `ctrl *)

let string_of_lambda (l, r) =
  string_of_core_t_list l ^ " -> " ^ string_of_fun_t r

let string_of_app l r =
  "(" ^ string_of_lambda l ^ ") " ^ string_of_core_t_list r

(* Naive union-find for type-equivalence classes *)
module Gamma = Set.Make (struct
  type t = gen_type

  let compare = String.compare
end)

type store_t = (Gamma.t * base_type option) list

let string_of_base_opt = function
  | None -> "nil"
  | Some `int -> "int"
  | Some `float -> "float"
  | Some `string -> "string"

let string_of_gamma set =
  "{" ^ (Gamma.elements set |> String.concat ", ") ^ "}"

let post_exn env (constr, t) =
  let union_set, disjoint_sets =
    env
    |> List.partition (fun (s, _) -> not Gamma.(is_empty (inter constr s)))
  in
  let new_class =
    (constr, t) :: union_set
    |> List.fold_left
         (fun (acc, t) (set, t') ->
           match (t, t') with
           | None, x -> (Gamma.union acc set, x)
           | (Some _ as x), None -> (Gamma.union acc set, x)
           | (Some `int as x), Some `int
           | (Some `string as x), Some `string
           | (Some `float as x), Some `float ->
               (Gamma.union acc set, x)
           | Some `string, Some `int
           | Some `string, Some `float
           | Some `int, Some `string
           | Some `float, Some `string
           | Some `int, Some `float
           | Some `float, Some `int ->
               raise UNIFICATION)
         (Gamma.empty, None)
  in
  new_class :: disjoint_sets

(* Example: N(type0, type1) = [ type0; type1] -> `ctrl 0 dom, codom N(3, 4.8)
   = ([ type0; type1] -> `ctrl 0) [`int; `float] args type0 = `int type1 =
   `float *)
let app_exn env (dom : core_type list) (args : core_type list) =
  try
    List.fold_left2
      (fun env d a ->
        match (d, a) with
        | `g t0, `g t1 ->
            post_exn env (Gamma.(empty |> add t0 |> add t1), None)
        | `g t0, `b t1 | `b t1, `g t0 ->
            post_exn env (Gamma.(empty |> add t0), Some t1)
        | `b `int, `b `int | `b `string, `b `string | `b `float, `b `float ->
            env
        | `b `string, `b `int
        | `b `int, `b `string
        | `b `string, `b `float
        | `b `float, `b `string
        | `b `int, `b `float
        | `b `float, `b `int ->
            raise UNIFICATION)
      env dom args
  with Invalid_argument _ -> raise UNIFICATION

let add_type env (t : gen_type) = (Gamma.(empty |> add t), None) :: env

let add_types (t_list : gen_type list) env =
  List.fold_left add_type env t_list

let box_gen_types t_list = List.map (fun t -> `g t) t_list

let resolve_type env = function
  | `g t -> (
      try
        let set, t' = List.find (fun (set, _) -> Gamma.mem t set) env in
        match t' with None -> `g (Gamma.min_elt set) | Some b -> `b b
      with Not_found -> assert false )
  | `b _ as t -> t

let resolve_types env = List.map (resolve_type env)

let string_of_env env =
  List.map
    (fun (set, t) -> string_of_gamma set ^ " -> " ^ string_of_base_opt t)
    env
  |> String.concat "\n"
