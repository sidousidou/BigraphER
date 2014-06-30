open Utils

(* Actual parameters *)
type act = 
  | I of int 
  | R of float

type param =
  | A of act
  | F of string

type ide = string

type t = 
  | Ctrl of ide * act list
  | Fun_ctrl of ide *  param list (* at least one formal *)

let name = function
  | Ctrl (ide, _) | Fun_ctrl (ide, _) -> ide

let string_of_act = function
  | I p -> string_of_int p
  | R p -> string_of_float p

let string_of_param = function
  | A p -> string_of_act p
  | F p -> p

let string_of_list f = function
  | [] -> ""
  | x  -> "(" ^ (String.concat "," (List.map f x)) ^ ")"

let to_string = function
  | Ctrl (c, a_list) -> c ^ (string_of_list string_of_act a_list)
  | Fun_ctrl (c, p_list) -> c ^ (string_of_list string_of_param p_list)

let ide_compare = String.compare

let int_compare a b = a - b

let float_compare (a : float) (b : float) = compare a b 

let act_compare a0 a1 = 
  match (a0, a1) with
  | (I a, I b) -> int_compare a b
  | (I _, _) -> -1
  | (_ , I _) -> 1
  | (R a, R b) -> float_compare a b

let param_compare p0 p1 = 
  match (p0, p1) with
  | (A a0, A a1) -> act_compare a0 a1
  | (A _, _) | (F _, _) -> 0 

(* Functional controls act like regular expressions: formals match anything. *)
let compare c0 c1 =
  match (c0, c1) with
  | (Ctrl (ide0, acts0), Ctrl (ide1, acts1)) -> (
      match ide_compare ide0 ide1 with
      | 0 -> list_compare act_compare acts0 acts1
      | x -> x
    )
  | (Ctrl (ide0, acts), Fun_ctrl (ide1, params)) -> (
      match ide_compare ide0 ide1 with
      | 0 -> list_compare param_compare (List.map (fun a -> A a) acts) params
      | x -> x
    )
  | (Fun_ctrl (ide0, params), Ctrl (ide1, acts)) -> (
      match ide_compare ide0 ide1 with
      | 0 -> list_compare param_compare params (List.map (fun a -> A a) acts)
      | x -> x
    )
  | (Fun_ctrl (ide0, params0), Fun_ctrl (ide1, params1)) -> (
      match ide_compare ide0 ide1 with
      | 0 -> list_compare param_compare params0 params1
      | x -> x
    )

let actuals = function
  | Ctrl (_, l) -> Some l
  | Fun_ctrl _ -> None

let rec p_norm i = function
  | [] -> []
  | A _ as a :: ps -> a :: (p_norm i ps)
  | F _ :: ps -> (F ("x" ^ (string_of_int i))) :: (p_norm (i + 1) ps)

let norm = function
  | Ctrl _ as c -> c
  | Fun_ctrl (ide, params) -> Fun_ctrl (ide, p_norm 0 params)

let parse (s : string) =
  try
    let i = String.index s '(' in
    let c = String.sub s 0 (i + 1) in 
    assert (s.[(String.length s) - 1] = ')');
    let params = 
      Str.split (Str.regexp ",") 
        (String.sub s (i + 1) ((String.length s) - i - 2)) in
    let p = List.map (fun token ->
        try
          A (I (int_of_string token))
        with
        | Failure _ -> (
            try
              A (R (float_of_string token))
            with
            | Failure _ -> F token
          )
      ) params in
    if List.exists (function
        | F _ -> true
        | A _ -> false
      ) p then Fun_ctrl (c, p)
    else 
      Ctrl (c, List.map (function
        | A a -> a
        | F _ -> assert false
      ) p
      )
  with
  | Not_found -> Ctrl (s, [])

let (=) c0 c1 =
  (compare c0 c1) = 0
