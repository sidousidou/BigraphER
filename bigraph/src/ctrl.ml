type param = I of int | F of float | S of String.t

type t = C of String.t * param list * int

(* name, parameters and arity *)

let string_of_param = function
  | I i -> string_of_int i
  | F f -> string_of_float f
  | S s -> s

(* Name and parameters *)
let long_name = function
  | C (c, [], _) -> c
  | C (c, ps, _) ->
      c ^ "(" ^ (List.map string_of_param ps |> String.concat ",") ^ ")"

let err = "Not a valid string representation of a control"

let parse_name s a =
  match Str.(split (regexp_string "(")) s with
  | [ n ] -> C (n, [], a)
  | [ n; args ] ->
      let ps =
        String.(sub args 0 (length args - 1))
        |> Str.(split (regexp_string ","))
        |> List.map (fun p ->
               try I (int_of_string p)
               with Failure _ -> (
                 try F (float_of_string p) with Failure _ -> S p ))
      in
      C (n, ps, a)
  | _ -> invalid_arg err

let arity = function C (_, _, ar) -> ar

(* Name, parameters and arity *)
let to_string c = long_name c ^ ":" ^ string_of_int (arity c)

let of_string s =
  match Str.(split (regexp_string ":")) s with
  | [ n; a ] -> parse_name n (int_of_string a)
  | _ -> invalid_arg err

(* Just the name of a control *)
let name = function C (n, _, _) -> n

let param_compare = function
  | I x, I y -> Base.int_compare x y
  | F x, F y -> compare x y
  | S x, S y -> String.compare x y
  | _ -> assert false

(* Only invoked when names are equal *)
let rec params_compare = function
  | [], [] -> 0
  | x :: xs, y :: ys -> (
      match param_compare (x, y) with
      | 0 -> params_compare (xs, ys)
      | res -> res )
  | _ -> assert false

let compare (C (c0, p0, _)) (C (c1, p1, _)) =
  match String.compare c0 c1 with 0 -> params_compare (p0, p1) | x -> x

let equal c0 c1 = compare c0 c1 = 0
