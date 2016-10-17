type t = C of string * int

let to_string = function 
  | C (c, ar) -> c ^ ":" ^ (string_of_int ar)

let arity = function 
  | C (_, ar) -> ar

let name = function 
  | C (n, _) -> n

let compare (C (c0, ar0)) (C (c1, ar1)) =
  match String.compare c0 c1 with
  | 0 -> ar0 - ar1
  | x -> x 

let (=) c0 c1 =
  (compare c0 c1) = 0 

