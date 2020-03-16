(* type arg_type = *)
(*   [ `int *)
(*   | `float *)
(*   | `unknown ] *)

(* type types = arg_type list *)

include Map.Make (struct
    type t = Ctrl.ide
    let compare = Ctrl.ide_compare
  end)

let arity c s =
  try 
    Some (find c s)
  with
  | Not_found -> None

(* let string_of_type = function *)
(*   | `int -> "int" *)
(*   | `float -> "float" *)
(*   | `unknown -> "unknown" *)

(* let string_of_types l = *)
(*   "(" ^ (String.concat ", " (List.map string_of_type l)) ^ ")" *)
