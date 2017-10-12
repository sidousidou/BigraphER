let int_equal (a : int) (b : int) = a = b

let int_compare a b = a - b

module M_int =
  Map.Make (struct
    type t = int
    let compare a b = a - b
  end)

module M_string = Map.Make (String)

module S_string = Set.Make (String)

module H_int =
  Hashtbl.Make(struct
    type t = int
    let equal = int_equal
    let hash = Hashtbl.hash
  end)

module H_string =
  Hashtbl.Make(struct
    type t = string
    let equal (x:string) y = x = y
    let hash = Hashtbl.hash
  end)

let safe = function
  | Some v -> v
  | None -> assert false (*BISECT-IGNORE*)

let safe_exn f =
  try f with
  | _ -> assert false (*BISECT-IGNORE*)

let ints_compare (i0, p0) (i1, p1) =
  match i0 - i1 with
  | 0 -> p0 - p1
  | x -> x

let flip f x y = f y x

let flip2 f a b c = f a c b

(* "\n12\n4\n678\n" -> ["12"; "4"; "678"] 
   "01234\n" -> ["01234"] 
   "0123" -> ["0123"] *)
let parse_lines = Str.(split (regexp_string "\n"))

(* "{}" -> ""
   "[123]" -> "123" *)
let remove_block_delims s =
  if String.length s >= 2 then
    String.(sub s 1 (length s - 2))
  else invalid_arg "String \"" ^ s ^ "\" has no block delimiters"

module JSON = struct

  type json_node =
    | J_int of string * int
    | J_string of string * string
    | J_array of string * json_node list
    | J_record of string * json_node list
    | J_node of json_node list

  let wrap_string s =
    "\"" ^ s ^ "\""

  let rec to_string = function
    | J_int (id, x) -> (wrap_string id) ^ ": " ^ (string_of_int x)
    | J_string (id, s) -> (wrap_string id) ^ ": " ^ (wrap_string s)
    | J_array (id, l) -> (wrap_string id) ^ ": ["
                         ^ (List.map to_string l
                            |> String.concat ",\n" )
                         ^ "]"
    | J_record (id, l) -> (wrap_string id) ^ ": {"
                          ^ (List.map to_string l
                             |> String.concat ",\n" )
                          ^ "}"
    | J_node l -> "{"
                  ^ (List.map to_string l
                     |> String.concat ",\n" )
                  ^ "}"                  
  
end
