let int_equal (a : int) (b : int) = a = b

let int_compare a b = a - b

(* Functor overriding values in module Map. Required to be compatible with
    OCaml < 4.05 *)
module M_opt (M_lib : Map.S) = struct

  include M_lib

  let find k m =
    try Some (find k m) with
    | Not_found -> None

  let choose m =
    try Some (choose m) with
    | Not_found -> None

  let min_binding m =
    try Some (min_binding m) with
    | Not_found -> None

  let max_binding m =
    try Some (max_binding m) with
    | Not_found -> None

end

module S_opt (S_lib : Set.S) = struct

  include S_lib

  let find x s =
    try Some (find x s) with
    | Not_found -> None

  let min_elt s =
    try Some (min_elt s) with
    | Not_found -> None

  let max_elt s =
    try Some (max_elt s) with
    | Not_found -> None

  let choose s =
    try Some (choose s) with
    | Not_found -> None

end

module M_int = M_opt (Map.Make (struct
                        type t = int
                        let compare = int_compare
                      end))

module M_string = M_opt (Map.Make (String))

module S_string = S_opt (Set.Make (String))
  
module H_int = struct

  include  Hashtbl.Make(struct
      type t = int
      let equal = int_equal
      let hash = Hashtbl.hash
    end)

  let find h x =
    try Some (find h x) with
    | Not_found -> None
      
end

module H_string = struct

  include Hashtbl.Make(struct
      type t = string
      let equal (x:string) y = x = y
      let hash = Hashtbl.hash
    end)
      
  let find h x =
    try Some (find h x) with
    | Not_found -> None

end

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
    | J_float of string * float
    | J_string of string * string
    | J_array of string * json_node list
    | J_record of string * json_node list
    | J_node of json_node list

  let wrap_string s =
    "\"" ^ s ^ "\""

  let rec to_string = function
    | J_int (id, x) -> (wrap_string id) ^ ": " ^ (string_of_int x)
    | J_float (id, x) -> (wrap_string id) ^ ": " ^ (string_of_float x)
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
