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
