let int_compare a b = a - b
let int_equal (a : int) (b : int) = a = b
 
include Map.Make (struct
    type t = int
    let compare = int_compare
  end)

let dom iso =
  fst (List.split (bindings iso))

let codom iso = 
  snd (List.split (bindings iso))

let inverse iso =
  fold (fun i j iso' ->
      add j i iso'
    ) iso empty

(* Must be a bijection: e.g. 1 -> 2 , 3 -> 2 is not allowed *)
exception NOT_BIJECTIVE

let add_exn i j iso = 
  if List.mem j (codom iso) then raise NOT_BIJECTIVE
  else add i j iso

let to_list = bindings

(* In case of clashing bindings only the right-most is stored. *)
(* raise NOT_BIJECTIVE *)
let of_list_exn =
  List.fold_left (fun acc (i, j) ->
      add_exn i j acc) empty

let to_string iso =
  Printf.sprintf "{%s}" 
    (String.concat ", " (List.map (fun (i, j) ->
         Printf.sprintf "(%d, %d)" i j
       ) (bindings iso)))

let is_id = for_all int_equal

let equal = equal int_equal

let compare = compare int_compare

(* Disjoint input isos are assumed *)
(* raise NOT_BIJECTIVE *)
let union_exn = fold add_exn  

(* Apply an iso to domain and codomain.
   raise: Not_found *)
let transform_exn iso i_dom i_codom =
  fold (fun i j iso' ->
      add_exn (find i i_dom) (find j i_codom) iso'
    ) iso empty

(* input:  i : P -> T  autos : P -> P *)
(* raise: Not_found *)
let gen_isos_exn i autos =
  List.map (fun a ->
      fold (fun i j iso' -> 
          add_exn (find i a) j iso'
        ) i empty
    ) autos

(* raise: Not_found *)
let find_exn = find

let find i iso =
  try Some (find_exn i iso)
  with
  | Not_found -> None
