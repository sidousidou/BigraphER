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

let inverse f =
  fold (fun i j rel ->
      Rel.add j (IntSet.singleton i) rel
    ) f Rel.empty

let to_list = bindings

(* In case of clashing bindings only the right-most is stored. *)
let of_list l =
  List.fold_left (fun acc (i, j) ->
      add i j acc) empty l

let to_string f =
  Printf.sprintf "{%s}" 
    (String.concat ", " (List.map (fun (i, j) ->
         Printf.sprintf "(%d, %d)" i j
       ) (bindings f)))

let equal = equal int_equal

let compare = compare int_compare

(* Disjoint input isos are assumed *)
let union = fold add  

(* Apply an iso to domain and codomain.
   raise: Not_found *)
let transform_exp f i_dom i_codom =
  fold (fun i j f' ->
      add (Iso.find_exp i i_dom) (Iso.find_exp j i_codom) f'
    ) f empty

(* raise: Not_found *)
let find_exp = find

let find i f =
  try Some (find_exp i f)
  with
  | Not_found -> None





