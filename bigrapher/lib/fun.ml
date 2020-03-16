open Utils

include Map.Make (struct
    type t = int
    let compare = int_compare
  end)

let dom f =
  fst (List.split (bindings f))

let codom f = 
  snd (List.split (bindings f))

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
let transform_exn f i_dom i_codom =
  fold (fun i j f' ->
      add (Iso.find_exn i i_dom) (Iso.find_exn j i_codom) f'
    ) f empty

(* raise: Not_found *)
let find_exn = find

let find i f =
  try Some (find_exn i f)
  with
  | Not_found -> None

(* check if there is a binding for each 0 ... (n - 1) *)
let is_total n f =
  let rec aux i f =
    if i < 0 then true
    else match find i f with
      | Some _ -> aux (i - 1) f
      | None -> false in
  aux (n - 1) f
