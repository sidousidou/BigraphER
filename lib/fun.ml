include Base.M_int

let dom f =
  fold (fun i _ acc ->
      IntSet.add i acc)
    f IntSet.empty

let codom f = 
  fold (fun _ j acc ->
      IntSet.add j acc)
    f IntSet.empty

let inverse f =
  fold (fun i j rel ->
      Rel.add j (IntSet.singleton i) rel)
    f Rel.empty

let to_list = bindings

let of_list =
  List.fold_left (fun acc (i, j) ->
      add i j acc) empty

let parse l = of_list (List.mapi (fun i j -> (i, j)) l)

let to_string f =
  "{"
  ^ (bindings f
     |> List.map (fun (i, j) ->
         "(" ^ (string_of_int i) ^ ", " ^ (string_of_int j) ^ ")")
     |> String.concat ", " )
  ^ "}"

let equal = equal Base.int_equal

let compare = compare Base.int_compare

let transform_exn f i_dom i_codom =
  fold (fun i j f' ->
      add (Iso.apply_exn i_dom i) (Iso.apply_exn i_codom j) f')
    f empty

let apply_exn f i = find i f

let apply f i =
  try Some (apply_exn f i) with
  | Not_found -> None

(* Check if there is a binding for each 0 ... (n - 1) *)
let is_total n f =
  assert (n >= 0);
  let rec aux i f =
    if i < 0 then true
    else match apply f i with
      | Some _ -> aux (i - 1) f
      | None -> false in
  aux (n - 1) f

let check_codom min max f =
  assert (max >= min);
  let c = codom f in
  if IntSet.is_empty c then true
  else (IntSet.min_elt c >= min) && (IntSet.max_elt c <= max)
