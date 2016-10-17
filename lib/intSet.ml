include Set.Make (struct 
    type t = int
    let compare = Base.int_compare
  end)

let to_string s =
  "{"
  ^ (List.map string_of_int (elements s)
     |> String.concat ",")
  ^ "}"

(* Transform an int list to an Int_set *)
let of_list =
  List.fold_left (fun acc e -> 
      add e acc)
    empty

(* given a non-negative integer i return ordinal i = {0,1,....,i-1} i.e.   *)
(* set with cardinality i                                                  *)
let of_int i =
  assert (i >= 0);
  let rec loop i acc =
    match i with
    | 0 -> acc
    | _ -> loop (i - 1) (add (i - 1) acc) in
  loop i empty

(* add offset i to every element in set s *)
let off i s =
  fold (fun x acc -> 
      add (x + i) acc)
    s empty

let apply_exn s iso =
  assert (Iso.cardinal iso >= cardinal s);
  fold (fun i acc ->
      add (Iso.apply_exn iso i) acc)
    s empty

let filter_apply s iso =
  let s' = inter s (of_list (Iso.dom iso)) in
  if is_empty s' then empty
  else try apply_exn s' iso with
    | Not_found -> assert false (*BISECT-IGNORE*)

(* Generates an isomorphism to fix the numbering of a set of int. 
     [2;5;6;7] --> [(2,0),(5,1),(6,2),(7,3)]                       *)
let fix s =
  try
    elements (of_int (cardinal s))
    |> List.combine (elements s)
    |> Iso.of_list_exn
  with
  | Iso.NOT_BIJECTIVE -> assert false (*BISECT-IGNORE*)

let union_list = 
  List.fold_left (fun acc s ->
      union s acc)
    empty

let rec augment s l =
  let (l1, l2) = 
    List.partition (fun s' -> 
        is_empty (inter s s'))
      l in
  match l2 with
  | [] -> (s, l1)
  | _ -> augment (union_list (s :: l2)) l1

(* Merge sets with common elements *)
let rec merge = function
  | [] -> []
  | s :: l' -> 
    let (s', l'') = augment s l' in
    s' :: (merge l'') 

(* Check if the intersection of two sets is empty *)
let disjoint a b =
  is_empty (inter a b)
