include Set.Make (struct 
    type t = int
    let compare a b = a - b
  end)
    
let to_string s =
  Printf.sprintf "{%s}"
    (String.concat "," (List.map string_of_int (elements s)))

(* Transform an int list to an Int_set *)
let of_list =
  List.fold_left (fun acc e -> 
      add e acc) empty
          
(* given a non-nagative integer i return ordinal i = {0,1,....,i-1} i.e.   *)
(* set with cardinality i                                                  *)
let of_int i =
  assert (i >= 0);
  let rec fold i acc =
    match i with
    | 0 -> acc
    | _ -> fold (i - 1) (add (i - 1) acc) in
  fold i empty
    
(* add offset i to every element in set s *)
let off i s =
  fold (fun x acc -> 
      add (x + i) acc) s empty
    
(* Normalises a set of integers e.g. [2;5;7;8] -- > [0;1;2;3] *)
let norm s = 
  of_int (cardinal s)

let apply_exn s iso =
  assert (Iso.cardinal iso >= cardinal s);
  fold (fun i acc ->
      add (Iso.find_exn i iso) acc) s empty

(* Generates an isomorphism to fix the numbering of a set of int. 
     [2;5;6;7] --> [(2,0),(5,1),(6,2),(7,3)]                           *)
let fix s =
  try
    Iso.of_list_exn 
      (List.combine (elements s) (elements (of_int (cardinal s))))
  with
  | Iso.NOT_BIJECTIVE -> assert false

let union_list = 
  List.fold_left (fun acc s ->
      union s acc) empty

let rec augment s l =
  let (l1, l2) = 
    List.partition (fun s' -> 
        is_empty (inter s s')
      ) l in
  match l2 with
  | [] -> (s, l1)
  | _ -> augment (union_list (s :: l2)) l1

(* Merge sets with common elements *)
let rec merge l =
  match l with
  | [] -> []
  | s :: l' -> 
    let (s', l'') = augment s l' in
    s' :: (merge l'') 

(* Check if the intersection of two sets is empty *)
let disjoint a b =
  is_empty (inter a b)
