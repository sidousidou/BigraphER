open Base

module S =
  S_opt
    (Set.Make (struct
      type t = int

      let compare = int_compare
    end))
    (struct
      type t = int

      let pp = Format.pp_print_int
    end)

type t = S.t

let pp =
  S.pp ~open_b:Format.pp_open_hbox
    ~first:(fun out -> Format.pp_print_string out "{")
    ~last:(fun out -> Format.pp_print_string out "}")
    ~sep:(fun out -> Format.pp_print_string out ",")

let to_string s =
  "{" ^ (List.map string_of_int (S.elements s) |> String.concat ",") ^ "}"

let add = S.add

let cardinal = S.cardinal

let compare = S.compare

let diff = S.diff

let elements = S.elements

let empty = S.empty

let equal = S.equal

let exists = S.exists

let filter = S.filter

let fold = S.fold

let for_all = S.for_all

let inter = S.inter

let is_empty = S.is_empty

let iter = S.iter

let max_elt = S.max_elt

let mem = S.mem

let min_elt = S.min_elt

let partition = S.partition

let remove = S.remove

let singleton = S.singleton

let subset = S.subset

let union = S.union

(* Given a non-negative integer i return ordinal i = {0,1,....,i-1} i.e. set
   with cardinality i. *)
let of_int i =
  assert (i >= 0);
  let rec loop i acc =
    match i with 0 -> acc | _ -> loop (i - 1) (add (i - 1) acc)
  in
  loop i empty

(* Transform an int list to an Int_set *)
let of_list = List.fold_left (fun acc e -> add e acc) empty

(* Add offset i to every element in set s. *)
let off i s = S.fold (fun x acc -> add (x + i) acc) s empty

let apply iso s =
  fold
    (fun i acc ->
      match Iso.apply iso i with None -> acc | Some i' -> add i' acc)
    s empty

(* Generates an isomorphism to fix the numbering of a set of int. [2;5;6;7]
   --> [(2,0),(5,1),(6,2),(7,3)] *)
let fix s =
  elements (of_int (cardinal s)) |> List.combine (elements s) |> Iso.of_list

let union_list = List.fold_left (fun acc s -> union s acc) empty

let rec augment s l =
  let l1, l2 = List.partition (fun s' -> is_empty (inter s s')) l in
  match l2 with [] -> (s, l1) | _ -> augment (union_list (s :: l2)) l1

(* Merge sets with common elements *)
let rec merge = function
  | [] -> []
  | s :: l' ->
      let s', l'' = augment s l' in
      s' :: merge l''

(* Check if the intersection of two sets is empty *)
let disjoint a b = is_empty (inter a b)

let iso_dom i = Iso.fold (fun i _ acc -> add i acc) i empty

let iso_codom i = Iso.fold (fun _ j acc -> add j acc) i empty
