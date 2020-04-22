open Base

type t = int M_int.t

let add = M_int.add

let empty = M_int.empty

let equal = M_int.equal int_equal

let compare = M_int.compare int_compare

let fold = M_int.fold

let iter = M_int.iter

let to_list = M_int.bindings

let dom f = fold (fun i _ acc -> IntSet.add i acc) f IntSet.empty

let codom f = fold (fun _ j acc -> IntSet.add j acc) f IntSet.empty

let inverse f =
  fold (fun i j rel -> Rel.add j (IntSet.singleton i) rel) f Rel.empty

let of_list = List.fold_left (fun acc (i, j) -> M_int.add i j acc) empty

let parse l = of_list (List.mapi (fun i j -> (i, j)) l)

let pp =
  M_int.pp ~open_b:Format.pp_open_hbox
    ~first:(fun out -> Format.pp_print_string out "{")
    ~last:(fun out -> Format.pp_print_string out "}")
    ~sep:(fun out ->
      Format.pp_print_string out ",";
      Format.pp_print_space out ())
    Format.pp_print_int

let to_string f =
  "{"
  ^ ( to_list f
    |> List.map (fun (i, j) ->
           "(" ^ string_of_int i ^ ", " ^ string_of_int j ^ ")")
    |> String.concat ", " )
  ^ "}"

let transform ~iso_dom ~iso_codom f =
  fold
    (fun i j f' ->
      match (Iso.apply iso_dom i, Iso.apply iso_codom j) with
      | Some i', Some j' -> M_int.add i' j' f'
      | _ -> f')
    f empty

let apply f i = M_int.find i f

(* Check if there is a binding for each 0 ... (n - 1) *)
let is_total n f =
  assert (n >= 0);
  let rec aux i f =
    if i < 0 then true else if M_int.mem i f then aux (i - 1) f else false
  in
  aux (n - 1) f

(* Check if f is surjective *)
let is_surj n f =
  assert (n >= 0);
  IntSet.equal (codom f) (IntSet.of_int n)

let is_id = M_int.for_all (fun i j -> i = j)

let check_codom n f =
  assert (n >= 0);
  IntSet.subset (codom f) (IntSet.of_int n)
