open Base

type t = int M_int.t * int M_int.t

let empty = (M_int.empty, M_int.empty)

let is_empty (i, _) = M_int.is_empty i

let mem v (i, _) = M_int.mem v i

let iter f (i, _) = M_int.iter f i

let fold f (i, _) = M_int.fold f i

let cardinal (i, _) = M_int.cardinal i

(* Inverse of a bijection is a bijection *)
let inverse (i, i') = (i', i)

(* Add a binding only if the result is still a bijection *)
let add x y (i, i') =
  if M_int.mem x i || M_int.mem y i' then (i, i')
  else (M_int.add x y i, M_int.add y x i')

let to_list (i, _) = M_int.bindings i

let of_list = List.fold_left (fun acc (i, j) -> add i j acc) empty

let pp out (i, _) =
  M_int.pp ~open_b:Format.pp_open_hbox
    ~first:(fun out -> Format.pp_print_string out "{")
    ~last:(fun out -> Format.pp_print_string out "}")
    ~sep:(fun out ->
      Format.pp_print_string out ",";
      Format.pp_print_space out ())
    Format.pp_print_int out i

let to_string (i, _) =
  "{"
  ^ ( M_int.bindings i
    |> List.map (fun (i, j) ->
           "(" ^ string_of_int i ^ ", " ^ string_of_int j ^ ")")
    |> String.concat ", " )
  ^ "}"

let is_id (i, _) = M_int.for_all int_equal i

let equal (i, _) (i', _) = M_int.equal int_equal i i'

let compare (i, _) (i', _) = M_int.compare int_compare i i'

let apply (i, _) x = M_int.find x i

let transform ~iso_dom ~iso_codom iso =
  fold
    (fun i j iso' ->
      match (apply iso_dom i, apply iso_codom j) with
      | Some i', Some j' -> add i' j' iso'
      | _ -> iso')
    iso empty

(* input: i : P -> T *)
let gen_isos i =
  List.rev_map (fun auto ->
      fold
        (fun i j iso' ->
          match apply auto i with None -> iso' | Some i' -> add i' j iso')
        i empty)

let equal_up_to i i' autos = gen_isos i' autos |> List.exists (equal i)
