include Base.M_int

let dom iso =
  fst (List.split (bindings iso))

let codom iso = 
  snd (List.split (bindings iso))

let inverse iso =
  fold (fun i j iso' ->
      add j i iso')
    iso empty

exception NOT_BIJECTIVE

let add_exn i j iso = 
  if List.mem j (codom iso) then raise NOT_BIJECTIVE
  else add i j iso

let to_list = bindings

let of_list_exn =
  List.fold_left (fun acc (i, j) ->
      add_exn i j acc)
    empty

let to_string iso =
  "{"
  ^ (bindings iso
     |> List.map (fun (i, j) ->
         "("
         ^ (string_of_int i)
         ^ ", "
         ^ (string_of_int j)
         ^ ")")
     |> String.concat ", ")
  ^ "}"

let is_id = for_all Base.int_equal

let equal = equal Base.int_equal

let compare = compare Base.int_compare

let transform_exn iso i_dom i_codom =
  fold (fun i j iso' ->
      add_exn (find i i_dom) (find j i_codom) iso')
    iso empty

(* input:  i : P -> T  autos : P -> P *)
let gen_isos_exn i autos =
  List.map (fun a ->
      fold (fun i j iso' -> 
          add_exn (find i a) j iso')
        i empty)
    autos

let apply_exn iso i = find i iso

let apply iso i =
  try Some (apply_exn iso i) with
  | Not_found -> None

