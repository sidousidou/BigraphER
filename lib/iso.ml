include Map.Make (struct
    type t = int
    let compare a b = a - b
  end)
    
let inverse iso =
  fold (fun i j iso' ->
      add j i iso') iso empty

let dom iso =
  fst (List.split (bindings iso))

let codom iso = 
  snd (List.split (bindings iso))

let to_list = bindings

(* In case of clashing bindings only the right-most is stored. *)
let of_list l =
   List.fold_left (fun acc (i, j) ->
    add i j acc) empty l

let to_string iso =
  Printf.sprintf "{%s}" 
    (String.concat ", " (List.map (fun (i, j) ->
         Printf.sprintf "(%d, %d)" i j
       ) (bindings iso)))

let is_id = for_all (fun i j -> i = j)

let equal = equal (fun a b -> a = b)

let compare = compare (fun a b -> a - b)

(* Disjoint input isos are assumed *)
let union = fold add  

(* Apply an iso to domain and codomain. Replaces map in lib/big.ml 
   Raise: Not_found *)
let transform iso i_dom i_codom =
  fold (fun i j iso' ->
      add (find i i_dom) (find j i_codom) iso') iso empty

(* input:  i : P -> T  autos : P -> P *)
let gen_isos i autos =
  List.map (fun a ->
      fold (fun i j iso' -> 
          add (find i a) j iso') i empty
    ) autos
