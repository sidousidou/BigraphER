include Map.Make (struct
    type t = int
    let compare a b = a - b
  end)

let add i js r = 
  try
    add i (IntSet.union (find i r) js) r
  with
  | Not_found -> add i js r
                       
let dom r =
  fst (List.split (bindings r))

let codom r =
  fold (fun _ js acc ->
      IntSet.union js acc) r IntSet.empty
  
let is_iso = for_all (fun _ js -> IntSet.cardinal js = 1)

let equal = equal IntSet.equal

let compare = compare IntSet.compare

let to_string r =
  Printf.sprintf "{%s}"
    (String.concat ", " (List.map (fun (i, js) ->
         Printf.sprintf "(%d, %s)" i (IntSet.to_string js)
       ) (bindings r)))

let union = fold add
    
let inverse r =
  fold (fun i js acc ->
      IntSet.fold (fun j acc ->
          add j (IntSet.singleton i) acc) js acc
    ) r empty

let transform r i_dom i_codom =
  fold (fun i js r' ->
      add (Iso.find i i_dom) (IntSet.apply js i_codom) r'
    ) r empty
