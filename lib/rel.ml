include Map.Make (struct
    type t = int
    let compare a b = a - b
  end)
    
let dom r =
  fst (List.split (bindings r))

let codom r =
  fold (fun i js acc ->
      IntSet.union js acc) r IntSet.empty
  
let is_iso = for_all (fun _ js -> IntSet.cardinal js = 1)

let equal = equal IntSet.equal

let compare = compare IntSet.compare

let to_string r =
  Printf.sprintf "{%s}"
    (String.concat ", " (List.map (fun (i, js) ->
         Printf.sprintf "(%d, %s)" i (IntSet.to_string js)
       ) (bindings r)))

let union = fold (fun i js acc ->
    try
      add i (IntSet.union (find i acc) js) acc
    with
    | Not_found -> add i js acc)
