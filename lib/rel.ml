let int_compare a b = a - b

include Map.Make (struct
		     type t = int
		     let compare = int_compare
		   end)

let apply_exn r i = find i r

let apply r i =
  try apply_exn r i with
  | Not_found -> IntSet.empty

let add i js r = 
  add i (IntSet.union (apply r i) js) r
      
let dom r =
  fst (List.split (bindings r))

let codom r =
  IntSet.union_list (snd (List.split (bindings r)))

let is_fun = for_all (fun _ js -> IntSet.cardinal js = 1)

let equal = equal IntSet.equal

let compare = compare IntSet.compare

let to_string r =
  "{"
  ^ (bindings r
     |> List.map (fun (i, js) ->
		  "("
		  ^ (string_of_int i)
		  ^ ", "
		  ^ (IntSet.to_string js))
     |> String.concat ", " )
  ^ "}"
      
let to_list = bindings

let of_list =
  List.fold_left (fun acc (i, js) ->
		  add i (IntSet.of_list js) acc)
		 empty

let inverse r =
  fold (fun i js acc ->
	IntSet.fold (fun j acc ->
		     add j (IntSet.singleton i) acc) js acc)
       r empty
       
let transform_exn r i_dom i_codom =
  fold (fun i js r' ->
	add (Iso.find_exn i i_dom) (IntSet.apply_exn js i_codom) r')
       r empty
