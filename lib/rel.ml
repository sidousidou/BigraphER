open Base

type t = IntSet.t M_int.t

let cardinal = M_int.cardinal

let empty = M_int.empty

let fold = M_int.fold

let is_empty = M_int.is_empty

let iter = M_int.iter

let mem = M_int.mem

let equal = M_int.equal IntSet.equal

let compare = M_int.compare IntSet.compare

let dom r =
  M_int.bindings r
  |> List.split
  |> fst
  |> IntSet.of_list

let codom r =
  M_int.bindings r
  |> List.split
  |> snd
  |> IntSet.union_list

let apply r i =
  match M_int.find i r with
  | None -> IntSet.empty
  | Some xs -> xs

let add i js r =
  M_int.add i (IntSet.union (apply r i) js) r

let to_string r =
  "{"
  ^ (M_int.bindings r
     |> List.map (fun (i, js) ->
         "("
         ^ (string_of_int i)
         ^ ", "
         ^ (IntSet.to_string js))
     |> String.concat ", " )
  ^ "}"

let to_list = M_int.bindings

let of_list =
  List.fold_left (fun acc (i, js) ->
      add i (IntSet.of_list js) acc)
    empty
