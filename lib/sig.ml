include Map.Make (struct
    type t = Ctrl.ide
    let compare = Ctrl.ide_compare
  end)

let arity c s =
  try Some (find c s)
  with
  | Not_found -> None
