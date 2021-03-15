open Base

type t = {
  ctrl : Ctrl.t M_int.t;
  (* Map from node ids to controls *)
  sort : IntSet.t M_string.t;
  (* Map from node names to node ids *)
  size : int;
}

let empty = { ctrl = M_int.empty; sort = M_string.empty; size = 0 }

let is_empty s = s.size = 0

let size s = s.size

let add_s name v m =
  match M_string.find name m with
  | Some vs -> M_string.add name (IntSet.add v vs) m
  | None -> M_string.add name (IntSet.singleton v) m

let add i c ns =
  assert (i >= 0);
  assert (i <= ns.size);
  {
    ctrl = M_int.add i c ns.ctrl;
    sort = add_s (Ctrl.name c) i ns.sort;
    size = ns.size + 1;
  }

let fold f s = M_int.fold f s.ctrl

let iter f s = M_int.iter f s.ctrl

let pp out s =
  M_int.pp ~open_b:Format.pp_open_hbox
    ~first:(fun out -> Format.pp_print_string out "{")
    ~last:(fun out -> Format.pp_print_string out "}")
    ~sep:(fun out ->
      Format.pp_print_string out ",";
      Format.pp_print_space out ())
    (fun out c -> Format.pp_print_string out (Ctrl.to_string c))
    out s.ctrl

let to_string s =
  "{"
  ^ ( fold
        (fun i c acc ->
          acc @ [ "(" ^ string_of_int i ^ ", " ^ Ctrl.to_string c ^ ")" ])
        s []
    |> String.concat "," )
  ^ "}"

let controls s =
  M_int.fold (fun _ c acc -> c :: acc) s.ctrl []
  |> List.fast_sort Ctrl.compare

let get_ctrl i s =
  assert (i >= 0);
  assert (i < s.size);
  M_int.find i s.ctrl

let find_all_sort c s =
  match M_string.find (Ctrl.name c) s.sort with
  | None -> IntSet.empty
  | Some s -> s

let find_all c s =
  find_all_sort c s |> IntSet.filter (fun i -> Base.safe @@ get_ctrl i s |> Ctrl.equal c)

let to_dot s =
  let escape_quotes s =
    let r = Str.regexp "\"" in Str.global_replace r "" s
  in
  fold
    (fun i c acc ->
      let n = escape_quotes (Ctrl.long_name c) in
      acc
      @ [
          Printf.sprintf
            "v%d [ label=\"%s\", shape=ellipse, id=\"v%d_%s\" \
             fontname=\"sans-serif\", fontsize=9.0,fixedsize=true, \
             width=%f, height=.30 ];"
            i n i n
            ((0.1 *. float (String.length n)) +. 0.2);
        ])
    s []
  |> String.concat "\n"

let tens a b =
  {
    ctrl =
      M_int.fold (fun v c acc -> M_int.add (v + a.size) c acc) b.ctrl a.ctrl;
    sort =
      M_string.merge
        (fun _ l r ->
          match (l, r) with
          | Some l, Some r -> Some (IntSet.union l (IntSet.off a.size r))
          | Some l, None -> Some l
          | None, Some r -> Some (IntSet.off a.size r)
          | None, None -> None)
        a.sort b.sort;
    size = a.size + b.size;
  }

let apply iso s =
  fold
    (fun i c acc ->
      match Iso.apply iso i with Some i' -> add i' c acc | None -> acc)
    s empty

let of_string s =
  let err = "Not a valid string representation of a node set" in
  Base.remove_block_delims s
  |> (function "" -> "" | s -> Base.remove_block_delims s)
  |> Str.(split (regexp_string "),("))
  |> List.map (fun s ->
         match Str.(split (regexp_string ", ")) s with
         | [ i; c ] -> (int_of_string i, Ctrl.of_string c)
         | _ -> invalid_arg err)
  |> List.fold_left (fun acc (i, c) -> add i c acc) empty

(* Set of controls *)
module S_ctrl = Set.Make (struct
  type t = Ctrl.t

  let compare = Ctrl.compare
end)

(* True when a contains a control that is not present in b *)
let not_sub a b =
  let set_ctrl_a =
    M_int.fold (fun _ c res -> S_ctrl.add c res) a.ctrl S_ctrl.empty
  in
  try
    S_ctrl.fold
      (fun c res ->
        if M_int.exists (fun _ c' -> Ctrl.equal c c') b.ctrl then res
        else raise_notrace Exit)
      set_ctrl_a false
  with Exit -> true

let norm s = List.map Ctrl.long_name (controls s)

let equal a b = Base.list_equal Ctrl.equal (controls a) (controls b)
