open Base

type t =
  { ctrl : Ctrl.t M_int.t;
    sort : IntSet.t M_string.t;
    size : int; }

let empty =
  { ctrl = M_int.empty;
    sort = M_string.empty;
    size = 0; }

let is_empty s = s.size = 0

let size s = s.size

let add_s name v m =
  match M_string.find name m with
  | Some vs -> M_string.add name (IntSet.add v vs) m
  | None -> M_string.add name (IntSet.singleton v) m

let add i c ns =
  assert (i >= 0);
  assert (i <= ns.size);
  { ctrl = M_int.add i c ns.ctrl;
    sort = add_s (Ctrl.name c) i ns.sort;
    size = ns.size + 1; }

let fold f s =
  M_int.fold f s.ctrl

let to_string s =
  "{"
  ^ (fold (fun i c acc ->
      acc @ ["("
             ^ (string_of_int i)
             ^ ", "
             ^ (Ctrl.to_string c)
             ^ ")"])
      s []
     |> String.concat ",")
  ^ "}"

let json_of_nodes_f s =
  let open JSON in
  fold (fun i c acc ->
      let n = J_node [ J_int ("node_id", i);
                       J_record ("control", [
                           J_string ("control_id", Ctrl.name c);
                           J_int ("control_arity", Ctrl.arity c)]) ] in
      n :: acc) s []
  |> (fun l -> J_array ("nodes", l))

let json_of_nodes s =
  JSON.to_string @@ json_of_nodes_f s

let string_of_sorts s =
  "{"
  ^ (M_string.fold (fun c vs acc ->
      acc @ ["("
             ^ c
             ^ ", "
             ^ (IntSet.to_string vs)
             ^ ")"])
      s.sort []
     |> String.concat ",")
  ^ "}"

let get_ctrl i s =
  assert (i >= 0);
  assert (i < s.size);
  M_int.find i s.ctrl

let find_all (Ctrl.C (n, _)) s =
  match M_string.find n s.sort with
  | None -> IntSet.empty
  | Some s -> s

let to_dot s =
  fold (fun i (Ctrl.C (n, _)) acc ->
      acc @ [Printf.sprintf "v%d [ label=\"%s\", shape=ellipse, id=\"v%d_%s\" \
                             fontname=\"sans-serif\", fontsize=9.0,\
                             fixedsize=true, width=%f, height=.30 ];"
               i n i n (0.1 *. (float (String.length n)) +. 0.2)])
    s []
  |>  String.concat "\n"

let tens a b =
  { ctrl = M_int.fold (fun v c acc ->
        M_int.add (v + a.size) c acc)
        b.ctrl a.ctrl;
    sort = M_string.merge (fun _ l r ->
        match (l, r) with
        | (Some l, Some r) ->
          Some (IntSet.union l (IntSet.off a.size r))
        | (Some l, None) -> Some l
        | (None, Some r) -> Some (IntSet.off a.size r)
        | (None, None) -> None)
        a.sort b.sort;
    size = a.size + b.size; }

let apply iso s =
  fold (fun i c acc ->
      match Iso.apply iso i with
      | Some i' ->  add i' c acc
      | None -> acc)
    s empty

let of_string s =
  let err =
    "Not a valid string representation of a node set" in
  Base.remove_block_delims s
  |> (function
      | "" -> ""
      | s -> Base.remove_block_delims s)
  |> Str.(split (regexp_string "),("))
  |> List.map (fun s ->
      match Str.(split (regexp_string ", ")) s with
      | i :: c :: [] ->
        (int_of_string i,
         match Str.(split (regexp_string ":")) c with
         | n :: a :: [] -> Ctrl.C (n, int_of_string a)
         | _ -> invalid_arg err)
      | _ -> invalid_arg err)
  |> List.fold_left (fun acc (i, c) -> add i c acc) empty

(* true when a contains a control that is not present in b *)
let not_sub a b =
  try
    M_string.fold (fun c _ res ->
        if M_string.mem c b.sort
        then res
        else raise Exit)
      a.sort false
  with
  | Exit -> true

let norm s =
  M_int.bindings s.ctrl
  |> List.map snd
  |> List.map Ctrl.name
  |> List.fast_sort String.compare

let equal a b =
  (norm a) = (norm b)
