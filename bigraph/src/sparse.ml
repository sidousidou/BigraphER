open Base

type t = {
  r : int;
  c : int;
  r_major : IntSet.t M_int.t;
  (* Row-major order: i -> j,...,j' *)
  c_major : IntSet.t M_int.t; (* Column-major order: j -> i,...,i' *)
}

(* Create an empty matrix *)
let make r c =
  assert (r >= 0);
  assert (c >= 0);
  { r; c; r_major = M_int.empty; c_major = M_int.empty }

let equal a b =
  a.r = b.r && a.c = b.c && M_int.equal IntSet.equal a.r_major b.r_major

let compare a b =
  match a.r - b.r with
  | 0 -> (
      match a.c - b.c with
      | 0 -> (
          match M_int.compare IntSet.compare a.r_major b.r_major with
          | 0 -> M_int.compare IntSet.compare a.c_major b.c_major
          | x -> x )
      | x -> x )
  | x -> x

let _init_array m =
  let buff = Array.make_matrix m.r m.c "0" in
  M_int.iter
    (fun i js -> IntSet.iter (fun j -> buff.(i).(j) <- "1") js)
    m.r_major;
  buff

let to_string m =
  _init_array m
  |> Array.map (fun r -> String.concat "" (Array.to_list r))
  |> Array.to_list |> String.concat "\n"

let pp out m =
  let open Format in
  let buff = _init_array m
  and pp_ints out a =
    pp_open_hbox out ();
    Array.iter (fun i -> pp_print_string out i) a;
    pp_close_box out ()
  in
  pp_open_vbox out 0;
  Array.iteri
    (fun i r ->
      if i = m.r - 1 then pp_ints out r
      else (
        pp_ints out r;
        pp_print_cut out () ))
    buff;
  pp_close_box out ()

let add_m i j m =
  match M_int.find i m with
  | Some s -> IntSet.add j s |> flip2 M_int.add i m
  | None -> M_int.add i (IntSet.singleton j) m

let add i j m =
  assert (i >= 0);
  assert (j >= 0);
  assert (i < m.r);
  assert (j < m.c);
  { m with r_major = add_m i j m.r_major; c_major = add_m j i m.c_major }

let flip_major m =
  M_int.fold
    (fun i js acc -> IntSet.fold (fun j acc -> add_m j i acc) js acc)
    m M_int.empty

let off o_r o_c m =
  M_int.fold
    (fun i js acc -> M_int.add (i + o_r) (IntSet.off o_c js) acc)
    m M_int.empty

let merge_f _ l r =
  match (l, r) with
  | Some s, Some s' -> Some (IntSet.union s s')
  | Some s, None | None, Some s -> Some s
  | None, None -> None

let row n =
  assert (n >= 0);
  IntSet.of_int n |> flip2 IntSet.fold (fun j acc -> add 0 j acc) (make 1 n)

let col n =
  assert (n >= 0);
  IntSet.of_int n |> flip2 IntSet.fold (fun i acc -> add i 0 acc) (make n 1)

let diag n =
  assert (n >= 0);
  IntSet.of_int n |> flip2 IntSet.fold (fun i acc -> add i i acc) (make n n)

let tens a b =
  {
    r = a.r + b.r;
    c = a.c + b.c;
    r_major = M_int.merge merge_f a.r_major (off a.r a.c b.r_major);
    c_major = M_int.merge merge_f a.c_major (off a.c a.r b.c_major);
  }

let append (a : t) (b : t) =
  assert (a.r = b.r);
  {
    r = a.r;
    c = a.c + b.c;
    r_major = M_int.merge merge_f a.r_major (off 0 a.c b.r_major);
    c_major = M_int.merge merge_f a.c_major (off a.c 0 b.c_major);
  }

let stack a b =
  assert (a.c = b.c);
  {
    r = a.r + b.r;
    c = a.c;
    r_major = M_int.merge merge_f a.r_major (off a.r 0 b.r_major);
    c_major = M_int.merge merge_f a.c_major (off 0 a.r b.c_major);
  }

let apply_rows iso m =
  assert (Iso.cardinal iso = m.r);
  let r_major, c_major =
    M_int.fold
      (fun i js (acc_r, acc_c) ->
        match Iso.apply iso i with
        | Some i' ->
            ( M_int.add i' js acc_r,
              IntSet.fold
                (fun j acc -> M_int.add j (IntSet.singleton i') acc)
                js acc_c )
        | None -> (acc_r, acc_c))
      m.r_major
      (M_int.empty, M_int.empty)
  in
  { m with r_major; c_major }

let apply_cols iso m =
  assert (Iso.cardinal iso = m.c);
  let r_major, c_major =
    M_int.fold
      (fun j is (acc_r, acc_c) ->
        match Iso.apply iso j with
        | Some j' ->
            ( IntSet.fold
                (fun i acc -> M_int.add i (IntSet.singleton j') acc)
                is acc_r,
              M_int.add j' is acc_c )
        | None -> (acc_r, acc_c))
      m.c_major
      (M_int.empty, M_int.empty)
  in
  { m with r_major; c_major }

let apply iso m =
  assert (Iso.cardinal iso = m.r);
  assert (m.r = m.c);
  apply_rows iso m |> apply_cols iso

let parse_vectors adj rows =
  assert (rows >= 0);
  List.fold_left
    (fun (j, acc) i_list ->
      (j + 1, List.fold_left (fun acc i -> add i j acc) acc i_list))
    (0, make rows (List.length adj))
    adj
  |> snd

let chl m i =
  assert (i >= 0);
  assert (i < m.r);
  match M_int.find i m.r_major with None -> IntSet.empty | Some s -> s

let mem m i j = chl m i |> IntSet.mem j

let prn m j =
  assert (j >= 0);
  assert (j < m.c);
  match M_int.find j m.c_major with None -> IntSet.empty | Some s -> s

let mul a b =
  assert (a.c = b.r);
  let m = make a.r b.c in
  M_int.fold
    (fun i js acc ->
      M_int.fold
        (fun j is acc ->
          if IntSet.is_empty (IntSet.inter js is) then acc else add i j acc)
        b.c_major acc)
    a.r_major m

let sum a b =
  assert (a.r = b.r);
  assert (a.c = b.c);
  M_int.fold
    (fun i js acc -> IntSet.fold (fun j acc -> add i j acc) js acc)
    b.r_major a

let trans m0 =
  let rec fix m acc =
    let m' = mul m0 m in
    if equal m m' then acc else fix m' (sum m' acc)
  in
  fix m0 m0

let dom m =
  M_int.fold (fun i _ acc -> IntSet.add i acc) m.r_major IntSet.empty

let codom m =
  M_int.fold (fun j _ acc -> IntSet.add j acc) m.c_major IntSet.empty

let leaves m = IntSet.diff (IntSet.of_int m.r) (dom m)

let orphans m = IntSet.diff (IntSet.of_int m.c) (codom m)

let siblings m j =
  IntSet.fold
    (fun i acc -> IntSet.union acc (chl m i))
    (prn m j) IntSet.empty
  |> IntSet.remove j

let partners m i =
  IntSet.fold
    (fun j acc -> IntSet.union acc (prn m j))
    (chl m i) IntSet.empty
  |> IntSet.remove i

let iter f m =
  M_int.iter (fun i js -> IntSet.iter (fun j -> f i j) js) m.r_major

let fold f m acc =
  M_int.fold
    (fun i js acc -> IntSet.fold (fun j acc -> f i j acc) js acc)
    m.r_major acc

let edges m =
  fold (fun i j acc -> (i, j) :: acc) m []
  |> List.fast_sort Base.ints_compare

let fold_r f m = M_int.fold f m.r_major

let fold_c f m = M_int.fold f m.c_major

let add_list m = List.fold_left (fun acc (i, j) -> add i j acc) m

let entries m =
  M_int.fold (fun _ js acc -> acc + IntSet.cardinal js) m.r_major 0

let levels m =
  (* m is a graph *)
  assert (m.r = m.c);
  let rec fix acc nodes res =
    (* find nodes with all children in acc *)
    let leaves =
      IntSet.filter (fun i -> IntSet.subset (chl m i) acc) nodes
    in
    if IntSet.is_empty leaves then res
    else
      fix (IntSet.union acc leaves) (IntSet.diff nodes leaves) (leaves :: res)
  in
  fix IntSet.empty (IntSet.of_int m.r) []

let aux_split r m =
  let t, mi, b = M_int.split r m in
  match mi with Some js -> (t, M_int.add r js b) | None -> (t, b)

(* Dual of stack *)
let split_rows r m =
  assert (r >= 0);
  assert (r <= m.r);
  let top, b = aux_split r m.r_major in
  let bottom = off (-r) 0 b in
  ( { r; c = m.c; r_major = top; c_major = flip_major top },
    { r = m.r - r; c = m.c; r_major = bottom; c_major = flip_major bottom }
  )

(* Dual of append *)
let split_columns c m =
  assert (c >= 0);
  assert (c <= m.c);
  let left, right' = aux_split c m.c_major in
  let right = off (-c) 0 right' in
  ( { r = m.r; c; r_major = flip_major left; c_major = left },
    { r = m.r; c = m.c - c; r_major = flip_major right; c_major = right } )

let split r c m =
  let t, b = split_rows r m in
  (split_columns c t, split_columns c b)

(* Dual of split *)
let glue rn rs nn ns = stack (append rn rs) (append nn ns)

let parse_string r n s rows =
  assert (r >= 0);
  assert (n >= 0);
  assert (s >= 0);
  assert (List.length rows = r + n);
  assert (List.for_all (fun l -> String.length l = n + s) rows);
  let to_int s =
    let rec aux i acc =
      if i < 0 then acc
      else
        match s.[i] with
        | '1' -> aux (i - 1) (IntSet.add i acc)
        | '0' -> aux (i - 1) acc
        | _ -> invalid_arg "Values must either be '0' or '1'"
    in
    aux (String.length s - 1) IntSet.empty
  in
  let _, r_major =
    List.fold_left
      (fun (i, acc) r -> (i + 1, M_int.add i (to_int r) acc))
      (0, M_int.empty) rows
  in
  { r = r + n; c = n + s; r_major; c_major = flip_major r_major }
  |> split r n

(* Compute a set of rows such that the union of their children set is equal
   to js. *)
let aux_eq m js =
  let rec aux (res_i, res_j) = function
    | [] -> if IntSet.equal js res_j then res_i else IntSet.empty
    | (i, js') :: xs ->
        let res_i', res_j' = (IntSet.add i res_i, IntSet.union res_j js') in
        if IntSet.equal js res_j' then res_i' else aux (res_i', res_j') xs
  in
  let m' = M_int.filter (fun _ js' -> IntSet.subset js' js) m in
  if M_int.is_empty m' then IntSet.empty
  else
    match
      M_int.filter (fun _ js' -> IntSet.equal js' js) m' |> M_int.choose
    with
    | Some (i, _) -> IntSet.singleton i
    | None -> aux (IntSet.empty, IntSet.empty) (M_int.bindings m')

let row_eq m = aux_eq m.r_major

let col_eq m = aux_eq m.c_major

(* Symmetric closure (for square matrices) *)
let sym m =
  assert (m.r = m.c);
  edges m |> List.rev_map (fun (v, u) -> (u, v)) |> add_list m

let descendants m i =
  assert (i >= 0);
  assert (i < m.r);
  let rec dfs stack visited =
    match stack with
    | [] -> visited
    | i :: stack ->
        let js = chl m i in
        dfs (IntSet.elements js @ stack) (IntSet.union visited js)
  in
  dfs [ i ] (IntSet.of_int i)

(* Connected components for (undirected graphs) *)
let connected_comps m =
  assert (m.r = m.c);
  let rec aux v res =
    match IntSet.min_elt v with
    | None -> res
    | Some i ->
        let v' = descendants m i in
        aux (IntSet.diff v v') (v' :: res)
  in
  aux (IntSet.of_int m.r) [] |> List.rev
