(* Commander variable encoding *)

module Make (S : sig
  type lit

  type var

  type solver

  val add_clause : solver -> lit list -> unit

  val negate : lit -> lit

  val new_var : solver -> var

  val positive_lit : var -> lit
end) =
struct
  type group = S.lit list

  type cmd_tree = Leaf of group | Node of (S.lit * cmd_tree) list

  (* Parameters t and g are used for configure the commander-variable
     encoding *)
  type params = { t : int; g : int }

  let defaults = { t = 6; g = 3 }

  (* Disjunctions (all possible pairs) of negative literals *)
  let add_at_most_naive s l =
    (* Boolean encoding of at most one TRUE. Most common cases are hard-coded *)
    let rec _at_most acc = function
      | [] | [ _ ] -> acc
      | [ a; b ] -> (a, b) :: acc
      | [ a; b; c ] -> (a, b) :: (a, c) :: (b, c) :: acc
      | [ a; b; c; d ] ->
          (a, b) :: (a, c) :: (a, d) :: (b, c) :: (b, d) :: (c, d) :: acc
      | [ a; b; c; d; e ] ->
          (a, b) :: (a, c) :: (a, d) :: (a, e) :: (b, c) :: (b, d) :: (b, e)
          :: (c, d) :: (c, e) :: (d, e) :: acc
      | [ a; b; c; d; e; f ] ->
          (a, b) :: (a, c) :: (a, d) :: (a, e) :: (a, f) :: (b, c) :: (b, d)
          :: (b, e) :: (b, f) :: (c, d) :: (c, e) :: (c, f) :: (d, e)
          :: (d, f) :: (e, f) :: acc
      | x :: rest ->
          _at_most
            (List.rev_append (List.rev_map (fun y -> (x, y)) rest) acc)
            rest
    in
    List.iter
      (fun (a, b) -> S.add_clause s [ S.negate a; S.negate b ])
      (_at_most [] l)

  (* Return a list of groups of size at most g + 1. If [0;1;2] [3] then [0;1]
     [2;3] to avoid singletons. Also if [0;1] [2] then [0;1;2] *)
  let group l n g =
    assert (g > 1);
    assert (n >= g);
    if List.length l <= n then None
    else
      let x, i, res =
        List.fold_left
          (fun (group, i, res) x ->
            if i >= g then ([ x ], 1, group :: res)
            else (x :: group, i + 1, res))
          ([], 0, []) l
      in
      if i = 1 then
        if g > 2 then
          match res with
          | (v :: vs) :: res -> Some ((x @ [ v ]) :: vs :: res)
          | _ -> assert false
        else
          match res with
          | v :: res -> Some ((x @ v) :: res)
          | _ -> assert false
      else Some (x :: res)

  (* Build a tree of commander variables. Input is a tree, output split the
     root and add a level of variables. *)
  let cmd_init l p s =
    let rec _cmd_init n g t =
      match t with
      | Node cmd_l -> (
          match group cmd_l n g with
          | Some cmd_l' ->
              Node
                (List.fold_left
                   (fun acc g ->
                     (S.positive_lit (S.new_var s), Node g) :: acc)
                   [] cmd_l')
              |> _cmd_init n g
          (* Do not add an additional level of commander variables *)
          | None -> t )
      | Leaf vars -> (
          match group vars n g with
          | Some cmd_l ->
              Node
                (List.fold_left
                   (fun acc g ->
                     (S.positive_lit (S.new_var s), Leaf g) :: acc)
                   [] cmd_l)
              |> _cmd_init n g
          (* Do not add an additional level of commander variables *)
          | None -> t )
    in
    _cmd_init p.t p.g (Leaf l)

  (* Scan the tree and add constraints:
   *  1. at most one TRUE in every group
   *  2. if commander variable is TRUE then at least one TRUE in its group
   *  3. if commander variable is FALSE then no TRUE in its group 4. exactly
   *     one commander variable is true. *)

  (* [X0, X1, X2] -> [(!X0 or !X1), (!X0 or !X2), (!X1 or !X2)] *)
  let rec add_cmd_c1 s = function
    | Leaf g -> add_at_most_naive s g
    | Node cmd_g ->
        let cmd_vars, sub = List.split cmd_g in
        add_at_most_naive s cmd_vars;
        List.iter (fun t -> add_cmd_c1 s t) sub

  (* (C, [X0, X1, X2]) -> [!C or X0 or X1 or X2] *)
  let add_cmd_c2 s t =
    let rec aux = function
      | Leaf g -> g
      | Node cmd_g ->
          List.iter
            (fun (cmd_v, sub) -> S.add_clause s (S.negate cmd_v :: aux sub))
            cmd_g;
          Base.list_rev_split_left cmd_g
    in
    aux t |> ignore

  (* (C, [X0, X1, X2]) -> [(C or !X0), (C or !X1), (C or !X2)] *)
  let add_cmd_c3 s t =
    let rec aux = function
      | Leaf g -> g
      | Node cmd_g ->
          List.iter
            (fun (cmd_v, sub) ->
              aux sub
              |> List.iter (fun l -> S.add_clause s [ cmd_v; S.negate l ]))
            cmd_g;
          Base.list_rev_split_left cmd_g
    in
    aux t |> ignore

  let add_at_least_cmd s = function
    | Leaf g -> S.add_clause s g
    | Node cmd_g -> S.add_clause s (Base.list_rev_split_left cmd_g)
end
