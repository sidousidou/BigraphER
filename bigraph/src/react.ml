module type R = sig
  type t

  type ac

  type label

  val name : t -> string

  val lhs : t -> Big.t

  val rhs : t -> Big.t

  val conds : t -> ac list

  val l : t -> label

  val equal : t -> t -> bool

  val map : t -> Fun.t option

  val merge_occ :
    Big.t * label * t list ->
    Big.t * label * t list ->
    Big.t * label * t list

  val val_chk : t -> bool

  val val_chk_error_msg : string

  val string_of_label : label -> string

  val make :
    name:string ->
    lhs:Big.t ->
    rhs:Big.t ->
    ?conds:ac list ->
    label ->
    Fun.t option ->
    t

  val step_post :
    (Big.t * label * t list) list * int ->
    (Big.t * label * t list) list * int

  val random_step_post :
    (Big.t * label * t list) list * int ->
    (Big.t * label * t list) option * int
end

module type T = sig
  type t

  type ac

  type label

  type react_error

  exception NOT_VALID of react_error

  val name : t -> string

  val lhs : t -> Big.t

  val rhs : t -> Big.t

  val conds : t -> ac list

  val l : t -> label

  val equal : t -> t -> bool

  val map : t -> Fun.t option

  val make :
    name:string ->
    lhs:Big.t ->
    rhs:Big.t ->
    ?conds:ac list ->
    label ->
    Fun.t option ->
    t

  val to_string : t -> string

  val is_valid : t -> bool

  val is_valid_exn : t -> bool

  val string_of_react_err : react_error -> string

  val is_enabled : Big.t -> t -> bool

  val filter_iso :
    (Big.t * label * t list) list -> (Big.t * label * t list) list

  val apply : Big.t -> t list -> Big.t option

  val fix : Big.t -> t list -> Big.t * int

  val step : Big.t -> t list -> (Big.t * label * t list) list * int

  val random_step : Big.t -> t list -> (Big.t * label * t list) option * int

  module Memo : sig
    val is_enabled : Big.t -> Sparse.t -> t -> bool

    val fix : Big.t -> Sparse.t -> t list -> Big.t * int

    val step :
      Big.t ->
      Sparse.t ->
      (t * (Iso.t * Iso.t) list) list ->
      (Big.t * label * t list) list * int

    val random_step :
      Big.t ->
      Sparse.t ->
      (t * (Iso.t * Iso.t) list) list ->
      (Big.t * label * t list) option * int
  end
end

module Make (S : Solver.M) (AC : AppCond.C) (R : R with type ac = AppCond.t) :
  T with type t = R.t and type label = R.label and type ac = R.ac = struct
  include R

  type react_error =
    | Inter_eq_i of Big.inter * Big.inter
    | Inter_eq_o of Big.inter * Big.inter
    | Lhs_nodes
    | Lhs_solid
    | Map_chk
    | Val_chk

  exception NOT_VALID of react_error

  let string_of_react_err = function
    | Inter_eq_o (i, i') ->
        "Outer interfaces " ^ Big.string_of_inter i ^ " and "
        ^ Big.string_of_inter i' ^ " do not match"
    | Inter_eq_i (i, i') ->
        "Inner interfaces" ^ Big.string_of_inter i ^ " and "
        ^ Big.string_of_inter i' ^ " do not match"
    | Lhs_nodes -> "Left hand-side has no nodes"
    | Lhs_solid -> "Left hand-side is not solid"
    | Map_chk -> "Instantiation map is not valid"
    | Val_chk -> val_chk_error_msg

  let to_string r =
    Big.to_string (lhs r)
    ^ "\n-"
    ^ string_of_label (l r)
    ^ "->\n"
    ^ Big.to_string (rhs r)
    ^ match map r with None -> "" | Some eta -> "\n@ " ^ Fun.to_string eta

  let is_valid r =
    let lhs = lhs r and rhs = rhs r in
    Big.inter_equal (Big.outer lhs) (Big.outer rhs)
    && lhs.Big.p.Place.n > 0 && Big.is_solid lhs
    && ( match map r with
       | None -> Big.inter_equal (Big.inner lhs) (Big.inner rhs)
       | Some eta ->
           let s_lhs = lhs.Big.p.Place.s and s_rhs = rhs.Big.p.Place.s in
           Fun.is_total s_rhs eta && Fun.check_codom s_lhs eta )
    && val_chk r

  let is_valid_exn r =
    let lhs = lhs r and rhs = rhs r in
    let i, i' = (Big.outer lhs, Big.outer rhs) in
    if Big.inter_equal i i' then
      if lhs.Big.p.Place.n > 0 then
        if Big.is_solid lhs then
          if
            match map r with
            | None ->
                let i, i' = (Big.inner lhs, Big.inner rhs) in
                if Big.inter_equal i i' then true
                else raise (NOT_VALID (Inter_eq_i (i, i')))
            | Some eta ->
                let s_lhs = lhs.Big.p.Place.s
                and s_rhs = rhs.Big.p.Place.s in
                Fun.is_total s_rhs eta && Fun.check_codom s_lhs eta
          then if val_chk r then true else raise (NOT_VALID Val_chk)
          else raise (NOT_VALID Map_chk)
        else raise (NOT_VALID Lhs_solid)
      else raise (NOT_VALID Lhs_nodes)
    else raise (NOT_VALID (Inter_eq_o (i, i')))

  (* Merge isomorphic occurrences *)
  let filter_iso l =
    (* Input list is assumed without duplicates. Example: extract 4
       [0;2;3;4;5;6;7] -> (4, [0;2;3;5;6;7]) *)
    let rec extract (pred : 'a -> bool) acc = function
      | [] -> (None, acc)
      | x :: l ->
          if pred x then (Some x, l @ acc) else extract pred (x :: acc) l
    in
    let aux1 acc (a, b, c) =
      let iso, non_iso = extract (fun (a', _, _) -> S.equal a a') [] acc in
      match iso with
      | None -> (a, b, c) :: acc
      | Some iso_o -> merge_occ (a, b, c) iso_o :: non_iso
    in
    List.fold_left aux1 [] l

  (* Check application conditions *)
  let filter_conds s lhs conds o =
    let ctx, param, _ =
      Big.decomp ~target:s ~pattern:lhs
        ~i_n:Solver.(o.nodes)
        ~i_e:Solver.(o.edges)
        Solver.(o.hyper_edges)
    in
    let ctx_trans = Place.trans ctx.p
    and param_trans = Place.trans param.p in
    List.for_all
      (fun cnd -> AC.check_cond_memo cnd ~ctx ~ctx_trans ~param ~param_trans)
      conds

  (* Generic step function - lhs automorphisms and s transitive closure are
     precomputed *)
  let step_aux s s_trans rules =
    List.fold_left
      (fun acc (r, autos) ->
        S.Memo.occurrences ~target:s ~pattern:(lhs r) s_trans autos
        |> List.filter (filter_conds s (lhs r) (conds r))
        |> List.rev_map (fun o ->
               ( Big.rewrite
                   Solver.(o.nodes, o.edges, o.hyper_edges)
                   ~s ~r0:(lhs r) ~r1:(rhs r) (map r),
                 l r,
                 [ r ] ))
        |> Base.flip List.rev_append acc)
      [] rules
    |> fun l -> (filter_iso l, List.length l)

  module Memo = struct
    let is_enabled b b_trans r =
      match S.Memo.occurrence ~target:b ~pattern:(lhs r) b_trans with
      | Some o -> filter_conds b (lhs r) (conds r) o
      | None -> false

    let fix b b_trans = function
      | [] -> (b, 0)
      | rules ->
          let rec _step s s_trans = function
            | [] -> None
            | r :: rs -> (
                match
                  S.Memo.occurrence ~target:s ~pattern:(lhs r) s_trans
                with
                | Some ({ nodes = i_n; edges = i_e; hyper_edges = i_h } as o)
                  ->
                    if filter_conds s (lhs r) (conds r) o then
                      Some
                        (Big.rewrite (i_n, i_e, i_h) ~s ~r0:(lhs r)
                           ~r1:(rhs r) (map r))
                    else _step s s_trans rs
                | None -> _step s s_trans rs )
          in
          let rec _fix s s_trans rules i =
            match _step s s_trans rules with
            | Some b -> _fix b (Place.trans b.p) rules (i + 1)
            | None -> (s, i)
          in
          _fix b b_trans rules 0

    let step b b_trans rules = step_aux b b_trans rules |> step_post

    let random_step b b_trans rules =
      step_aux b b_trans rules |> random_step_post
  end

  let is_enabled b r = Memo.is_enabled b (Place.trans b.p) r

  let apply b reacts =
    let apply_rule b b_trans r =
      match S.Memo.occurrence ~target:b ~pattern:(lhs r) b_trans with
      | Some ({ nodes = i_n; edges = i_e; hyper_edges = i_h } as o) ->
          if filter_conds b (lhs r) (conds r) o then
            Some
              (Big.rewrite (i_n, i_e, i_h) ~s:b ~r0:(lhs r) ~r1:(rhs r)
                 (map r))
          else None
      | None -> None
    in
    List.fold_left
      (fun (s, n) r ->
        match apply_rule s (Place.trans s.p) r with
        | Some s' -> (s', n + 1)
        | None -> (s, n))
      (b, 0) reacts
    |> fun (b, n) -> if n = 0 then None else Some b

  (* Reduce a reducible class to the fixed point. Return the input state if
     no rewriting is performed. *)
  let fix b = Memo.fix b (Place.trans b.p)

  let step b rules =
    List.map (fun r -> (r, S.auto (R.lhs r))) rules
    |> Memo.step b (Place.trans b.p)

  let random_step b rules =
    List.map (fun r -> (r, S.auto (R.lhs r))) rules
    |> Memo.random_step b (Place.trans b.p)
end
