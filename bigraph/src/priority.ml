module type Val_Check = sig
  type t

  val f_val : t -> bool

  val f_r_val : t -> bool
end

module type P = sig
  type r_t

  type r_label

  type p_class = P_class of r_t list | P_rclass of r_t list

  val is_valid : p_class -> bool

  val is_valid_list : p_class list -> bool

  val cardinal : p_class list -> int

  val rewrite : Big.t -> p_class list -> Big.t * int

  val scan :
    Big.t * int ->
    part_f:
      ((Big.t * r_label * r_t list) list ->
      (int * (Big.t * r_label * r_t list)) list
      * (int * r_label * r_t list) list
      * int) ->
    p_class list ->
    ( (int * (Big.t * r_label * r_t list)) list
    * (int * r_label * r_t list) list
    * int )
    * int

  val scan_sim :
    Big.t -> p_class list -> (Big.t * r_label * r_t list) option * int

  module Memo : sig
    type t =
      | C of (r_t * (Iso.t * Iso.t) list) list
      | R of (r_t * (Iso.t * Iso.t) list) list

    val init : p_class list -> t list

    val rewrite : Big.t -> Sparse.t -> t list -> Big.t * int

    val scan :
      Big.t * int ->
      Sparse.t ->
      part_f:
        ((Big.t * r_label * r_t list) list ->
        (int * (Big.t * r_label * r_t list)) list
        * (int * r_label * r_t list) list
        * int) ->
      t list ->
      ( (int * (Big.t * r_label * r_t list)) list
      * (int * r_label * r_t list) list
      * int )
      * int

    val scan_sim :
      Big.t ->
      Sparse.t ->
      t list ->
      (Big.t * r_label * r_t list) option * int
  end
end

module Make
    (S : Solver.M)
    (R : React.T)
    (V : Val_Check with type t := R.t list) =
struct
  type p_class = P_class of R.t list | P_rclass of R.t list

  let is_valid = function
    | P_class [] | P_rclass [] -> false
    | P_class rr -> List.for_all R.is_valid rr && V.f_val rr
    | P_rclass rr -> List.for_all R.is_valid rr && V.f_r_val rr

  let is_valid_list =
    List.exists (function P_class _ -> true | P_rclass _ -> false)

  module Memo = struct
    type t =
      | C of (R.t * (Iso.t * Iso.t) list) list
      | R of (R.t * (Iso.t * Iso.t) list) list

    let is_enabled b b_trans =
      List.exists (fun (r, _) ->
          S.Memo.occurs ~target:b ~pattern:(R.lhs r) b_trans)

    let init (priorities : p_class list) =
      let r_map = List.map (fun r -> (r, S.auto (R.lhs r))) in
      List.map
        (function P_class rr -> C (r_map rr) | P_rclass rr -> R (r_map rr))
        priorities

    let rewrite b b_trans const_pri =
      let rec _rewrite b b_trans m = function
        | [] -> (b, m)
        | C rr :: classes ->
            if is_enabled b b_trans rr then (b, m)
            else _rewrite b b_trans m classes
        | R rr :: classes ->
            let b', i = List.map fst rr |> R.Memo.fix b b_trans in
            if i = 0 then _rewrite b' (Place.trans b'.p) m classes
            else _rewrite b' (Place.trans b'.p) (m + i) const_pri
      in
      _rewrite b b_trans 0 const_pri

    (* Iterate over priority classes *)
    let scan (b, i) b_trans ~part_f priorities =
      let rec _scan (b, i) ~matches ~part_f ~const_pri = function
        | [] -> (([], [], i), matches)
        | C rr :: cs ->
            let ss, l = R.Memo.step b b_trans rr in
            if l = 0 then _scan (b, i) ~matches ~part_f ~const_pri cs
            else
              (* Apply rewriting - instantaneous *)
              let ss', l' =
                List.fold_left
                  (fun (ss, l) (a, b, c) ->
                    let s', l' = rewrite a (Place.trans a.p) const_pri in
                    ((s', b, c) :: ss, l + l'))
                  ([], l) ss
                (* Merge isomorphic states *)
                |> fun (ss, l) -> (R.filter_iso ss, l)
              in
              (part_f ss', matches + l')
        | R _ :: cs ->
            (* Skip *)
            _scan (b, i) ~matches ~part_f ~const_pri cs
      in
      _scan (b, i) ~matches:0 ~part_f ~const_pri:priorities priorities

    let scan_sim b b_trans priorities =
      let rec _scan_sim b m ~const_pri = function
        | [] -> (None, m)
        | C rr :: cs -> (
            match R.Memo.random_step b b_trans rr with
            | None, m' ->
                (* Skip *)
                _scan_sim b (m + m') ~const_pri cs
            | Some (a, b, c), m' ->
                let b', m'' = rewrite a (Place.trans a.p) const_pri in
                (Some (b', b, c), m + m' + m'') )
        | R _ :: cs ->
            (* Skip *)
            _scan_sim b m ~const_pri cs
      in
      _scan_sim b 0 ~const_pri:priorities priorities
  end

  (* let is_enabled b =
   *   List.exists (fun r -> S.occurs ~target:b ~pattern:(R.lhs r)) *)

  (* Stop when there are no more classes or when a non reducible class is
     enabled *)
  let rewrite b priorities =
    Memo.(init priorities |> rewrite b (Place.trans b.p))

  (* Iterate over priority classes *)
  let scan (b, i) ~part_f priorities =
    Memo.(init priorities |> scan (b, i) (Place.trans b.p) ~part_f)

  let scan_sim b priorities =
    Memo.(init priorities |> scan_sim b (Place.trans b.p))

  let cardinal =
    List.fold_left
      (fun acc (P_class rr | P_rclass rr) -> acc + List.length rr)
      0
end
