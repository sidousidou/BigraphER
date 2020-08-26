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
    const_pri:p_class list ->
    p_class list ->
    ( (int * (Big.t * r_label * r_t list)) list
    * (int * r_label * r_t list) list
    * int )
    * int

  val scan_sim :
    Big.t ->
    const_pri:p_class list ->
    p_class list ->
    (Big.t * r_label * r_t list) option * int
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

  let is_enabled b =
    List.exists (fun r -> S.occurs ~target:b ~pattern:(R.lhs r))

  (*let is_reducible = function P_class _ -> false | P_rclass _ -> true*)

  (* Stop when there are no more classes or when a non reducible class is
     enabled *)
  let rewrite b const_pri =
    let rec _rewrite b m = function
      | [] -> (b, m)
      | P_class rr :: classes ->
          if is_enabled b rr then (b, m) else _rewrite b m classes
      | P_rclass rr :: classes ->
          let b', i = R.fix b rr in
          if i = 0 then _rewrite b' m classes
          else _rewrite b' (m + i) const_pri
    in
    _rewrite b 0 const_pri

  (* Iterate over priority classes *)
  let scan (b, i) ~part_f ~const_pri =
    let rec _scan (b, i) ~matches ~part_f ~const_pri = function
      | [] -> (([], [], i), matches)
      | P_class rr :: cs ->
          let ss, l = R.step b rr in
          if l = 0 then _scan (b, i) ~matches ~part_f ~const_pri cs
          else
            (* Apply rewriting - instantaneous *)
            let ss', l' =
              (* Parmap.parfold *)
              List.fold_left
                (fun (ss, l) (a, b, c) ->
                  let s', l' = rewrite a const_pri in
                  ((s', b, c) :: ss, l + l'))
                ([], l) ss
              (* Merge isomorphic states *)
              |> fun (ss, l) ->
              let open struct
                module G = React.Make_gen (S) (AppCond.Make (S))
              end in
              (G.filter_iso R.merge_occ ss, l)
            in
            (part_f ss', matches + l')
      | P_rclass _ :: cs ->
          (* Skip *)
          _scan (b, i) ~matches ~part_f ~const_pri cs
    in
    _scan (b, i) ~matches:0 ~part_f ~const_pri

  let scan_sim b ~const_pri =
    let rec _scan_sim b m ~const_pri = function
      | [] -> (None, m)
      | P_class rr :: cs -> (
          match R.random_step b rr with
          | None, m' ->
              (* Skip *)
              _scan_sim b (m + m') ~const_pri cs
          | Some (a, b, c), m' ->
              let b', m'' = rewrite a const_pri in
              (Some (b', b, c), m + m' + m'') )
      | P_rclass _ :: cs ->
          (* Skip *)
          _scan_sim b m ~const_pri cs
    in
    _scan_sim b 0 ~const_pri

  let cardinal =
    List.fold_left (fun acc (P_class rr | P_rclass rr) ->
        acc + (List.length rr)) 0

end
