module type P = sig
  type t
  val f_val : t -> bool
  val f_r_val : t -> bool
end

module Make (R :RrType.T)
    (P : P with type t = R.t list) =
struct

  type p_class =
    | P_class of R.t list 
    | P_rclass of R.t list

  let is_valid = function
    | P_class []
    | P_rclass [] -> false	
    | P_class rr -> (List.for_all R.is_valid rr)
                    && P.f_val rr
    | P_rclass rr -> (List.for_all R.is_valid rr)
                     && P.f_r_val rr

  let is_valid_list =
    List.exists (function
        | P_class _ -> true
        | P_rclass _ -> false)

  let is_enabled b =
    List.exists (fun r -> Big.occurs b (R.lhs r))

  let is_reducible = function
    | P_class _ -> false
    | P_rclass _ -> true

  (* Stop when there are no more classes or when a non reducible class is enabled *)
  let rewrite b const_pri =
    let rec _rewrite b m = function 
      | [] -> (b, m)
      | (P_class rr) :: classes ->
        (if is_enabled b rr then (b, m)
         else _rewrite b m classes)
      | (P_rclass rr) :: classes ->
        (let (b', i) = R.fix b rr in
         if i = 0
         then _rewrite b' (m + i) classes
         else _rewrite b' (m + i) const_pri) in
    _rewrite b 0 const_pri

  (* Iterate over priority classes *)
  let scan (b, i) ~part_f ~const_pri =
    let rec _scan (b, i) ~matches ~part_f ~const_pri = function
      | [] -> (([], [], i), matches)
      | (P_class rr) :: cs ->
        (let (ss, l) = R.step b rr in
         if l = 0 then _scan (b, i) ~matches ~part_f ~const_pri cs 
         else 
           (* Apply rewriting - instantaneous *)
           let (ss', l') =
             (* Parmap.parfold *)
             List.fold_left (fun (ss,  l) o -> 
                 let (s', l') =
                   rewrite (R.big_of_occ o) const_pri in
                 ((R.update_occ o s') :: ss, l + l'))
               ([], l) ss in
           ((part_f ss' : (int * R.occ) list * R.edge list * int), matches + l'))
      | (P_rclass _) :: cs -> (* Skip *)
        _scan (b, i) ~matches ~part_f ~const_pri cs in
    _scan (b, i) ~matches:0 ~part_f ~const_pri

  let scan_sim b ~const_pri =
    let rec _scan_sim b m ~const_pri = function
      | [] -> (None, m)
      | (P_class rr) :: cs ->
        (match R.random_step b rr with
         | (None, m') -> (* Skip *)
           _scan_sim b (m + m') ~const_pri cs
         | (Some o, m') ->
           (let (b', m'') = rewrite (R.big_of_occ o) const_pri in
            (Some (R.update_occ o b'), m + m' + m'')))
      | (P_rclass _) :: cs -> (* Skip *)
        _scan_sim b m ~const_pri cs in
    _scan_sim b 0 ~const_pri

  let cardinal l =
    let to_reacts = function
      | P_class rr
      | P_rclass rr -> rr in
    List.map to_reacts l
    |> List.flatten
    |> List.length

end
