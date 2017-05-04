module type R =
sig
  type t
  type label
  type occ
  type edge
  val lhs : t -> Big.bg
  val rhs : t -> Big.bg
  val l : t -> label
  val string_of_label : label -> string
  val map : t -> int Fun.t option
  val val_chk : t -> bool
  val val_chk_error_msg : string
  val to_occ : Big.bg -> t -> occ
  val big_of_occ : occ -> Big.bg
  val merge_occ : occ -> occ -> occ
  val update_occ : occ -> Big.bg -> occ
  (* Replace the bigraph in an occurrence with an index *)
  val edge_of_occ : occ -> int -> edge
  val step : Big.bg -> t list -> occ list * int
  val random_step : Big.bg -> t list -> occ option * int
end

module type T =
sig
  type t
  type label
  type occ
  type edge
  type react_error
  exception NOT_VALID of react_error
  val lhs : t -> Big.bg
  val rhs : t -> Big.bg
  val l : t -> label
  val string_of_label : label -> string
  val map : t -> int Fun.t option
  val to_occ : Big.bg -> t -> occ
  val big_of_occ : occ -> Big.bg
  val merge_occ : occ -> occ -> occ
  val update_occ : occ -> Big.bg -> occ
  val edge_of_occ : occ -> int -> edge
  val to_string : t -> string
  val is_valid : t -> bool
  val is_valid_exn : t -> bool			  
  val string_of_react_err : react_error -> string
  val is_enabled : Big.bg -> t -> bool
  val fix : Big.bg -> t list -> Big.bg * int
  val step : Big.bg -> t list -> occ list * int
  val random_step : Big.bg -> t list -> occ option * int
end

(* Generic step function *)
let gen_step b rules ~big_of_occ ~to_occ ~merge_occ ~lhs ~rhs ~map =
  (* Input list is assumed without duplicates. Example: extract 4
     [0;2;3;4;5;6;7] -> (4, [0;2;3;5;6;7]) *)	   
  let rec extract (pred :'a -> bool) acc = function
    | [] -> (None, acc)
    | x :: l -> if pred x then (Some x, l @ acc)
      else extract pred (x :: acc) l in
  let filter_iso l =
    let aux1 acc o =
      let (iso, non_iso) =
        extract (fun o' ->
            Big.equal (big_of_occ o) (big_of_occ o'))
          [] acc in
      match iso with
      | None -> o :: acc
      | Some iso_o -> (merge_occ o iso_o) :: non_iso in
    (List.fold_left aux1 [] l, List.length l) in
  let aux2 acc r =
    (Big.occurrences b (lhs r)
     (* Parmap.parmap *)			 
     |> List.map (fun o ->
         to_occ
           (Big.rewrite o b (lhs r) (rhs r) (map r)) r))
    @ acc in
  List.fold_left aux2 [] rules
  |> filter_iso

module Make (R : R) = struct

  include R

  type react_error =
      Inter_eq_i of Big.inter * Big.inter
    | Inter_eq_o of Big.inter * Big.inter
    | Lhs_nodes | Lhs_solid | Map_chk | Val_chk

  exception NOT_VALID of react_error

  let string_of_react_err = function
    | Inter_eq_o (i, i')->
      ("Outer interfaces " ^ (Big.string_of_inter i)
       ^ " and " ^ (Big.string_of_inter i') ^ " do not match")
    | Inter_eq_i (i, i') ->
      ("Inner interfaces" ^ (Big.string_of_inter i)
       ^ " and " ^ (Big.string_of_inter i') ^ " do not match")
    | Lhs_nodes -> "Left hand-side has no nodes"
    | Lhs_solid -> "Left hand-side is not solid"
    | Map_chk -> "Instantiation map is not valid"
    | Val_chk -> val_chk_error_msg

  let to_string r =
    (Big.to_string (lhs r))
    ^ "\n--"
    ^ (string_of_label (l r))
    ^ "-->\n"
    ^	(Big.to_string (rhs r))
    ^ (match map r with
        | None -> ""
        | Some eta -> "\n@ " ^ (Fun.to_string eta))

  let is_valid r =
    let lhs = lhs r
    and rhs = rhs r in
    (Big.inter_equal (Big.outer lhs) (Big.outer rhs))
    && (lhs.Big.p.Place.n > 0)
    && (Big.is_solid lhs)
    && (match map r with
        | None -> Big.inter_equal (Big.inner lhs) (Big.inner rhs)
        | Some eta ->
          (let s_lhs = lhs.Big.p.Place.s
           and s_rhs = rhs.Big.p.Place.s in
           (Fun.is_total s_rhs eta)
           && (Fun.check_codom 0 (s_lhs - 1) eta)))
    && val_chk r

  let is_valid_exn r =
    let lhs = lhs r
    and rhs = rhs r in
    let (i, i') = (Big.outer lhs, Big.outer rhs) in
    if Big.inter_equal i i'
    then if lhs.Big.p.Place.n > 0
      then if Big.is_solid lhs
        then if (match map r with
            | None ->
              (let (i, i') = (Big.inner lhs, Big.inner rhs) in
               if Big.inter_equal i i'
               then true
               else raise (NOT_VALID (Inter_eq_i (i, i'))))
            | Some eta ->
              (let s_lhs = lhs.Big.p.Place.s
               and s_rhs = rhs.Big.p.Place.s in
               (Fun.is_total s_rhs eta)
               && (Fun.check_codom 0 (s_lhs - 1) eta)))
          then if val_chk r
            then true
            else raise (NOT_VALID Val_chk)
          else raise (NOT_VALID Map_chk)
        else raise (NOT_VALID Lhs_solid)
      else raise (NOT_VALID Lhs_nodes)
    else raise (NOT_VALID (Inter_eq_o (i, i')))

  let is_enabled b r =
    Big.occurs b (lhs r)

  (* Reduce a reducible class to the fixed point. Return the input state if no
     rewriting is performed. *)
  let fix b rules =
    let t_trans = Sparse.trans b.Big.p.Place.nn in
    let rec _step s = function
      | [] -> None
      | r :: rs ->
        (match Big.occurrence s (lhs r) t_trans with
         | Some o ->
           Some (Big.rewrite o s (lhs r) (rhs r) (map r))
         | None -> _step s rs) in
    let rec _fix s rules i =
      match _step s rules with
      | Some b ->  _fix b rules (i + 1)
      | None -> (s, i) in
    _fix b rules 0

end
