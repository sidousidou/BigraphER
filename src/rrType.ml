module type R =
sig
  type t
  type label
  val lhs : t -> Big.t
  val rhs : t -> Big.t
  val l : t -> label
  val equal : t -> t -> bool
  val map : t -> Fun.t option
  val merge_occ : (Big.t * label) -> (Big.t * label) -> (Big.t * label)
  val val_chk : t -> bool
  val val_chk_error_msg : string
  val string_of_label : label -> string
  val parse : lhs:Big.t -> rhs:Big.t -> label -> Fun.t option -> t
  val step : Big.t -> t list -> (Big.t * label) list * int
  val random_step : Big.t -> t list -> (Big.t * label) option * int
end

module type T =
sig
  type t
  type label
  type react_error
  exception NOT_VALID of react_error
  val lhs : t -> Big.t
  val rhs : t -> Big.t
  val l : t -> label
  val equal : t -> t -> bool
  val map : t -> Fun.t option
  val merge_occ : (Big.t * label) -> (Big.t * label) -> (Big.t * label)
  val parse : lhs:Big.t -> rhs:Big.t -> label -> Fun.t option -> t
  val to_string : t -> string
  val is_valid : t -> bool
  val is_valid_exn : t -> bool
  val string_of_react_err : react_error -> string
  val is_enabled : Big.t -> t -> bool
  val apply : Big.t -> t list -> Big.t option
  val fix : Big.t -> t list -> Big.t * int
  val step : Big.t -> t list -> (Big.t * label) list * int
  val random_step : Big.t -> t list -> (Big.t * label) option * int
end

(* Merge isomorphic occurrences *)
let filter_iso merge_occ l =
  (* Input list is assumed without duplicates. Example: extract 4
     [0;2;3;4;5;6;7] -> (4, [0;2;3;5;6;7]) *)
  let rec extract (pred :'a -> bool) acc = function
    | [] -> (None, acc)
    | x :: l -> if pred x then (Some x, l @ acc)
      else extract pred (x :: acc) l in
  let aux1 acc o =
    let (iso, non_iso) =
      extract (fun o' ->
          Big.equal (fst o) (fst o'))
        [] acc in
    match iso with
    | None -> o :: acc
    | Some iso_o -> (merge_occ o iso_o) :: non_iso in
  List.fold_left aux1 [] l

(* Generic step function *)
let gen_step s rules
    (merge_occ:(Big.t * 'b) -> (Big.t * 'b) -> (Big.t * 'b))
    ~lhs ~rhs ~label ~map  =
  let aux2 acc r =
    (Big.occurrences ~target:s ~pattern:(lhs r)
     (* Parmap.parmap *)
     |> List.map (fun o ->
         (Big.rewrite o ~s ~r0:(lhs r) ~r1:(rhs r) (map r), label r)))
    @ acc in
  List.fold_left aux2 [] rules
  |> fun l -> (filter_iso merge_occ l, List.length l)

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
    ^ "\n-"
    ^ (string_of_label (l r))
    ^ "->\n"
    ^ (Big.to_string (rhs r))
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
           (Fun.is_total s_rhs eta) && (Fun.check_codom s_lhs eta)))
    && val_chk r

  let is_valid_exn r =
    let lhs = lhs r
    and rhs = rhs r in
    let (i, i') = (Big.outer lhs, Big.outer rhs) in
    if Big.inter_equal i i'
    then if lhs.Big.p.Place.n > 0
      then if Big.is_solid lhs
        then if match map r with
          | None ->
            (let (i, i') = (Big.inner lhs, Big.inner rhs) in
             if Big.inter_equal i i'
             then true
             else raise (NOT_VALID (Inter_eq_i (i, i'))))
          | Some eta ->
            (let s_lhs = lhs.Big.p.Place.s
             and s_rhs = rhs.Big.p.Place.s in
             (Fun.is_total s_rhs eta) && (Fun.check_codom s_lhs eta))
          then if val_chk r
            then true
            else raise (NOT_VALID Val_chk)
          else raise (NOT_VALID Map_chk)
        else raise (NOT_VALID Lhs_solid)
      else raise (NOT_VALID Lhs_nodes)
    else raise (NOT_VALID (Inter_eq_o (i, i')))

  let is_enabled b r =
    Big.occurs ~target:b ~pattern:(lhs r)
  
  let apply b reacts =
    let apply_rule b r = 
      match Big.occurrence
              ~target:b
              ~pattern:(lhs r)
              (Place.trans b.Big.p) with
      | Some o ->
        Some (Big.rewrite o ~s:b ~r0:(lhs r) ~r1:(rhs r) (map r))
      | None -> None in
    List.fold_left (fun (s, n) r ->
        match apply_rule s r with
        | Some s' -> (s', n + 1)
        | None -> (s, n))
      (b, 0) reacts
    |> (fun (b, n) ->
        if n = 0 then None
        else Some b)
  
  (* Reduce a reducible class to the fixed point. Return the input state if no
     rewriting is performed. *)
  let fix b = function
    | [] -> (b, 0)
    | rules ->
      let t_trans = Place.trans b.Big.p in
      let rec _step s = function
        | [] -> None
        | r :: rs ->
          (match Big.occurrence ~target:s ~pattern:(lhs r) t_trans with
           | Some o ->
             Some (Big.rewrite o ~s ~r0:(lhs r) ~r1:(rhs r) (map r))
           | None -> _step s rs) in
      let rec _fix s rules i =
        match _step s rules with
        | Some b ->  _fix b rules (i + 1)
        | None -> (s, i) in
      _fix b rules 0
        
end
