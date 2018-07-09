module type G = sig
  type t
  type l
  val init : int -> String.t list -> t
  val states : t -> (int * Big.t) Base.H_int.t
  val label : t -> (Base.S_string.t * int Base.H_string.t)
  val edges : t -> (int * l * string) Base.H_int.t
  val string_of_l : l -> string
end

module type L = sig
  type t
  type l
  val init : t
  val increment : t -> l -> t
  val is_greater : t -> t -> bool
  val to_string : t -> string
end

module type T = sig
  val typ : Rs.t
end

(* Export functions for graphs *)
module MakeE (G : G) = struct

  let to_prism g =
    let (s, e) =
      (Base.H_int.length (G.states g), Base.H_int.length (G.edges g))
    and edges =
      Base.H_int.fold (fun v u acc ->
          (v, u) :: acc) (G.edges g) [] in
    List.fast_sort (fun (v, (u, _, _)) (v', (u', _, _)) ->
        Base.ints_compare (v, u) (v', u'))
      edges
    |> List.map (fun (v, (u1, u2, _)) ->
        (string_of_int v)
        ^ " "
        ^ (string_of_int u1)
        ^ (match G.string_of_l u2 with
            | "" -> ""
            | s -> " " ^ s))
    |> List.append [ (string_of_int s) ^ " " ^ (string_of_int e) ]
    |> String.concat "\n"

  let to_dot g ~name =
    let rank = "{ rank=source; 0 };\n" in
    let (preds, preds_to_states) = G.label g in
    let states =
      Base.H_int.fold (fun _ (i, _) buff ->
          let bolding = if i = 0 then ", style=\"bold\"" else "" in
          let relevant_preds = Base.S_string.filter (fun pred ->
              let states_satisfying_pred = Base.H_string.find_all
                  preds_to_states pred in
              List.mem i states_satisfying_pred
            ) preds in
          let label = Base.S_string.elements relevant_preds
                      |> String.concat ", " in
          Printf.sprintf
            "%s%d [ label=\"%s\", URL=\"./%d.svg\", fontsize=9.0, \
             id=\"s%d\", fontname=\"monospace\", width=.60, height=.30%s ];\n"
            buff i label i i bolding)
        (G.states g) ""
    and edges =
      Base.H_int.fold (fun v (u1, u2, u3) buff ->
          Printf.sprintf
            "%s%d -> %d [ label=\"%s\", fontname=\"monospace\", fontsize=7.0,\
             arrowhead=\"vee\", arrowsize=0.5 ];\n"
            buff v u1 (G.string_of_l u2 ^ ", " ^ u3))
        (G.edges g) "" in
    Printf.sprintf "digraph \"%s\" {\n\
                    stylesheet = \"style_sbrs.css\"\n%s%s\n%s}"
      name rank states edges

  let to_lab g =
    let (preds, h) = G.label g in
    Base.S_string.fold (fun p acc ->
        (match Base.H_string.find_all h p with
         | [] -> "false"
         | xs -> (List.map (fun s -> "x = " ^ (string_of_int s)) xs
                  |> String.concat " | "))
        |> fun s -> "label \"" ^ p ^ "\" = " ^ s
                    |> fun s -> s ::  acc) preds []
    |> List.rev
    |> String.concat ";\n"

  let iter_states f g =
    Base.H_int.iter (fun _ (i, b) -> f i b) (G.states g)

  let fold_states f g =
    Base.H_int.fold (fun _ (i, b) acc -> f i b acc) (G.states g)

  let iter_edges f g =
    Base.H_int.iter (fun v (u, l, _) -> f v u l) (G.edges g)

  let fold_edges f g =
    Base.H_int.fold (fun v (u, l, _) acc -> f v u l acc) (G.edges g)

end

module type RS = sig
  type react
  type p_class =
    | P_class of react list
    | P_rclass of react list
  type graph
  type label
  type limit
  type react_error
  val typ : Rs.t
  val string_of_react : react -> string
  val lhs : react -> Big.t
  val rhs : react -> Big.t
  val map : react -> Fun.t option
  val string_of_limit : limit -> string
  val is_valid_react : react -> bool
  exception NOT_VALID of react_error
  val is_valid_react_exn : react -> bool
  val string_of_react_err : react_error -> string
  val equal_react : react -> react -> bool
  val is_valid_priority : p_class -> bool
  val is_valid_priority_list : p_class list -> bool
  val cardinal : p_class list -> int
  val step : Big.t -> react list -> (Big.t * label * react list) list * int
  val random_step : Big.t -> react list ->
    (Big.t * label * react list) option * int
  val apply : Big.t -> react list -> Big.t option
  val fix : Big.t -> react list -> Big.t * int
  val rewrite : Big.t -> p_class list -> Big.t * int
  exception MAX of graph * Stats.t
  val bfs :
    s0:Big.t ->
    priorities:p_class list ->
    predicates:(Base.H_string.key * Big.t) list ->
    max:int -> iter_f:(int -> Big.t -> unit) -> graph * Stats.t
  exception DEADLOCK of graph * Stats.t * limit
  exception LIMIT of graph * Stats.t
  val sim :
    s0:Big.t ->
    priorities:p_class list ->
    predicates:(Base.H_string.key * Big.t) list
    -> init_size:int -> stop:limit -> iter_f:(int -> Big.t -> unit)
    -> graph * Stats.t
  val to_prism : graph -> string
  val to_dot : graph -> name:string -> string
  val to_lab : graph -> string
  val iter_states : (int -> Big.t -> unit) -> graph -> unit
  val fold_states : (int -> Big.t -> 'a -> 'a) -> graph -> 'a -> 'a
  val iter_edges : (int -> int -> label -> unit) -> graph -> unit
  val fold_edges : (int -> int -> label -> 'a -> 'a) -> graph -> 'a -> 'a
  val parse_react_unsafe : lhs:Big.t -> rhs:Big.t -> label -> Fun.t option -> react
  val parse_react : lhs:Big.t -> rhs:Big.t -> label -> Fun.t option -> react option
end

module Make (R : RrType.T)
    (P : sig
       type p_class =
         | P_class of R.t list
         | P_rclass of R.t list
       val is_valid : p_class -> bool
       val is_valid_list : p_class list -> bool
       val rewrite : Big.t -> p_class list -> Big.t * int
       val cardinal : p_class list -> int
       val scan : Big.t * int ->
         part_f:((Big.t * R.label * R.t list) list ->
                 ((int * (Big.t * R.label * R.t list)) list
                  * (int * R.label * R.t list) list * int)) ->
         const_pri:p_class list -> p_class list ->
         ((int * (Big.t * R.label * R.t list)) list
          * (int * R.label * R.t list) list * int) * int
       val scan_sim : Big.t ->
         const_pri:p_class list -> p_class list ->
         (Big.t * R.label * R.t list) option * int
     end)
    (L : L with type l = R.label)
    (G : G with type l = R.label)
    (Ty : T) = struct

  type t = G.t

  include P

  include Ty

  type limit = L.t

  type label = R.label

  exception MAX of t * Stats.t

  exception LIMIT of t * Stats.t

  exception DEADLOCK of t * Stats.t * limit

  (* Override some functions *)
  type react_error = R.react_error

  exception NOT_VALID of react_error

  let string_of_react = R.to_string

  let parse_react_unsafe = R.parse

  let parse_react ~lhs ~rhs l f =
    let r = R.parse ~lhs ~rhs l f in
    if R.is_valid r then
      Some r
    else None

  let string_of_limit = L.to_string

  let is_valid_react = R.is_valid

  let is_valid_react_exn r =
    try R.is_valid_exn r with
    | R.NOT_VALID e -> raise (NOT_VALID e)

  let equal_react = R.equal

  let string_of_react_err = R.string_of_react_err

  let lhs = R.lhs

  let rhs = R.rhs

  let map = R.map

  let apply = R.apply

  let fix = R.fix

  let step = R.step

  let random_step = R.random_step

  let is_valid_priority = is_valid

  let is_valid_priority_list = is_valid_list

  let is_new b v =
    let k_buket =
      Base.H_int.find_all v (Big.key b) in
    try
      let (old, _) =
        List.find (fun (_, b') ->
            Big.equal_opt b b')
          k_buket in
      Some old            (* Is_new? FALSE *)
    with
    | Not_found -> None   (* Is_new? TRUE  *)

  (* Partition a list of occurrences into new and old states *)
  let partition g i f_iter =
    List.fold_left (fun (new_acc, old_acc, i) (b, c, d) ->
        match is_new b (G.states g) with
        | None ->
          (let i' = i + 1 in (* Stop here when i > max *)
           f_iter i' b;
           ((i', (b, c, d)) :: new_acc, old_acc, i'))
        | Some x -> (new_acc, (x, c, d) :: old_acc, i))
      ([], [], i)

  (* Add labels for predicates *)
  let check (i, s) (_, h) =
    List.iter (fun (id, p) ->
        if Big.occurs ~target:s ~pattern:p then
          Base.H_string.add h id i)

  (* Number of states in a graph *)
  let size_s g =
    Base.H_int.length (G.states g)

  (* NUmber of edges in a graph. *)
  let size_t g =
    Base.H_int.length (G.edges g)

  let string_of_reaction_rules r =
    List.map string_of_react r |> String.concat " | "

  let rec _bfs g q i m t0 priorities predicates max iter_f =
    if not (Queue.is_empty q) then
      if i > max then
        raise (MAX (g,
                    Stats.init
                      ~t0
                      ~states:(size_s g)
                      ~trans:(size_t g)
                      ~occs:m))
      else begin
        let (v, curr) = Queue.pop q in
        let ((new_s, old_s, i'), m') =
          P.scan (curr, i)
            ~part_f:(partition g i iter_f)
            ~const_pri:priorities priorities in
        List.iter (fun (i, (b, l, r)) ->
            (* Add new states to v *)
            Base.H_int.add (G.states g) (Big.key b) (i, b);
            (* Add edges from v to new states *)
            Base.H_int.add (G.edges g) v (i, l, string_of_reaction_rules r);
            (* Add labels for new states *)
            check (i, b) (G.label g) predicates;
            (* Add new states to q *)
            Queue.push (i, b) q)
          new_s;
        (* Add edges from v to old states *)
        List.iter (fun (a, b, c) ->
            Base.H_int.add (G.edges g) v (a, b, string_of_reaction_rules c))
          old_s;
        (* recursive call *)
        _bfs g q i' (m + m') t0 priorities predicates max iter_f end
    else
      (g, Stats.init ~t0 ~states:(size_s g) ~trans:(size_t g) ~occs:m)

  let bfs ~s0 ~priorities ~predicates ~max ~iter_f =
    let q = Queue.create () in
    (* Apply rewriting to s0 *)
    let (s0', m) = P.rewrite s0 priorities
    and g =
      List.split predicates
      |> fst
      |> G.init max in
    Queue.push (0, s0') q;
    (* Add initial state *)
    iter_f 0 s0';
    Base.H_int.add (G.states g) (Big.key s0') (0, s0');
    check (0, s0') (G.label g) predicates;
    _bfs g q 0 m (Unix.gettimeofday ()) priorities predicates max iter_f

  let rec _sim trace s i t_sim m t0 priorities predicates t_max iter_f =
    if L.is_greater t_sim t_max then
      raise (LIMIT (trace,
                    Stats.init
                      ~t0
                      ~states:(size_s trace)
                      ~trans:(size_t trace)
                      ~occs:m))
    else
      match P.scan_sim s
              ~const_pri:priorities
              priorities with
      | (None, m') ->
        raise (DEADLOCK (trace,
                         Stats.init
                           ~t0
                           ~states:(size_s trace)
                           ~trans:(size_t trace)
                           ~occs:(m + m'),
                         t_sim))
      | (Some (s', l, r), m') ->
        ((*let s' = R.big_of_occ o in*)
          iter_f (i + 1) s';
          Base.H_int.add (G.states trace) (Big.key s') (i + 1, s');
          check (i + 1, s') (G.label trace) predicates;
          Base.H_int.add (G.edges trace) i (i + 1, l,
                                            string_of_reaction_rules r);
          _sim trace s' (i + 1) (L.increment t_sim l) (m + m')
            t0 priorities predicates t_max iter_f)

  let sim ~s0 ~priorities ~predicates ~init_size ~stop ~iter_f =
    Random.self_init ();
    (* Apply rewriting to s0 *)
    let (s0', m) = P.rewrite s0 priorities
    and trace =
      List.split predicates
      |> fst
      |> G.init init_size in
    (* Add initial state *)
    iter_f 0 s0';
    Base.H_int.add (G.states trace) (Big.key s0') (0, s0');
    check (0, s0') (G.label trace) predicates;
    _sim trace s0' 0 L.init m (Unix.gettimeofday ())
      priorities predicates stop iter_f

  include MakeE (G)

end
