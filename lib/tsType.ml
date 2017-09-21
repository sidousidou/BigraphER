module type G = sig
  type t
  type edge_type
  val init : int -> String.t list -> t
  val states : t -> (int * Big.bg) Base.H_int.t
  val label : t -> (Base.S_string.t * int Base.H_string.t)
  val edges : t -> edge_type Base.H_int.t
  val dest : edge_type -> int
  val string_of_arrow : edge_type -> string
end

(* Limit *)
module type L = sig
  type t
  type occ
  val init : t
  val increment : t -> occ -> t
  (* is_greater a b = a > b *)
  val is_greater : t -> t -> bool
  val to_string : t -> string
end

module type T = sig
  val typ : Rs.t
end

type stats_t = { time : float;
                 states : int;
                 trans : int;
                 occs : int; }

(* Execution statistics *)
module type S = sig
  type t = stats_t
  type g
  val init : float -> g -> int -> t
  (* Returns a list of string triples (description, value, run dependant) *)
  val to_string : t -> (string * string * bool) list
end

module MakeS (G : G) = struct

  type t = stats_t

  (*type g = G.t*)

  let init t0 g m =
    { time = (Unix.gettimeofday () -. t0);
      states = Base.H_int.length (G.states g);
      trans = Base.H_int.length (G.edges g);
      occs = m; }

  let to_string stats =
    [ ("Build time:", Printf.sprintf "%-3g" stats.time, true);
      ("States:", Printf.sprintf "%-8d" stats.states, false);
      ("Transitions:", Printf.sprintf "%-8d" stats.trans, false);
      ("Occurrences:", Printf.sprintf "%-8d" stats.occs, false) ]

end

(* Export functions *)
module MakeE (G : G) = struct

  let to_prism g =
    let (s, e) =
      (Base.H_int.length (G.states g), Base.H_int.length (G.edges g))
    and edges =
      Base.H_int.fold (fun v u acc ->
          (v, u) :: acc) (G.edges g) [] in
    List.fast_sort (fun (v, u) (v', u') ->
        Base.ints_compare (v, G.dest u) (v', G.dest u'))
      edges
    |> List.map (fun (v, u) ->
        (string_of_int v)
        ^ " "
        ^ (string_of_int (G.dest u))
        ^ (match G.string_of_arrow u with
            | "" -> ""
            | s -> " " ^ s))
    |> List.append [(string_of_int s) ^ " " ^ (string_of_int e)]
    |> String.concat "\n"

  let to_json g =
    let open Base.JSON in
    Base.H_int.fold (fun v u acc ->
        (J_node [ J_int ("source", v);
                  J_int ("target", G.dest u) ])
        :: acc) (G.edges g) []
    |> (fun l -> J_node [ J_array ("transition_system", l) ])
    |> to_string
  
  let to_dot g ~name =
    let rank = "{ rank=source; 0 };\n" in
    let states =
      Base.H_int.fold (fun _ (i, _) buff ->
          if i = 0 then
            Printf.sprintf
              "%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, \
               id=\"s%d\", fontname=\"monospace\", fixedsize=true, width=.60, \
               height=.30, style=\"bold\" ];\n"
              buff i i i i
          else
            Printf.sprintf
              "%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, \
               id=\"s%d\", fontname=\"monospace\", fixedsize=true, width=.60, \
               height=.30 ];\n"
              buff i i i i)
        (G.states g) ""
    and edges =
      Base.H_int.fold (fun v u buff ->
          Printf.sprintf
            "%s%d -> %d [ label=\"%s\", fontname=\"monospace\", fontsize=7.0,\
             arrowhead=\"vee\", arrowsize=0.5 ];\n"
            buff v (G.dest u) (G.string_of_arrow u))
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
  
  let iter_states ~f g =
    Base.H_int.iter (fun _ (i, b) -> f i b) (G.states g)

  let write_svg g ~name ~path =
    Export.write_svg (to_dot g ~name) ~name ~path

  let write_prism g ~name ~path =
    Export.write_string (to_prism g) ~name ~path

  let write_lab g ~name ~path =
    Export.write_string (to_lab g) ~name ~path

  let write_dot g ~name ~path =
    Export.write_string (to_dot g ~name) ~name ~path
      
  let write_json g ~name ~path =
    Export.write_string (to_json g) ~name ~path

end

(* The interface of a Reactive System *)
module type RS =
sig
  type react
  type p_class =
    | P_class of react list
    | P_rclass of react list
  type stats = stats_t
  type graph
  type react_error
  type occ
  type limit
  type label
  val typ : Rs.t
  val string_of_stats : stats -> (string * string * bool) list
  val string_of_react : react -> string
  val parse_react :
    lhs:Big.bg -> rhs:Big.bg -> label option -> int Fun.t option -> react
  val lhs_of_react : react -> Big.bg
  val rhs_of_react : react -> Big.bg
  val string_of_limit : limit -> string
  val is_valid_react : react -> bool
  exception NOT_VALID of react_error
  val is_valid_react_exn : react -> bool
  val string_of_react_err : react_error -> string
  (* val is_inst : react -> bool *)
  val is_valid_priority : p_class -> bool
  val is_valid_priority_list : p_class list -> bool
  val cardinal : p_class list -> int
  val step : Big.bg -> react list -> occ list * int
  val random_step : Big.bg -> react list -> occ option * int
  val apply : Big.bg -> react list -> Big.bg
  val fix : Big.bg -> react list -> Big.bg * int
  val rewrite : Big.bg -> p_class list -> Big.bg * int
  exception MAX of graph * stats
  val bfs :
    s0:Big.bg ->
    priorities:p_class list ->
    predicates:(Base.H_string.key * Big.bg) list ->
    max:int -> iter_f:(int -> Big.bg -> unit) -> graph * stats
  exception DEADLOCK of graph * stats * limit
  exception LIMIT of graph * stats
  val sim :
    s0:Big.bg ->
    priorities:p_class list ->
    predicates:(Base.H_string.key * Big.bg) list
    -> init_size:int -> stop:limit -> iter_f:(int -> Big.bg -> unit)
    -> graph * stats
  val to_prism : graph -> string
  val to_dot : graph -> name:string -> string
  val to_lab : graph -> string
  val iter_states : f:(int -> Big.bg -> unit) -> graph -> unit
  val write_svg : graph -> name:string -> path:string -> int
  val write_prism : graph -> name:string -> path:string -> int
  val write_lab : graph -> name:string -> path:string -> int
  val write_dot : graph -> name:string -> path:string -> int
  val write_json : graph -> name:string -> path:string -> int
end

(* Discrete time or continuous time *)
module type TT = sig
  type t
  val stop : t
end

module Make (R : RrType.T)
    (P : sig
       type p_class =
         | P_class of R.t list
         | P_rclass of R.t list
       val is_valid : p_class -> bool
       val is_valid_list : p_class list -> bool
       val rewrite : Big.bg -> p_class list -> Big.bg * int
       val cardinal : p_class list -> int
       val scan :
         Big.bg * int
         -> part_f:(R.occ list ->
                    ((int * R.occ) list * R.edge list * int))
         -> const_pri:p_class list -> p_class list
         -> ((int * R.occ) list * R.edge list * int) * int
       val scan_sim : Big.bg ->
         const_pri:p_class list -> p_class list ->
         R.occ option * int
     end)
    (L : L with type occ = R.occ)
    (G : G with type edge_type = R.edge)
    (Ty : T) = struct

  type t = G.t

  include P

  include Ty

  module S = MakeS (G)

  type stats = S.t

  type occ = R.occ

  type limit = L.t

  type label = R.label

  exception MAX of t * stats

  exception LIMIT of t * stats

  exception DEADLOCK of t * stats * limit

  (* Override some functions *)
  type react_error = R.react_error

  exception NOT_VALID of react_error

  let string_of_stats = S.to_string

  let string_of_react = R.to_string

  let parse_react = R.parse

  let string_of_limit = L.to_string

  let is_valid_react = R.is_valid

  let is_valid_react_exn r =
    try R.is_valid_exn r with
    | R.NOT_VALID e -> raise (NOT_VALID e)

  let string_of_react_err = R.string_of_react_err

  let lhs_of_react r = R.lhs r

  let rhs_of_react r = R.rhs r

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
    List.fold_left (fun (new_acc, old_acc, i) o ->
        let b = R.big_of_occ o in
        match is_new b (G.states g) with
        | None ->
          (let i' = i + 1 in (* Stop here when i > max *)
           f_iter i' b;
           ((i', o) :: new_acc, old_acc, i'))
        | Some x -> (new_acc, (R.edge_of_occ o x) :: old_acc, i))
      ([], [], i)

  (* Add labels for predicates *)
  let check (i, s) (_, h) =
    List.iter (fun (id, p) ->
        if Big.occurs ~target:s ~pattern:p then
          Base.H_string.add h id i)

  let rec _bfs g q i m t0 priorities predicates max iter_f =
    if not (Queue.is_empty q) then
      if i > max then
        raise (MAX (g, S.init t0 g m))
      else begin
        let (v, curr) = Queue.pop q in
        let ((new_s, old_s, i'), m') =
          P.scan (curr, i)
            ~part_f:(partition g i iter_f)
            ~const_pri:priorities priorities in
        List.iter (fun (i, o) ->
            let b = R.big_of_occ o in
            (* Add new states to v *)
            Base.H_int.add (G.states g) (Big.key b) (i, b);
            (* Add labels for new states *)
            check (i, b) (G.label g) predicates;
            (* Add new states to q *)
            Queue.push (i, R.big_of_occ o) q)
          new_s;
        (* Add edges from v to new states *)
        List.iter (fun (u, o) ->
            Base.H_int.add (G.edges g) v (R.edge_of_occ o u))
          new_s;
        (* Add edges from v to old states *)
        List.iter (fun e ->
            Base.H_int.add (G.edges g) v e)
          old_s;
        (* recursive call *)
        _bfs g q i' (m + m') t0 priorities predicates max iter_f end
    else
      (g, S.init t0 g m)

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
      raise (LIMIT (trace, S.init t0 trace m))
    else
      match P.scan_sim s
              ~const_pri:priorities
              priorities with
      | (None, m') ->
        raise (DEADLOCK (trace, S.init t0 trace (m + m'), t_sim))
      | (Some o, m') ->
        (let s' = R.big_of_occ o in
         iter_f (i + 1) s';
         Base.H_int.add (G.states trace) (Big.key s') (i + 1, s');
         check (i + 1, s') (G.label trace) predicates;
         Base.H_int.add (G.edges trace) i (R.edge_of_occ o (i + 1));
         _sim trace s' (i + 1) (L.increment t_sim o) (m + m')
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
