type rs_type = BRS | PBRS | SBRS | NBRS

let string_of_rs_type = function
  | BRS -> "BRS"
  | PBRS -> "Probabilistic BRS"
  | SBRS -> "Stochastic BRS"
  | NBRS -> "Nondeterministic BRS"

type stats = { time : float; states : int; trans : int; occs : int }

let stats_init ~t0 ~states ~trans ~occs =
  { time = Unix.gettimeofday () -. t0; states; trans; occs }

let stats_descr stats =
  [
    ("Build time:", Printf.sprintf "%-3g" stats.time, true);
    ("States:", Printf.sprintf "%-8d" stats.states, false);
    ("Transitions:", Printf.sprintf "%-8d" stats.trans, false);
    ("Occurrences:", Printf.sprintf "%-8d" stats.occs, false);
  ]

module type G = sig
  type t

  type l

  val init : int -> Base.Predicate.t list -> t

  val states : t -> (int * Big.t) Base.H_int.t

  val label : t -> Base.S_predicate.t * int Base.H_predicate.t

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

module type K = sig
  val typ : rs_type
end

(* Export functions for graphs *)
module MakeE (G : G) = struct
  (* Check if there are any edges with action labels *)
  let is_mdp g =
    Base.H_int.fold
      (fun _ (_, label, _) answer ->
        answer || String.contains (G.string_of_l label) ' ')
      (G.edges g) false

  (* Convert a hash table of edges into a sorted list of tuples with action
     names and edge labels (with an empty slot for action IDs) *)
  let list_of_edges g =
    Base.H_int.fold
      (fun vertex1 (vertex2, edge_label, _) acc ->
        let action, reward, label =
          match G.string_of_l edge_label with
          | "" -> ("", "", "")
          | s ->
              if String.contains s ' ' then
                let substrings = String.split_on_char ' ' s in
                ( List.hd substrings,
                  List.nth substrings 1,
                  List.nth substrings 2 )
              else ("", "", s)
        in
        (vertex1, vertex2, "", action, label, reward) :: acc)
      (G.edges g) []
    |> List.fast_sort
         (fun (v11, v12, _, a1, _l1, _r1) (v21, v22, _, a2, _l2, _r2) ->
           if v11 > v21 then 1
           else if v11 < v21 then -1
           else if a1 > a2 then 1
           else if a1 < a2 then -1
           else v12 - v22)

  (* Add action IDs to the list of tuples. The first action of each vertex
     gets ID 0, and so on. The list is assumed to be sorted. Also return the
     number of choices, i.e., the sum of the numbers of distinct actions per
     vertex. *)
  let action_names_to_ints edges =
    let previous_vertex = ref (-1) in
    let previous_action = ref "!" in
    let previous_number = ref (-1) in
    let choices = ref 0 in
    let edges =
      List.map
        (fun (vertex1, vertex2, _, action, label, reward) ->
          if vertex1 <> !previous_vertex then (
            previous_vertex := vertex1;
            previous_action := "!";
            previous_number := -1 );
          if action <> !previous_action then (
            previous_action := action;
            previous_number := !previous_number + 1;
            choices := !choices + 1 );
          ( vertex1,
            vertex2,
            string_of_int !previous_number,
            action,
            label,
            reward ))
        edges
    in
    (edges, !choices)

  (* A generalisation of to_prism and to_transition_rewards. print_rewards
     switches between the two modes. *)
  let generate_transitions g print_rewards =
    let mdp = is_mdp g in
    let num_states = Base.H_int.length (G.states g) in
    let edges = list_of_edges g in
    let edges, num_choices =
      if mdp then action_names_to_ints edges else (edges, 0)
    in
    let edges =
      List.filter
        (fun (_, _, _, _, _, reward) -> (not print_rewards) || reward <> "0")
        edges
    in
    let num_rows = List.length edges in
    List.map
      (fun (vertex1, vertex2, action_id, action, label, reward) ->
        let last_columns =
          if print_rewards then " " ^ reward
          else
            (if label = "" then "" else " " ^ label)
            ^ if action = "" then "" else " " ^ action
        in
        string_of_int vertex1
        ^ (if action_id = "" then "" else " " ^ action_id)
        ^ " " ^ string_of_int vertex2 ^ last_columns)
      edges
    |> List.append
         [
           string_of_int num_states
           ^ (if mdp then " " ^ string_of_int num_choices else "")
           ^ " " ^ string_of_int num_rows;
         ]
    |> String.concat "\n"

  let to_prism g = generate_transitions g false

  (* Calculate the total reward of a state *)
  let total_reward g i =
    let preds, preds_to_states = G.label g in
    let relevant_preds =
      Base.S_predicate.filter
        (fun pred ->
          Base.H_predicate.find_all preds_to_states pred |> List.mem i)
        preds
    in
    Base.S_predicate.fold
      (fun (_, reward) sum -> reward + sum)
      relevant_preds 0

  let to_state_rewards g =
    let states = G.states g in
    let rewards =
      Base.H_int.fold
        (fun _ (i, _) acc ->
          let reward = total_reward g i in
          if reward = 0 then acc else Printf.sprintf "%d %d" i reward :: acc)
        states []
      |> List.fast_sort compare
    in
    List.append
      [
        Printf.sprintf "%d %d"
          (Base.H_int.length states)
          (List.length rewards);
      ]
      rewards
    |> String.concat "\n"

  let to_transition_rewards g = generate_transitions g true

  let generate_reward_html reward =
    let color, sign =
      if reward > 0 then ("darkgreen", "+") else ("red", "")
    in
    if reward = 0 then ""
    else Printf.sprintf "<br/><font color='%s'>%s%d</font>" color sign reward

  (* Return a DOT string of action nodes of an MDP as well as edges to them,
     and a hash table mapping (vertex, action) pairs to the IDs of the
     actions *)
  let construct_action_nodes g mdp =
    let next_id = ref (Base.H_int.length (G.states g) - 1) in
    let mapping = Hashtbl.create !next_id in
    if mdp then
      let nodes =
        Base.H_int.fold
          (fun vertex1 (_, label, _) acc ->
            let substrings =
              G.string_of_l label |> String.split_on_char ' '
            in
            let action = List.hd substrings in
            let reward = List.nth substrings 1 |> int_of_string in
            if Hashtbl.mem mapping (vertex1, action) then acc
            else (
              next_id := !next_id + 1;
              Hashtbl.add mapping (vertex1, action) !next_id;
              Printf.sprintf
                "%d [ label=<%s%s>, fontsize=6.0, id=\"s%d_%s\", \
                 fontname=\"monospace\", width=.40, height=.20, \
                 style=\"filled\" fillcolor=\"grey75\" ]; \n\
                 %d -> %d [ fontname=\"monospace\", fontsize=7.0, \
                 arrowhead=\"vee\", arrowsize=0.5 ];\n"
                !next_id action
                (generate_reward_html reward)
                vertex1 action vertex1 !next_id
              :: acc ))
          (G.edges g) []
      in
      (String.concat "" nodes, mapping)
    else ("", mapping)

  let construct_edge_label label reaction_rules =
    (if label = "" then "" else label ^ ", ") ^ reaction_rules

  let construct_node_label g i =
    let preds, preds_to_states = G.label g in
    let relevant_preds =
      Base.S_predicate.filter
        (fun pred ->
          Base.H_predicate.find_all preds_to_states pred |> List.mem i)
        preds
    in
    if Base.S_predicate.is_empty relevant_preds then string_of_int i
    else
      Base.S_predicate.elements relevant_preds
      |> List.split |> fst |> String.concat ", "

  let to_dot g ~path ~name =
    let rank = "{ rank=source; 0 };\n" in
    let mdp = is_mdp g in
    let actions, mapping = construct_action_nodes g mdp in
    let states =
      Base.H_int.fold
        (fun _ (i, _) buff ->
          let bolding = if i = 0 then ", style=\"bold\"" else "" in
          let label = construct_node_label g i in
          let filename = Printf.sprintf "%d.svg" i |> Filename.concat path in
          let reward = total_reward g i |> generate_reward_html in
          Printf.sprintf
            "%s%d [ label=<%s%s>, URL=\"%s\", fontsize=9.0, id=\"s%d\", \
             fontname=\"monospace\", width=.60, height=.30%s ];\n"
            buff i label reward filename i bolding)
        (G.states g) ""
    and edges =
      Base.H_int.fold
        (fun vertex1 (vertex2, label, reaction_rules) buff ->
          let label_str = G.string_of_l label in
          if mdp then
            let substrings = String.split_on_char ' ' label_str in
            let action = List.hd substrings in
            let probability = List.nth substrings 2 in
            let action_node = Hashtbl.find mapping (vertex1, action) in
            let edge_label =
              construct_edge_label probability reaction_rules
            in
            Printf.sprintf
              "%s%d -> %d [ label=\"%s\", fontname=\"monospace\", \
               fontsize=7.0,arrowhead=\"vee\", arrowsize=0.5 ];\n"
              buff action_node vertex2 edge_label
          else
            let edge_label = construct_edge_label label_str reaction_rules in
            Printf.sprintf
              "%s%d -> %d [ label=\"%s\", fontname=\"monospace\", \
               fontsize=7.0,arrowhead=\"vee\", arrowsize=0.5 ];\n"
              buff vertex1 vertex2 edge_label)
        (G.edges g) ""
    in
    Printf.sprintf
      "digraph \"%s\" {\nstylesheet = \"style_sbrs.css\"\n%s%s\n%s\n%s}" name
      rank states
      (if mdp then actions else "")
      edges

  let to_lab g =
    let sanitise s =
      Str.global_replace (Str.regexp_string "(") "_" s
      |> Str.global_replace (Str.regexp_string ",") "_"
      |> Str.global_replace (Str.regexp_string ")") ""
    in
    let preds, h = G.label g in
    let labs =
      Base.S_predicate.fold
        (fun (p, r) acc ->
          ( match Base.H_predicate.find_all h (p, r) with
          | [] -> "false"
          | xs ->
              List.map (fun s -> "x = " ^ string_of_int s) xs
              |> String.concat " | " )
          |> fun s ->
          "label \"" ^ sanitise p ^ "\" = " ^ s |> fun s -> s :: acc)
        preds []
      |> List.rev |> String.concat ";\n"
    in
    if labs == "" then labs else labs ^ ";\n"

  let iter_states f g = Base.H_int.iter (fun _ (i, b) -> f i b) (G.states g)

  let fold_states f g =
    Base.H_int.fold (fun _ (i, b) acc -> f i b acc) (G.states g)

  let iter_edges f g =
    Base.H_int.iter (fun v (u, l, _) -> f v u l) (G.edges g)

  let fold_edges f g =
    Base.H_int.fold (fun v (u, l, _) acc -> f v u l acc) (G.edges g)
end

module type RS = sig
  type react

  type ac

  type p_class = P_class of react list | P_rclass of react list

  type graph

  type label

  type limit

  val typ : rs_type

  type react_error

  val string_of_react : react -> string

  val name : react -> string

  val lhs : react -> Big.t

  val rhs : react -> Big.t

  val label : react -> label

  val conds : react -> ac list

  val map : react -> Fun.t option

  val parse_react_unsafe :
    name:string ->
    lhs:Big.t ->
    rhs:Big.t ->
    ?conds:ac list ->
    label ->
    Fun.t option ->
    react

  val parse_react :
    name:string ->
    lhs:Big.t ->
    rhs:Big.t ->
    ?conds:ac list ->
    label ->
    Fun.t option ->
    react option

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

  val random_step :
    Big.t -> react list -> (Big.t * label * react list) option * int

  val apply : Big.t -> react list -> Big.t option

  val fix : Big.t -> react list -> Big.t * int

  val rewrite : Big.t -> p_class list -> Big.t * int

  exception MAX of graph * stats

  val bfs :
    s0:Big.t ->
    priorities:p_class list ->
    predicates:(Base.Predicate.t * Big.t) list ->
    max:int ->
    (int -> Big.t -> unit) ->
    graph * stats

  exception DEADLOCK of graph * stats * limit

  exception LIMIT of graph * stats

  val sim :
    s0:Big.t ->
    ?seed:int ->
    priorities:p_class list ->
    predicates:(Base.Predicate.t * Big.t) list ->
    init_size:int ->
    stop:limit ->
    (int -> Big.t -> unit) ->
    graph * stats

  val to_prism : graph -> string

  val to_state_rewards : graph -> string

  val to_transition_rewards : graph -> string

  val to_dot : graph -> path:string -> name:string -> string

  val to_lab : graph -> string

  val iter_states : (int -> Big.t -> unit) -> graph -> unit

  val fold_states : (int -> Big.t -> 'a -> 'a) -> graph -> 'a -> 'a

  val iter_edges : (int -> int -> label -> unit) -> graph -> unit

  val fold_edges : (int -> int -> label -> 'a -> 'a) -> graph -> 'a -> 'a
end

module Make
    (S : Solver.M)
    (R : React.T)
    (P : Priority.P with type r_t := R.t and type r_label := R.label)
    (L : L with type l = R.label)
    (G : G with type l = R.label)
    (K : K) =
struct
  type react = R.t

  type p_class = P.p_class = P_class of R.t list | P_rclass of R.t list

  type graph = G.t

  type label = R.label

  type limit = L.t

  include K

  exception MAX of graph * stats

  exception LIMIT of graph * stats

  exception DEADLOCK of graph * stats * limit

  (* Override some functions *)
  type react_error = R.react_error

  exception NOT_VALID of react_error

  let string_of_react = R.to_string

  let parse_react_unsafe = R.make

  let parse_react ~name ~lhs ~rhs ?conds:(c = []) l f =
    let r = R.make ~name ~lhs ~rhs ~conds:c l f in
    if R.is_valid r then Some r else None

  let string_of_limit = L.to_string

  let is_valid_react = R.is_valid

  let is_valid_react_exn r =
    try R.is_valid_exn r with R.NOT_VALID e -> raise (NOT_VALID e)

  let equal_react = R.equal

  let string_of_react_err = R.string_of_react_err

  let name = R.name

  let lhs = R.lhs

  let rhs = R.rhs

  let label = R.l

  let conds = R.conds

  let map = R.map

  let apply = R.apply

  let fix = R.fix

  let step = R.step

  let random_step = R.random_step

  let is_valid_priority = P.is_valid

  let is_valid_priority_list = P.is_valid_list

  let cardinal = P.cardinal

  let rewrite = P.rewrite

  let is_new b v =
    let k_buket = Base.H_int.find_all v (Big.key b) in
    try
      let old, _ = List.find (fun (_, b') -> S.equal_key b b') k_buket in
      Some old
    with Not_found -> None

  (* Partition a list of occurrences into new and old states *)
  let partition g i f_iter =
    List.fold_left
      (fun (new_acc, old_acc, i) (b, c, d) ->
        match is_new b (G.states g) with
        | None ->
            let i' = i + 1 in
            (* Stop here when i > max *)
            f_iter i' b;
            ((i', (b, c, d)) :: new_acc, old_acc, i')
        | Some x -> (new_acc, (x, c, d) :: old_acc, i))
      ([], [], i)

  (* Add labels for predicates *)
  let check (i, s) (_, h) =
    let t_trans = Big.(Place.trans s.p) in
    List.iter (fun (id, p) ->
        if S.Memo.occurs ~target:s ~pattern:p t_trans then
          Base.H_predicate.add h id i)

  (* Number of states in a graph *)
  let size_s g = Base.H_int.length (G.states g)

  (* Number of edges in a graph. *)
  let size_t g = Base.H_int.length (G.edges g)

  (* Turns a list of reaction rules into a string of names, sorting
     lexicographically and removing duplicates. *)
  let string_of_reaction_rules r =
    List.rev_map name r |> List.sort_uniq compare |> String.concat " | "

  let rec _bfs g q i m t0 (priorities : P.Memo.t list) predicates max iter_f
      =
    if not (Queue.is_empty q) then (
      if i > max then
        raise
          (MAX
             (g, stats_init ~t0 ~states:(size_s g) ~trans:(size_t g) ~occs:m))
      else
        let v, curr = Queue.pop q in
        let (new_s, old_s, i'), m' =
          P.Memo.scan (curr, i) (Place.trans curr.p)
            ~part_f:(partition g i iter_f) priorities
        in
        List.iter
          (fun (i, (b, l, r)) ->
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
        List.iter
          (fun (a, b, c) ->
            Base.H_int.add (G.edges g) v (a, b, string_of_reaction_rules c))
          old_s;
        (* recursive call *)
        _bfs g q i' (m + m') t0 priorities predicates max iter_f )
    else (g, stats_init ~t0 ~states:(size_s g) ~trans:(size_t g) ~occs:m)

  let bfs ~s0 ~priorities ~predicates ~max iter_f =
    (* Preprocess priorities to include automorphisms *)
    let priorities' = P.Memo.init priorities and q = Queue.create () in
    (* Apply rewriting to s0 *)
    let s0', m = P.Memo.rewrite s0 (Place.trans s0.p) priorities'
    and g = List.map (fun (id, _) -> id) predicates |> G.init max in
    Queue.push (0, s0') q;
    (* Add initial state *)
    iter_f 0 s0';
    Base.H_int.add (G.states g) (Big.key s0') (0, s0');
    check (0, s0') (G.label g) predicates;
    _bfs g q 0 m (Unix.gettimeofday ()) priorities' predicates max iter_f

  let rec _sim trace s i t_sim m t0 (priorities : P.Memo.t list) predicates
      t_max iter_f =
    if L.is_greater t_sim t_max then
      raise
        (LIMIT
           ( trace,
             stats_init ~t0 ~states:(size_s trace) ~trans:(size_t trace)
               ~occs:m ))
    else
      match P.Memo.scan_sim s (Place.trans s.p) priorities with
      | None, m' ->
          raise
            (DEADLOCK
               ( trace,
                 stats_init ~t0 ~states:(size_s trace) ~trans:(size_t trace)
                   ~occs:(m + m'),
                 t_sim ))
      | Some (s', l, r), m' ->
          iter_f (i + 1) s';
          Base.H_int.add (G.states trace) (Big.key s') (i + 1, s');
          check (i + 1, s') (G.label trace) predicates;
          Base.H_int.add (G.edges trace) i
            (i + 1, l, string_of_reaction_rules r);
          _sim trace s' (i + 1) (L.increment t_sim l) (m + m') t0 priorities
            predicates t_max iter_f

  let sim ~s0 ?seed ~priorities ~predicates ~init_size ~stop iter_f =
    (* Preprocess priorities to inlclude automorphisms *)
    let priorities' = P.Memo.init priorities in
    (match seed with None -> Random.self_init () | Some x -> Random.init x);
    (* Apply rewriting to s0 *)
    let s0', m = P.Memo.rewrite s0 (Place.trans s0.p) priorities'
    and trace =
      List.map (fun (id, _) -> id) predicates |> G.init init_size
    in
    (* Add initial state *)
    iter_f 0 s0';
    Base.H_int.add (G.states trace) (Big.key s0') (0, s0');
    check (0, s0') (G.label trace) predicates;
    _sim trace s0' 0 L.init m (Unix.gettimeofday ()) priorities' predicates
      stop iter_f

  include MakeE (G)
end
