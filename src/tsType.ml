module type G = sig
  type t
  type l
  val init : int -> Base.Predicate.t list -> t
  val states : t -> (int * Big.t) Base.H_int.t
  val label : t -> (Base.S_predicate.t * int Base.H_predicate.t)
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

  (* Convert a hash table of edges into a sorted list of tuples with action
     names and edge labels. Also return whether there are any non-empty action
     names (i.e. whether we should be building an MDP). *)
  let generate_actions_and_labels g =
    let mdp = ref false in
    let edges = Base.H_int.fold (fun vertex1 (vertex2, edge_label, _) acc ->
        let action, label = match G.string_of_l edge_label with
          | "" -> "", ""
          | s ->
            if String.contains s ' ' then
              begin
                mdp := true ;
                let substrings = String.split_on_char ' ' s in
                List.hd substrings, List.nth substrings 2
              end
            else "", s
        in
        (vertex1, action, vertex2, label) :: acc)
        (G.edges g) [] |> List.fast_sort compare in
    edges, !mdp

  (* Replace action names with strings of numbers. The first action of each
     vertex gets number 0, and so on. The list is assumed to be sorted. Also
     return the number of choices, i.e., the sum of the numbers of distinct
     actions per vertex. *)
  let ints_of_actions edges =
    let previous_vertex = ref (-1) in
    let previous_action = ref "!" in
    let previous_number = ref (-1) in
    let choices = ref 0 in
    let edges = List.map (fun (vertex1, action, vertex2, label) ->
        if vertex1 <> !previous_vertex then
          begin
            previous_vertex := vertex1;
            previous_action := "!";
            previous_number := (-1);
          end ;
        if action <> !previous_action then
          begin
            previous_action := action;
            previous_number := !previous_number + 1;
            choices := !choices + 1;
          end ;
        (vertex1, string_of_int !previous_number, vertex2, label)
      ) edges in
    edges, !choices

  let to_prism g =
    let states = Base.H_int.length (G.states g) in
    let transitions = Base.H_int.length (G.edges g) in
    let edges, mdp = generate_actions_and_labels g in
    let edges, choices = if mdp then ints_of_actions edges else edges, 0 in
    List.map (fun (vertex1, action, vertex2, label) ->
        (string_of_int vertex1)
        ^ (if action = "" then "" else " " ^ action)
        ^ " "
        ^ (string_of_int vertex2)
        ^ (if label = "" then "" else " " ^ label)
      ) edges
    |> List.append [ (string_of_int states)
                     ^ (if mdp then " " ^ string_of_int choices else "")
                     ^ " "
                     ^ (string_of_int transitions) ]
    |> String.concat "\n"

  (* Calculate the total reward of a state *)
  let total_reward g i =
    let (preds, preds_to_states) = G.label g in
    let relevant_preds = Base.S_predicate.filter (fun pred ->
    Base.H_predicate.find_all preds_to_states pred |> List.mem i) preds in
    Base.S_predicate.fold (fun (_, reward) sum -> reward + sum) relevant_preds 0

  let to_state_rewards g =
    let states = G.states g in
    let rewards = Base.H_int.fold (fun _ (i, _) acc ->
        let reward = total_reward g i in
        if reward = 0 then acc else Printf.sprintf "%d %d" i reward :: acc
      ) states [] |> List.fast_sort compare in
    List.append [Printf.sprintf "%d %d" (Base.H_int.length states)
                   (List.length rewards)] rewards
    |> String.concat "\n"

  (* Check if there are any edges with action labels *)
  let is_mdp g =
    Base.H_int.fold (fun _ (_, label, _) answer ->
        answer || String.contains (G.string_of_l label) ' '
      ) (G.edges g) false

  let generate_reward_html reward =
    let (color, sign) = if reward > 0 then "darkgreen", "+" else "red", "" in
    if reward = 0 then ""
    else Printf.sprintf "<br/><font color='%s'>%s%d</font>" color sign reward

  (* Return a DOT string of action nodes of an MDP as well as edges to them,
     and a hash table mapping (vertex, action) pairs to the IDs of the
     actions *)
  let construct_action_nodes g =
    let next_id = ref (Base.H_int.length (G.states g) - 1) in
    let mapping = Hashtbl.create !next_id in
    let nodes = Base.H_int.fold (fun vertex1 (_, label, _) acc ->
        let substrings = G.string_of_l label |> String.split_on_char ' ' in
        let action = List.hd substrings in
        let reward = List.nth substrings 1 |> int_of_string in
        if Hashtbl.mem mapping (vertex1, action) then acc
        else
          begin
            next_id := !next_id + 1;
            Hashtbl.add mapping (vertex1, action) !next_id ;
            Printf.sprintf "%d [ label=<%s%s>, fontsize=6.0, id=\"s%d_%s\", \
                            fontname=\"monospace\", width=.40, height=.20, \
                            style=\"filled\" fillcolor=\"grey75\" ];\
                            \n%d -> %d [ fontname=\"monospace\", fontsize=7.0,\
                            arrowhead=\"vee\", arrowsize=0.5 ];\n"
              !next_id action (generate_reward_html reward) vertex1 action
              vertex1 !next_id  :: acc
          end
      ) (G.edges g) [] in
    String.concat "" nodes, mapping

  let construct_edge_label label reaction_rules =
    (if label = "" then "" else label ^ ", ") ^ reaction_rules

  let construct_node_label g i =
    let (preds, preds_to_states) = G.label g in
    let relevant_preds = Base.S_predicate.filter (fun pred ->
        Base.H_predicate.find_all preds_to_states pred |> List.mem i) preds in
    if Base.S_predicate.is_empty relevant_preds then string_of_int i
    else Base.S_predicate.elements relevant_preds
         |> List.split
         |> fst
         |> String.concat ", "

  let to_dot g ~path ~name =
    let rank = "{ rank=source; 0 };\n" in
    let mdp = is_mdp g in
    let actions, mapping = construct_action_nodes g in
    let states =
      Base.H_int.fold (fun _ (i, _) buff ->
          let bolding = if i = 0 then ", style=\"bold\"" else "" in
          let label = construct_node_label g i in
          let filename = Printf.sprintf "%d.svg" i |> Filename.concat path in
          let reward = total_reward g i |> generate_reward_html in
          Printf.sprintf
            "%s%d [ label=<%s%s>, URL=\"%s\", fontsize=9.0, \
             id=\"s%d\", fontname=\"monospace\", width=.60, height=.30%s ];\n"
            buff i label reward filename i bolding)
        (G.states g) ""
    and edges =
      Base.H_int.fold (fun vertex1 (vertex2, label, reaction_rules) buff ->
          let label_str = G.string_of_l label in
          if mdp then
            let substrings = String.split_on_char ' ' label_str in
            let action = List.hd substrings in
            let probability = List.nth substrings 1 in
            let action_node = Hashtbl.find mapping (vertex1, action) in
            let edge_label = construct_edge_label probability reaction_rules in
            Printf.sprintf
              "%s%d -> %d [ label=\"%s\", fontname=\"monospace\", fontsize=7.0,\
               arrowhead=\"vee\", arrowsize=0.5 ];\n"
              buff action_node vertex2 edge_label
          else
            let edge_label = construct_edge_label label_str reaction_rules in
            Printf.sprintf
              "%s%d -> %d [ label=\"%s\", fontname=\"monospace\", fontsize=7.0,\
               arrowhead=\"vee\", arrowsize=0.5 ];\n"
              buff vertex1 vertex2 edge_label
        ) (G.edges g) "" in
    Printf.sprintf "digraph \"%s\" {\n\
                    stylesheet = \"style_sbrs.css\"\n%s%s\n%s\n%s}"
      name rank states (if mdp then actions else "") edges

  let to_lab g =
    let (preds, h) = G.label g in
    Base.S_predicate.fold (fun (p, r) acc ->
        (match Base.H_predicate.find_all h (p, r) with
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
  val name : react -> string
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
    predicates:(Base.Predicate.t * Big.t) list ->
    max:int -> iter_f:(int -> Big.t -> unit) -> graph * Stats.t
  exception DEADLOCK of graph * Stats.t * limit
  exception LIMIT of graph * Stats.t
  val sim :
    s0:Big.t ->
    priorities:p_class list ->
    predicates:(Base.Predicate.t * Big.t) list
    -> init_size:int -> stop:limit -> iter_f:(int -> Big.t -> unit)
    -> graph * Stats.t
  val to_prism : graph -> string
  val to_state_rewards : graph -> string
  val to_dot : graph -> path:string -> name:string -> string
  val to_lab : graph -> string
  val iter_states : (int -> Big.t -> unit) -> graph -> unit
  val fold_states : (int -> Big.t -> 'a -> 'a) -> graph -> 'a -> 'a
  val iter_edges : (int -> int -> label -> unit) -> graph -> unit
  val fold_edges : (int -> int -> label -> 'a -> 'a) -> graph -> 'a -> 'a
  val parse_react_unsafe : name:string -> lhs:Big.t -> rhs:Big.t ->
    label -> Fun.t option -> react
  val parse_react : name:string -> lhs:Big.t -> rhs:Big.t ->
    label -> Fun.t option -> react option
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

  let parse_react ~name ~lhs ~rhs l f =
    let r = R.parse ~name ~lhs ~rhs l f in
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

  let name = R.name

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
          Base.H_predicate.add h id i)

  (* Number of states in a graph *)
  let size_s g =
    Base.H_int.length (G.states g)

  (* NUmber of edges in a graph. *)
  let size_t g =
    Base.H_int.length (G.edges g)

  (* Turns a list of reaction rules into a string of names, sorting
     lexicographically and removing duplicates. *)
  let string_of_reaction_rules r =
    List.map name r |> List.sort_uniq compare |> String.concat " | "

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
    and g = List.map (fun (id, _) -> id) predicates
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
    and trace = List.map (fun (id, _) -> id) predicates
                |> G.init init_size in
    (* Add initial state *)
    iter_f 0 s0';
    Base.H_int.add (G.states trace) (Big.key s0') (0, s0');
    check (0, s0') (G.label trace) predicates;
    _sim trace s0' 0 L.init m (Unix.gettimeofday ())
      priorities predicates stop iter_f

  include MakeE (G)

end
