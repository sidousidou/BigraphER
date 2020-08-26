open Base
open Printf

(* ide strings no capital letter or number at the start *)
type name = Name of string

module Face =
  S_opt
    (Set.Make (struct
      type t = name

      let compare (Name s0) (Name s1) = String.compare s0 s1
    end))
    (struct
      type t = name

      let pp out (Name s) = Format.pp_print_string out s
    end)

(* Module used to compute equivalence classes *)
module Face_set = Set.Make (struct
  type t = Face.t

  let compare = Face.compare
end)

(* Map nodes to their port arity *)
module Ports = struct
  type t = int M_int.t

  let choose = M_int.choose

  let compare = M_int.compare

  let empty = M_int.empty

  let equal = M_int.equal Base.int_equal

  let filter = M_int.filter

  let fold = M_int.fold

  let is_empty = M_int.is_empty

  let iter = M_int.iter

  let max_binding = M_int.max_binding

  let min_binding = M_int.min_binding

  let of_list =
    List.fold_left (fun res (i, a) -> M_int.add i a res) M_int.empty

  let to_string ps =
    "{"
    ^ ( M_int.bindings ps
      |> List.map (fun (a, b) ->
             "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")")
      |> String.concat ", " )
    ^ "}"

  let pp =
    M_int.pp ~open_b:Format.pp_open_hbox
      ~first:(fun out -> Format.pp_print_string out "{")
      ~last:(fun out -> Format.pp_print_string out "}")
      ~sep:(fun out ->
        Format.pp_print_string out ",";
        Format.pp_print_space out ())
      Format.pp_print_int

  let of_string s =
    try
      match s with
      | "" -> empty
      | _ ->
          Base.remove_block_delims s
          |> Str.split (Str.regexp_string "), (")
          |> List.map (fun p ->
                 Str.split (Str.regexp_string ", ") p |> function
                 | a :: b :: _ -> (int_of_string a, int_of_string b)
                 | _ -> invalid_arg "")
          |> List.fold_left (fun acc (a, b) -> M_int.add a b acc) empty
    with Invalid_argument _ ->
      invalid_arg "Not a valid string representation of a set of ports"

  (* Transform a set of nodes in a set of ports *)
  let of_nodes ns =
    Nodes.fold (fun n c acc -> M_int.add n (Ctrl.arity c) acc) ns empty

  let to_IntSet ps = fold (fun i _ res -> IntSet.add i res) ps IntSet.empty

  let apply iso s =
    fold
      (fun i p acc ->
        match Iso.apply iso i with
        | Some i' -> M_int.add i' p acc
        | None -> acc)
      s empty

  let arity ps i = M_int.find i ps

  let offset ps n = fold (fun i ar acc -> M_int.add (i + n) ar acc) ps empty

  let add i ps =
    match M_int.find i ps with
    | Some a -> M_int.add i (a + 1) ps
    | None -> M_int.add i 1 ps

  let sum =
    M_int.merge (fun _ l r ->
        match (l, r) with
        | Some ar, Some ar' -> Some (ar + ar')
        | Some ar, None | None, Some ar -> Some ar
        | None, None -> None)

  let cardinal ps = fold (fun _ ar acc -> ar + acc) ps 0

  let fold_arities f =
    let rec rep n f x acc =
      if n = 0 then acc else rep (n - 1) f x (f x acc)
    in
    fold (fun v ar acc -> rep ar f v acc)
end

let string_of_name (Name s) = s

let parse_face =
  List.fold_left (fun acc x -> Face.add (Name x) acc) Face.empty

let string_of_face f =
  "{"
  ^ (Face.elements f |> List.map string_of_name |> String.concat ", ")
  ^ "}"

let pp_face =
  Face.pp ~open_b:Format.pp_open_hbox
    ~first:(fun out -> Format.pp_print_string out "{")
    ~last:(fun out -> Format.pp_print_string out "}")
    ~sep:(fun out ->
      Format.pp_print_string out ",";
      Format.pp_print_space out ())

let face_of_string s =
  try Str.(split (regexp_string ", ")) s |> parse_face
  with _ -> invalid_arg "Not a valid string representation of a face"

(* (in, out, ports) *)
type edg = { i : Face.t; o : Face.t; p : Ports.t }

let edg_compare (h : edg) (k : edg) =
  match Face.compare h.i k.i with
  | 0 -> (
      match Face.compare h.o k.o with
      | 0 -> (
          match Ports.compare int_compare h.p k.p with
          (* Structural equality to allow duplicates *)
          | 0 -> if h.p == k.p then 0 else 1
          | x -> x )
      | x -> x )
  | x -> x

let edg_is_empty e =
  Face.is_empty e.i && Face.is_empty e.o && Ports.is_empty e.p

let string_of_edge e =
  "(" ^ string_of_face e.i ^ ", " ^ string_of_face e.o ^ ", "
  ^ Ports.to_string e.p ^ ")"

let pp_edge out e =
  let open Format in
  fprintf out "@[(%a, %a, %a)@]" pp_face e.i pp_face e.o Ports.pp e.p

let edge_of_string s =
  try
    ( Base.remove_block_delims s
    |> Base.remove_block_delims
    |> Str.(split_delim (regexp_string "}, {")) )
    |> function
    | [ i; o; p ] ->
        { i = face_of_string i; o = face_of_string o; p = Ports.of_string p }
    | _ -> assert false
  with _ -> invalid_arg "Not a valid string representation of an edge"

module Lg = struct
  include S_opt
            (Set.Make (struct
              type t = edg

              let compare = edg_compare
            end))
            (struct
              type t = edg

              let pp = pp_edge
            end)

  let add e l = if edg_is_empty e then l else add e l

  let singleton e = if edg_is_empty e then empty else singleton e
end

(* tensor product fails (inner common names, outer common names)*)
exception NAMES_ALREADY_DEFINED of (Face.t * Face.t)

(* Composition fails *)
exception FACES_MISMATCH of (Face.t * Face.t)

let to_string l =
  Lg.elements l |> List.map string_of_edge |> String.concat "\n"

let pp =
  Lg.pp
    ~open_b:(fun out () -> Format.pp_open_vbox out 0)
    ~first:(fun _ -> ())
    ~last:(fun _ -> ())
    ~sep:(fun out -> Format.pp_print_cut out ())

let of_string s =
  Str.split (Str.regexp_string "\n") s
  |> List.map edge_of_string
  |> List.fold_left (fun acc e -> Lg.add e acc) Lg.empty

let arities lg = Lg.fold (fun e acc -> Ports.sum e.p acc) lg Ports.empty

let parse_nodes s m =
  Str.split (Str.regexp_string " ") s
  |> List.fold_left
       (fun (acc, i) token ->
         let ar = match M_int.find i m with None -> 0 | Some v -> v in
         (Nodes.add i (Ctrl.parse_name token ar) acc, i + 1))
       (Nodes.empty, 0)
  |> fst

let parse ~links:lines ~nodes =
  let build_edge s n =
    let a = Str.split (Str.regexp_string " ") s in
    {
      p =
        List.rev a |> List.tl
        |> List.map (fun x -> int_of_string x - 1)
        |> List.fold_left (flip Ports.add) Ports.empty;
      i = Face.empty;
      o =
        ( match List.nth a (List.length a - 1) with
        | "t" -> parse_face [ "n" ^ string_of_int n ]
        | _ -> Face.empty );
    }
  in
  let l =
    List.fold_left
      (fun (acc, i) l -> (Lg.add (build_edge l i) acc, i + 1))
      (Lg.empty, 0) lines
    |> fst
  in
  (l, arities l |> parse_nodes nodes)

(* Elementary substitution: one edge without ports *)
let elementary_sub ~inner ~outer =
  Lg.singleton { i = inner; o = outer; p = Ports.empty }

(* Node index is 0. Ports are from 0 to |f| - 1 *)
let elementary_ion f =
  Face.fold
    (fun n acc ->
      Lg.add
        { i = Face.empty; o = Face.singleton n; p = Ports.add 0 Ports.empty }
        acc)
    f Lg.empty

let inner (lg : Lg.t) =
  Lg.fold (fun e acc -> Face.union e.i acc) lg Face.empty

let outer (lg : Lg.t) =
  Lg.fold (fun e acc -> Face.union e.o acc) lg Face.empty

(* Add offset m to all the port indices *)
let offset (lg : Lg.t) (n : int) =
  Lg.fold
    (fun e acc -> Lg.add { e with p = Ports.offset e.p n } acc)
    lg Lg.empty

(* n0 is necessary because some nodes my be present in the left place      *)
(* graph but not in the link graph.                                        *)
let tens lg0 lg1 n0 =
  let i_in = Face.inter (inner lg0) (inner lg1)
  and i_out = Face.inter (outer lg0) (outer lg1) in
  if Face.is_empty i_in && Face.is_empty i_out then
    Lg.union lg0 (offset lg1 n0)
  else raise (NAMES_ALREADY_DEFINED (i_in, i_out))

(* Identity. One edge for every name in f *)
let elementary_id f =
  Face.fold
    (fun n acc ->
      Lg.union
        (Lg.singleton
           { i = Face.singleton n; o = Face.singleton n; p = Ports.empty })
        acc)
    f Lg.empty

let id_empty = elementary_id Face.empty

let is_id l =
  Lg.for_all
    (fun e ->
      Face.equal e.i e.o && Face.cardinal e.i = 1 && Ports.is_empty e.p)
    l

(* Merge two sets of equivalence classes *)
let equiv_class a b =
  let u = Face_set.union a b in
  let rec fix_point s res =
    try
      (* Smallest face in set s *)
      let f = Face_set.min_elt s in
      (* set s without face f *)
      let new_s = Face_set.remove f s in
      try
        (* Largest face having names in common with f *)
        let c =
          Face_set.max_elt
            (Face_set.filter
               (fun x -> not (Face.is_empty (Face.inter x f)))
               new_s)
        in
        let new_c = Face.union c f in
        fix_point (Face_set.add new_c (Face_set.remove c new_s)) res
      with _ -> fix_point new_s (Face_set.add f res)
    with _ -> res
  in
  fix_point u Face_set.empty

(* Merge a set of edges *)
let merge lg =
  {
    i = inner lg;
    o = outer lg;
    p = Lg.fold (fun e acc -> Ports.sum e.p acc) lg Ports.empty;
  }

(* Merge a set of edges on the inner face according to equivalence classes *)
let merge_in lg cls =
  Face_set.fold
    (fun f acc ->
      Lg.add
        (merge
           (Lg.filter (fun e -> not (Face.is_empty (Face.inter f e.i))) lg))
        acc)
    cls Lg.empty

(* Merge a set of edges on the outer face according to equivalence classes *)
let merge_out lg cls =
  Face_set.fold
    (fun f acc ->
      Lg.add
        (merge
           (Lg.filter (fun e -> not (Face.is_empty (Face.inter f e.o))) lg))
        acc)
    cls Lg.empty

(* Fuse two link graphs on common names. Note, mediating interfaces are
   assumed equal. *)
let fuse a b =
  Lg.fold
    (fun e acc ->
      let h =
        Lg.filter (fun h -> Face.equal h.o e.i) b |> Lg.choose |> safe
      in
      let new_e = { i = h.i; o = e.o; p = Ports.sum e.p h.p } in
      Lg.add new_e acc)
    a Lg.empty

(* Composition A o B. [n] is the number of nodes in A. *)
let comp a b n =
  let x = inner a and y = outer b in
  if Face.equal x y then
    let new_b = offset b n
    and cls_in =
      Lg.fold (fun e acc -> Face_set.add e.i acc) a Face_set.empty
    in
    let cls_out =
      Lg.fold (fun e acc -> Face_set.add e.o acc) new_b Face_set.empty
    in
    let cls = equiv_class cls_in cls_out in
    Lg.union
      (fuse (merge_in a cls) (merge_out new_b cls))
      (Lg.union
         (Lg.filter (fun e -> Face.is_empty e.i) a)
         (Lg.filter (fun e -> Face.is_empty e.o) new_b))
  else raise (FACES_MISMATCH (x, y))

(* no inner names that are siblings *)
let is_mono l = Lg.for_all (fun e -> Face.cardinal e.i < 2) l

(* no idle outer names *)
let is_epi l =
  Lg.for_all
    (fun e ->
      if Face.cardinal e.o > 0 then
        Face.cardinal e.i > 0 || not (Ports.is_empty e.p)
      else true)
    l

let is_guard l =
  (* true if inner are connected to outer *)
  not
    (Lg.exists
       (fun e -> (not (Face.is_empty e.i)) && not (Face.is_empty e.o))
       l)

let is_ground l = Face.is_empty (inner l)

(* Rename names in f_i and f_o *)
let rename_shared l i f_i f_o =
  let rename_face f i shr =
    Face.fold
      (fun n acc ->
        if Face.mem n shr then
          Face.add (Name (sprintf "%d%s" i (string_of_name n))) acc
        else Face.add n acc)
      f Face.empty
  in
  Lg.fold
    (fun e acc ->
      let new_e =
        { i = rename_face e.i i f_i; o = rename_face e.o i f_o; p = e.p }
      in
      Lg.add new_e acc)
    l Lg.empty

(* Duplicate the names in a face and build the substitutions *)
let dup_out f =
  Face.fold
    (fun n acc ->
      tens
        (elementary_sub
           ~inner:
             (Face.add
                (Name (sprintf "0%s" (string_of_name n)))
                (Face.singleton (Name (sprintf "1%s" (string_of_name n)))))
           ~outer:(Face.singleton n))
        acc 0)
    f Lg.empty

let dup_in f =
  Face.fold
    (fun n acc ->
      tens
        (elementary_sub ~inner:(Face.singleton n)
           ~outer:
             (Face.add
                (Name (sprintf "0%s" (string_of_name n)))
                (Face.singleton (Name (sprintf "1%s" (string_of_name n))))))
        acc 0)
    f Lg.empty

(* Parallel product *)
let ppar a b n =
  let shared_out = Face.inter (outer a) (outer b)
  and shared_in = Face.inter (inner a) (inner b) in
  let new_a = rename_shared a 0 shared_in shared_out
  and new_b = rename_shared b 1 shared_in shared_out in
  let a_b = tens new_a new_b n
  and f_out = Face.union (outer a) (outer b)
  and f_in = Face.union (inner a) (inner b) in
  let wiring_out =
    tens (elementary_id (Face.diff f_out shared_out)) (dup_out shared_out) 0
  in
  let wiring_in =
    tens (elementary_id (Face.diff f_in shared_in)) (dup_in shared_in) 0
  in
  comp wiring_out (comp a_b wiring_in n) 0

let apply i l =
  Lg.fold
    (fun e acc -> Lg.add { i = e.i; o = e.o; p = Ports.apply i e.p } acc)
    l Lg.empty

(* Is e a hyperedge? An extra node is not required when it is an edge or an
   idle name.*)
let is_hyp e =
  (* closure on a port *)
  (Ports.cardinal e.p = 1 && Face.is_empty e.i && Face.is_empty e.o)
  (* idle alias on two names *)
  || (Face.is_empty e.i && Ports.is_empty e.p && Face.cardinal e.o = 2)
  (* closure on two names *)
  || (Face.is_empty e.o && Ports.is_empty e.p && Face.cardinal e.i = 2)
  (* more than 2 ports or names *)
  || Ports.cardinal e.p + Face.cardinal e.i + Face.cardinal e.o > 2

(* is e an idle name? *)
let is_idle e =
  (* inner name *)
  (Face.is_empty e.o && Ports.is_empty e.p && Face.cardinal e.i = 1)
  (* outer name *)
  || (Face.is_empty e.i && Ports.is_empty e.p && Face.cardinal e.o = 1)

let is_closed e = Face.is_empty e.i && Face.is_empty e.o

(* Normalise link graph l as follows: l = omega o l' where omega is a linking
   and l' is the same as l but with all the links open. *)
let norm l =
  (* Normalise an edge. Outer names are numbered starting from i. *)
  let norm_edge e i =
    Face.fold
      (fun y (l, f, i) ->
        let x = Face.singleton (Name (sprintf "~%i" i)) in
        ( tens (elementary_sub ~inner:(Face.singleton y) ~outer:x) l 0,
          Face.union x f,
          i + 1 ))
      e.i (Lg.empty, Face.empty, i)
    |> Ports.fold
         (fun v _ (l, f, i) ->
           let x = Face.singleton (Name (sprintf "~%i" i)) in
           ( Lg.add { i = Face.empty; o = x; p = Ports.add v Ports.empty } l,
             Face.union x f,
             i + 1 ))
         e.p
  in
  Lg.fold
    (fun e (omega, l, i) ->
      let l_e, xs, i' = norm_edge e i in
      ( Lg.union (elementary_sub ~inner:xs ~outer:e.o) omega,
        Lg.union l l_e,
        i' ))
    l (Lg.empty, Lg.empty, 0)
  |> fun (omega, l, _) -> (omega, l)

let get_dot l =
  match
    Lg.fold
      (fun e (i, buff_i, buff_o, buff_h, buff_adj) ->
        let flag = is_hyp e in
        ( (* Edge index *)
          i + 1,
          (* Inner names *)
          Face.fold
            (fun n buff ->
              sprintf
                "%s\"i%s\" [ shape=plaintext, label=\"%s\", width=.18, \
                 height=.18, fontname=\"serif\", fontsize=9.0 ];\n"
                buff (string_of_name n) (string_of_name n))
            e.i buff_i,
          (* Outer names *)
          Face.fold
            (fun n buff ->
              sprintf
                "%s\"o%s\" [ shape=plaintext, label=\"%s\", width=.18, \
                 height=.18, fontname=\"serif\", fontsize=9.0 ];\n"
                buff (string_of_name n) (string_of_name n))
            e.o buff_o,
          (* Hyperedges *)
          ( if flag then
            sprintf
              "%se%d [ shape=none, label=\"\", width=0, height=0, margin=0, \
               color=green ];\n"
              buff_h i
          else buff_h ),
          (* Adjacency *)
          if flag then
            buff_adj
            ^ Face.fold
                (fun n buff ->
                  sprintf "%s\"i%s\" -> e%d [ headclip=false ];\n" buff
                    (string_of_name n) i)
                e.i ""
            ^ Face.fold
                (fun n buff ->
                  sprintf "%s\"o%s\" -> e%d [ headclip=false ];\n" buff
                    (string_of_name n) i)
                e.o ""
            ^
            if
              Ports.cardinal e.p = 1
              && Face.is_empty e.i && Face.is_empty e.o
            then
              (* closure from a port *)
              sprintf
                "e%d -> v%d [ dir=both, arrowtail=tee, tailclip=false, \
                 arrowsize=0.7, weight=5 ];\n"
                i
                (fst @@ safe @@ Ports.choose e.p)
            else
              Ports.fold_arities
                (fun v buff ->
                  sprintf "%se%d -> v%d [dir=both, tailclip=false ];\n" buff
                    i v)
                e.p ""
          else if (* idle name *)
                  is_idle e then buff_adj
          else if (* edge between two points *)
                  Ports.is_empty e.p then
            (* name -> name *)
            sprintf "%s\"i%s\" -> \"o%s\";\n" buff_adj
              (string_of_name @@ safe @@ Face.choose e.i)
              (string_of_name @@ safe @@ Face.choose e.o)
          else if Face.is_empty e.o then
            if Face.is_empty e.i then
              (* port -> port *)
              sprintf "%sv%d -> v%d [ dir=both, constraint=false ];\n"
                buff_adj
                (fst @@ safe @@ Ports.min_binding e.p)
                (fst @@ safe @@ Ports.max_binding e.p)
            else
              (* inner name -> port *)
              sprintf "%s\"i%s\" -> v%d;\n" buff_adj
                (string_of_name @@ safe @@ Face.choose e.i)
                (fst @@ safe @@ Ports.choose e.p)
          else
            (* port -> outer name *)
            sprintf "%sv%d -> \"o%s\" [ dir=back ];\n" buff_adj
              (fst @@ safe @@ Ports.choose e.p)
              (string_of_name @@ safe @@ Face.choose e.o) ))
      l
      ( 0,
        "",
        "",
        "",
        "edge [ color=green, arrowhead=none, arrowtail=none, arrowsize=0.3 ];\n"
      )
  with
  | _, a, b, c, d -> (a, b, c, d)

(* decompose t. p is assumed epi and mono. PortSet are normalised. i_c and
   i_d are isos from t to c and d.*)
let decomp ~target ~pattern ~i_e ~i_c ~i_d f_e =
  (* compute sets of nodes in c and d *)
  let v_c, v_d = (IntSet.iso_dom i_c, IntSet.iso_dom i_d)
  (* Introduce indices *)
  and t_a = Array.of_list (Lg.elements target)
  and p_a = Array.of_list (Lg.elements pattern)
  (* Inverse isos: T -> P *)
  and i_e' = Iso.inverse i_e
  and f_e' = Fun.inverse f_e in
  (* Not a function *)
  (* Domains: disjoint subsets of T *)
  let closed_t = IntSet.iso_dom i_e' and non_empty_t = Rel.dom f_e' in
  (* Split every edge indexed by n in edges in d, edges in c, id. *)
  let c, d, b_id, _ =
    Array.fold_left
      (fun (acc_c, acc_d, acc_id, n) e ->
        (* n is an edge in a match with an edge in P *)
        if IntSet.mem n closed_t then (acc_c, acc_d, acc_id, n + 1)
        else
          (* n needs to be split *)
          let p_d = Ports.filter (fun x _ -> IntSet.mem x v_d) e.p
          and p_c = Ports.filter (fun x _ -> IntSet.mem x v_c) e.p
          and in_c, out_d =
            (* n is a link/edge in a match with link in P *)
            if IntSet.mem n non_empty_t then
              let match_p =
                IntSet.fold
                  (fun i acc -> Lg.add p_a.(i) acc)
                  (Rel.apply f_e' n) Lg.empty
              in
              (outer match_p, inner match_p)
            else (Face.empty, Face.empty)
          in
          (* d and p *)
          if Face.is_empty e.o && Ports.is_empty p_c && Face.is_empty in_c
          then
            ( acc_c,
              Lg.add { i = e.i; o = out_d; p = Ports.apply i_d p_d } acc_d,
              acc_id,
              n + 1 )
          else if (* c and p *)
                  Face.is_empty e.i && Ports.is_empty p_d then
            ( Lg.add { i = in_c; o = e.o; p = Ports.apply i_c p_c } acc_c,
              acc_d,
              acc_id,
              n + 1 )
          else
            (* id *)
            let name = Face.singleton (Name (sprintf "~%d" n)) in
            ( Lg.add
                {
                  i = Face.union name in_c;
                  o = e.o;
                  p = Ports.apply i_c p_c;
                }
                acc_c,
              Lg.add
                {
                  i = e.i;
                  o = Face.union name out_d;
                  p = Ports.apply i_d p_d;
                }
                acc_d,
              Lg.add { i = name; o = name; p = Ports.empty } acc_id,
              n + 1 ))
      (Lg.empty, Lg.empty, Lg.empty, 0)
      t_a
  in
  (* printf "---- Decomposition\n\ *) (* T : %s\n\ *) (* P : %s\n\ *) (*
     iso_e : %s\n\ *) (* map_e : %s\n\ *) (* -----\n\ *) (* c : %s\n\ *) (* d
     : %s\n\ *) (* id : %s%!\n" *)
  (* (to_string t) (to_string p) (Iso.to_string i_e) (Iso.to_string f_e) *)
  (* (to_string c) (to_string d) (to_string b_id); *)
  (c, d, b_id)

let max_ports l =
  Lg.fold
    (fun e max ->
      let max' = Ports.cardinal e.p in
      if max' > max then max' else max)
    l 0

let cardinal_ports l =
  Lg.fold (fun e acc -> Ports.cardinal e.p :: acc) l []
  |> List.sort int_compare

let closed_edges l = Lg.filter is_closed l |> Lg.cardinal

let filter_iso f l =
  let l', _, _, iso =
    Lg.fold
      (fun e (acc, i, i', iso) ->
        if f e then (Lg.add e acc, i + 1, i' + 1, Iso.add i' i iso)
        else (acc, i + 1, i', iso))
      l
      (Lg.empty, 0, 0, Iso.empty)
  in
  (l', iso)

let closed_edges_iso l =
  (* Iso from closed link graph to full link graph *)
  filter_iso is_closed l

(* Prime components decomposition *)
let prime_components lg =
  List.map (fun iso ->
      Lg.fold
        (fun edg acc -> Lg.add { edg with p = Ports.apply iso edg.p } acc)
        lg Lg.empty)
