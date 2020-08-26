open Printf

type t = {
  p : Place.t;
  (* Place graph *)
  l : Link.Lg.t;
  (* Link graph *)
  n : Nodes.t; (* Set of nodes *)
}

type inter = Inter of int * Link.Face.t

exception SHARING_ERROR

exception COMP_ERROR of inter * inter

exception CTRL_ERROR of int * Link.Face.t

exception ISO_ERROR of int * int (* number of nodes, size domain *)

let inner b = Inter (b.p.Place.s, Link.inner b.l)

let outer b = Inter (b.p.Place.r, Link.outer b.l)

let inter_equal (Inter (i, n)) (Inter (j, m)) = i = j && Link.Face.equal n m

let ord_of_inter (Inter (i, _)) = i

let face_of_inter (Inter (_, f)) = f

let string_of_inter (Inter (n, f)) =
  "<" ^ string_of_int n ^ ", " ^ Link.string_of_face f ^ ">"

let pp_inter out (Inter (n, f)) =
  Format.fprintf out "@[%c%d,@ %a%c@]" '<' n Link.pp_face f '>'

let to_string b =
  List.filter
    (fun x -> String.compare "" x <> 0)
    [ Nodes.to_string b.n; Place.to_string b.p; Link.to_string b.l ]
  |> String.concat "\n"

let pp out b =
  Format.fprintf out "@[<v>%a@,%a" Nodes.pp b.n Place.pp b.p;
  if Link.Lg.is_empty b.l then Format.pp_close_box out ()
  else Format.fprintf out "@,%a@]" Link.pp b.l

let size b = Nodes.size b.n + Link.Lg.cardinal b.l

let parse s =
  let lines = Base.parse_lines s in
  let r, n, s, e =
    let a =
      Array.of_list (Str.split (Str.regexp_string " ") (List.hd lines))
    in
    ( int_of_string a.(0),
      int_of_string a.(1),
      int_of_string a.(2),
      int_of_string a.(3) )
  in
  let s_n, lines_p, lines_l, _ =
    List.fold_left
      (fun (s_n, acc_p, acc_l, i) l ->
        match i with
        | 0 -> (s_n, acc_p, acc_l, 1)
        | 1 -> (l, acc_p, acc_l, 2)
        | _ ->
            if i < 2 + r + n then (s_n, acc_p @ [ l ], acc_l, i + 1)
            else if i < 2 + r + n + e then (s_n, acc_p, acc_l @ [ l ], i + 1)
            else (s_n, acc_p, acc_l, i + 1))
      ("", [], [], 0) lines
  in
  assert (List.length lines_l = e);
  let l, s_n = Link.parse ~links:lines_l ~nodes:s_n in
  { n = s_n; p = Place.parse ~regions:r ~nodes:n ~sites:s lines_p; l }

let of_string s =
  let err = "Not a valid string representation of a bigraph" in
  let parse_p_header h =
    match Str.(split (regexp_string " ")) h with
    | r :: n :: s :: _ -> (int_of_string r, int_of_string n, int_of_string s)
    | _ -> invalid_arg err
  in
  let rec split_l n (a, b) =
    if n > 0 then
      match b with
      | x :: xs -> split_l (n - 1) (x :: a, xs)
      | [] -> assert false
    else (List.rev a, b)
  in
  match Base.parse_lines s with
  | n :: place_header :: rest ->
      let regions, nodes, sites = parse_p_header place_header in
      let m, l = split_l (regions + nodes) ([], rest) in
      {
        n = Nodes.of_string n;
        p = Place.parse ~regions ~nodes ~sites m;
        l = Link.of_string (String.concat "\n" l);
      }
  | _ -> invalid_arg err

let id (Inter (m, i)) =
  { n = Nodes.empty; p = Place.elementary_id m; l = Link.elementary_id i }

let id_eps = { n = Nodes.empty; p = Place.id0; l = Link.id_empty }

let merge n =
  { n = Nodes.empty; p = Place.elementary_merge n; l = Link.id_empty }

let split n =
  { n = Nodes.empty; p = Place.elementary_split n; l = Link.id_empty }

let one = { n = Nodes.empty; p = Place.one; l = Link.id_empty }

let zero = { n = Nodes.empty; p = Place.zero; l = Link.id_empty }

let sym (Inter (m, i)) (Inter (n, j)) =
  {
    n = Nodes.empty;
    p = Place.elementary_sym m n;
    l = Link.tens (Link.elementary_id i) (Link.elementary_id j) 0;
  }

let ion f c =
  {
    n = Nodes.add 0 c Nodes.empty;
    p = Place.elementary_ion;
    l = Link.elementary_ion f;
  }

let ion_chk f c =
  if Ctrl.arity c <> Link.Face.cardinal f then
    raise (CTRL_ERROR (Ctrl.arity c, f))
  else ion f c

let sub ~inner ~outer =
  { n = Nodes.empty; p = Place.id0; l = Link.elementary_sub ~inner ~outer }

let closure inner = sub ~inner ~outer:Link.Face.empty

let intro outer = sub ~inner:Link.Face.empty ~outer

(* (l list of parents for each site) r roots *)
let placing l r f =
  { n = Nodes.empty; p = Place.parse_placing l r; l = Link.elementary_id f }

(* Empty link graph and no nodes in the place graph. *)
let is_plc b =
  Link.Lg.equal b.l Link.id_empty && Nodes.size b.n = 0 && Place.is_plc b.p

(* Empty place graph and no nodes in the link graph. *)
let is_wir b = b.p = Place.id0 && Nodes.size b.n = 0

let is_id b = Nodes.is_empty b.n && Place.is_id b.p && Link.is_id b.l

let tens a b =
  {
    n = Nodes.tens a.n b.n;
    p = Place.tens a.p b.p;
    l = Link.tens a.l b.l (Nodes.size a.n);
  }

let tens_of_list = List.fold_left (fun acc b -> tens acc b) id_eps

let rec fold i n op f res =
  assert (i >= 0);
  assert (n >= i);
  if i < n then fold (i + 1) n op f (op res (f i)) else res

let tens_seq ~start ~stop f = fold start stop tens f id_eps

let comp a b =
  try
    {
      n = Nodes.tens a.n b.n;
      p = Place.comp a.p b.p;
      l = Link.comp a.l b.l (Nodes.size a.n);
    }
  with Place.COMP_ERROR _ | Link.FACES_MISMATCH _ ->
    raise (COMP_ERROR (inner a, outer b))

let comp_of_list = List.fold_left (fun acc b -> comp acc b) id_eps

let comp_seq ~start ~stop f = fold start stop comp f id_eps

let ppar a b =
  {
    n = Nodes.tens a.n b.n;
    p = Place.tens a.p b.p;
    l = Link.ppar a.l b.l (Nodes.size a.n);
  }

let ppar_of_list = List.fold_left (fun acc b -> ppar acc b) id_eps

let ppar_seq ~start ~stop f = fold start stop ppar f id_eps

let par a b =
  let p = ppar a b in
  let out_p = outer p in
  comp
    (tens (merge (ord_of_inter out_p)) (id (Inter (0, face_of_inter out_p))))
    p

let par_of_list = List.fold_left (fun acc b -> par acc b) id_eps

let par_seq ~start ~stop f = fold start stop par f id_eps

let nest a b =
  let idx = id (Inter (0, face_of_inter (outer b))) in
  comp (ppar a idx) b

(* share f by psi in g check that psi is a placing *)
let share f psi g =
  if is_plc psi then
    comp (comp g (tens psi (id (Inter (0, face_of_inter (outer f)))))) f
  else raise SHARING_ERROR

(* not checking if f is a subset of b's outer names *)
let close f b =
  let g = Link.Face.diff (face_of_inter (outer b)) f
  and n = ord_of_inter (outer b)
  and cs =
    Link.Face.fold
      (fun n acc -> tens acc (closure (Link.Face.singleton n)))
      f id_eps
  in
  comp (tens cs (id (Inter (n, g)))) b

(* renaming through subsitution (o/i * b) *)
let rename ~inner:i ~outer:o b =
  let g = Link.Face.diff (face_of_inter (outer b)) i
  and n = ord_of_inter (outer b)
  and s = sub ~inner:i ~outer:o in
  comp (tens s (id (Inter (n, g)))) b

let atom f c = comp (ion f c) one

let atom_chk f c = comp (ion_chk f c) one

let is_mono b = Place.is_mono b.p && Link.is_mono b.l

let is_epi b = Place.is_epi b.p && Link.is_epi b.l

let is_guard b = Place.is_guard b.p && Link.is_guard b.l

let is_solid b = is_epi b && is_mono b && is_guard b

let is_ground b = Place.is_ground b.p && Link.is_ground b.l

(* TO DO *)
(*let latex_of_big = function | Bg (ns, p, l) -> "latex representation"*)

let apply i b =
  { n = Nodes.apply i b.n; p = Place.apply i b.p; l = Link.apply i b.l }

let to_dot b ide =
  let build_rank i flag =
    let ord = ord_of_inter i and f = face_of_inter i in
    if ord = 0 && Link.Face.is_empty f then ""
    else if flag then
      let ss =
        List.map
          (fun i -> sprintf "r%d" i)
          (IntSet.elements (IntSet.of_int ord))
        @ List.map
            (fun (Link.Name n) -> sprintf "\"o%s\"" n)
            (Link.Face.elements f)
      in
      match ss with
      | [] -> ""
      | _ -> sprintf "{ rank=source; %s };\n" (String.concat "; " ss)
    else
      let xs =
        List.map
          (fun i -> sprintf "s%d" i)
          (IntSet.elements (IntSet.of_int ord))
        @ List.map
            (fun (Link.Name n) -> sprintf "\"i%s\"" n)
            (Link.Face.elements f)
      in
      match xs with
      | [] -> ""
      | _ -> sprintf "{ rank=sink; %s };\n" (String.concat "; " xs)
  in
  let inner_shp, outer_shp, hyp_shp, link_adj = Link.get_dot b.l
  and roots_shp, sites_shp, node_ranks, place_adj = Place.get_dot b.p
  and nodes_shp = Nodes.to_dot b.n
  and rank_out = build_rank (outer b) true
  and rank_in = build_rank (inner b) false in
  sprintf
    "digraph \"%s\" {\n\
     newrank = true;\n\
     stylesheet = \"style.css\";\n\
     %s%s%s%s%s\n\
     %s%s%s%s%s%s}"
    ide roots_shp outer_shp rank_out hyp_shp nodes_shp sites_shp node_ranks
    inner_shp rank_in place_adj link_adj

let decomp ~target ~pattern ~i_n ~i_e f_e =
  let p_c, p_d, p_id, i_c, i_d =
    Place.decomp ~target:target.p ~pattern:pattern.p i_n
  in
  let l_c, l_d, l_id =
    Link.decomp ~target:target.l ~pattern:pattern.l ~i_e ~i_c ~i_d f_e
  and n_c, n_d = (Nodes.apply i_c target.n, Nodes.apply i_d target.n) in
  ( { p = p_c; l = l_c; n = n_c },
    { p = p_d; l = l_d; n = n_d },
    { p = p_id; l = l_id; n = Nodes.empty } )

type big_key = int

let key b =
  Hashtbl.hash
    ( b.p.Place.r,
      b.p.Place.s,
      Place.size b.p,
      IntSet.cardinal (Place.leaves b.p),
      IntSet.cardinal (Place.orphans b.p),
      Link.Lg.cardinal b.l,
      Link.closed_edges b.l,
      Link.string_of_face (Link.inner b.l),
      Link.string_of_face (Link.outer b.l),
      String.concat "~" (Nodes.norm b.n) )

let prime_components b =
  let pgs, isos = List.split (Place.prime_components b.p) in
  let lgs = Link.prime_components b.l isos in
  List.map
    (fun ((p, l), iso) -> { p; l; n = Nodes.apply iso b.n })
    (List.combine (List.combine pgs lgs) isos)

let instantiate eta b =
  let bs = prime_components b in
  Fun.fold
    (fun _ s acc ->
      try ppar acc (List.nth bs s)
      with Failure _ | Invalid_argument _ -> assert false
      (* eta is assumed total *))
    eta id_eps

(* Decomposition of argument D = D' x D_id *)
let decomp_d d id =
  let p_d, p_id, iso_d, iso_id = Place.decomp_d d.p id in
  let lgs = Link.prime_components d.l [ iso_d; iso_id ] in
  match lgs with
  | [ l_d; l_id ] ->
      ( { p = p_d; l = l_d; n = Nodes.apply iso_d d.n },
        { p = p_id; l = l_id; n = Nodes.apply iso_id d.n } )
  | _ -> assert false

let rewrite (i_n, i_e, f_e) ~s ~r0 ~r1 eta =
  let c, d, id = decomp ~target:s ~pattern:r0 ~i_n ~i_e f_e in
  match eta with
  | None -> comp c (comp (tens r1 id) d)
  | Some eta' ->
      if Fun.is_id eta' && Fun.is_surj (inner r0 |> ord_of_inter) eta' then
        comp c (comp (tens r1 id) d)
      else
        (* Normalise link graph *)
        let omega_l, d_norm_l = Link.norm d.l in
        let d_norm = { d with l = d_norm_l } in
        let d', d_id = inner id |> ord_of_inter |> decomp_d d_norm in
        let d'' = ppar (instantiate eta' d') d_id in
        let omega =
          {
            p = Place.elementary_id d''.p.Place.r;
            l = omega_l;
            n = Nodes.empty;
          }
        in
        comp omega d'' |> comp (tens r1 id) |> comp c
