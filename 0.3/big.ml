open Base

open Printf
  
type bg = {
  p : Place.pg;  (** Place graph *)
  l : Link.Lg.t; (** Link graph *)
  n : Base.Nodes.t; (** Set of nodes *)
}
type inter = | Inter of int * Link.Face.t

exception SHARING_ERROR
exception CTRL_ERROR of int * Link.Face.t
exception ISO_ERROR of int * int (* number of nodes, size domain *)
  
let inner b = Inter (b.p.Place.s, Link.inner b.l)
  
let outer b = Inter (b.p.Place.r, Link.outer b.l)
    
let inter_equals (Inter (i, n)) (Inter (j, m)) =
  (i = j) && (Link.Face.equal n m)

let ord_of_inter (Inter (i, _)) = i

let face_of_inter (Inter (_, f)) = f

let string_of_inter (Inter (n, f)) =
  sprintf "<%d, %s>" n (Link.string_of_face f)

    
let id (Inter (m, i)) =
  {n = Nodes.empty;
   p = Place.elementary_id m;
   l = Link.elementary_id i;
  }   
  
let id_eps = 
  {n = Nodes.empty;
   p = Place.id0;
   l = Link.id_empty;
  } 
  
let merge n =
  {n = Nodes.empty;
   p = Place.elementary_merge n;
   l = Link.id_empty;
  }
  
let split n =
  {n = Nodes.empty;
   p = Place.elementary_split n;
   l = Link.id_empty;
  }
  
let one =
  {n = Nodes.empty;
   p = Place.one;
   l = Link.id_empty;
  }
  
let zero =
  {n = Nodes.empty;
   p = Place.zero;
   l = Link.id_empty;
  }
  
let sym (Inter (m, i)) (Inter (n, j)) =
  {n = Nodes.empty;
   p = Place.elementary_sym m n;
   l = Link.tens (Link.elementary_id i) (Link.elementary_id j) 0;
  }
  
let ion f c =
  if (arity c) != (Link.Face.cardinal f) then
    raise (CTRL_ERROR (arity c, f))
  else
    {n = Nodes.singleton (0, c);
     p = Place.elementary_ion;
     l = Link.elementary_ion f;
    } 

let sub i o =
  {n = Nodes.empty;
   p = Place.id0;
   l = Link.elementary_sub i o;
  }
  
let closure i = sub i Link.Face.empty
  
let intro o = sub Link.Face.empty o

(* (l list of parents for each site) r roots *)  
let placing l r f =
  {n = Nodes.empty;
   p = Place.parse_placing l r;
   l = Link.elementary_id f;
  }
  
(* Empty link graph and no nodes in the place graph. *)
let is_plc b =
  (Link.Lg.equal b.l Link.id_empty) &&
  (Nodes.cardinal b.n = 0) &&
  (b.p.Place.n = 0)
  
(* Empty place graph and no nodes in the link graph. *)
let is_wir b =
  (b.p = Place.id0) && (Nodes.cardinal b.n = 0)
  
let is_id b =
  (Nodes.is_empty b.n) && (Place.is_id b.p) && (Link.is_id b.l)
  
let tens a b =
  {n = uplus a.n b.n;
   p = Place.tens a.p b.p;
   l = Link.tens a.l b.l (Nodes.cardinal a.n);
  }

let comp a b =
  {n = uplus a.n b.n;
   p = Place.comp a.p b.p;
   l = Link.comp a.l b.l (Nodes.cardinal a.n);
  }
 
let ppar a b =
  {n = uplus a.n b.n;
   p = Place.tens a.p b.p;
   l = Link.ppar a.l b.l (Nodes.cardinal a.n);
  }
  
let ppar_of_list bs =
  List.fold_left (fun acc b -> ppar acc b) id_eps bs
  
let par a b =
  let p = ppar a b in
  let out_p = outer p in 
  comp (tens (merge (ord_of_inter out_p))
          (id (Inter (0, face_of_inter out_p)))
    ) p
  
let par_of_list bs =
  List.fold_left (fun acc b -> par acc b) id_eps bs
  
let nest a b =
  let idx = id (Inter (0, face_of_inter (outer b))) in
  comp (ppar a idx) b
  
(* share f by psi in g check that psi is a placing *)
let share f psi g =
  if is_plc psi
  then comp (comp g (tens psi (id (Inter (0, face_of_inter (outer f)))))) f
  else raise SHARING_ERROR
  
let string_of_bg b =
  sprintf "%s\n%s%s" (string_of_nodes b.n) (Place.string_of_pg b.p)
    (Link.string_of_lg b.l)    
  
let is_mono b =
  (Place.is_mono b.p) && (Link.is_mono b.l)
  
let is_epi b =
  (Place.is_epi b.p) && (Link.is_epi b.l)

let is_guard b =
  (Place.is_guard b.p) && (Link.is_guard b.l)   

let is_solid b =
  (is_epi b) && (is_mono b) && (is_guard b)
  
(* TO DO *)
(*let latex_of_big = function | Bg (ns, p, l) -> "latex representation"*)
  
let apply_iso i b =
  let (x, y) = ((Iso.cardinal i), (Nodes.cardinal b.n)) in
  if x = y then
    {n = apply_nodes b.n i;
     p = Place.apply_iso i b.p;
     l = Link.apply_iso i b.l;
    }
  else raise (ISO_ERROR (y, x))

let get_dot b ide =
  let build_rank i flag =
    let ord = ord_of_inter i
    and f = face_of_inter i in 
    if  (ord = 0) && (Link.Face.is_empty f) then ""
    else if flag then
      let ss =
        (List.map (fun i ->
          sprintf "r%d" i) (Int_set.elements (of_int ord))) @
        (List.map (fun (Link.Nam n) ->
          sprintf "o%s" n) (Link.Face.elements f)) in
      match ss with
       | [] -> ""
       | _ -> sprintf "{rank=source; %s};\n" (String.concat "; " ss)
      else
        let xs =
        (List.map (fun i ->
          sprintf "s%d" i) (Int_set.elements (of_int ord))) @
        (List.map (fun (Link.Nam n) ->
          sprintf "i%s" n) (Link.Face.elements f)) in
        match xs with
         | [] -> ""
         | _ -> sprintf "{rank=sink; %s};\n" (String.concat "; " xs) in 
  let inner_shp, outer_shp, hyp_shp, link_adj = Link.get_dot b.l
  and roots_shp, sites_shp, node_ranks, place_adj = Place.get_dot b.p
  and nodes_shp = get_dot b.n
  and rank_out = build_rank (outer b) true
  and rank_in = build_rank (inner b) false in
  sprintf "digraph \"%s\"{\n%s%s%s%s%s%s%s%s%s%s%s}"
      ide roots_shp outer_shp rank_out hyp_shp nodes_shp sites_shp
      node_ranks inner_shp rank_in place_adj link_adj

let decomp t p i_v i_e =
  let p_c, p_id, p_d, i_c, i_d = Place.decomp t.p p.p i_v in
  let l_c, l_d, l_id = Link.decomp t.l p.l i_v i_e i_c i_d
  and n_c, n_d = apply_nodes t.n i_c, apply_nodes t.n i_d in
  ({p = p_c; l = l_c; n = n_c}, {p = p_d; l = l_d; n = n_d},
   {p = p_id; l = l_id; n = Nodes.empty})

(* List of bigraphs. First one is the top level. *)
let levels b =
  let phi, ls = Place.levels b.p in
  let w, ids = Link.levels b.l (List.map (fun (ps, _, _) ->
    ports_of_nodes (Nodes.filter (fun (i, _) ->
      Int_set.mem i ps) b.n)) ls) in
  (tens {p = phi; n = Nodes.empty; l = Link.id_empty;}
        {p = Place.id0; l = w; n = Nodes.empty;}) ::
    (List.map2 (fun (ions, n, psi) l ->
      tens (comp (tens (ppar_of_list (Int_set.fold (fun j acc ->
            (* find j in the node set *)
            let n_j = (* Not_found? *)
              snd (Nodes.choose (Nodes.filter (fun (i, _) ->
                i = j) b.n)) in
            let f = 
              Int_set.fold (fun i acc ->
                (sprintf "n%d_%d" j i) :: acc) (of_int (arity n_j)) [] in    
            (ion (Link.parse_face f) n_j) :: acc) ions []))
          {p = Place.elementary_id n; l = Link.id_empty; n = Nodes.empty;})
        {p = psi; l = Link.id_empty; n = Nodes.empty;})
      {p = Place.id0; l = l; n = Nodes.empty;}) ls ids)

(* Normal form up to level iso *)

(*let fix_bug_roots isos t p = (*	Printf.printf "DEBUG: fix_bug_roots.\n";*)
  List.filter
    (fun (iso, _) ->
       let n_root = (* nodes in the pattern being a child of a root *)
         Place.children_of_roots (place_graph p) in
       let pairs = Iso.filter (fun (a, _) -> Int_set.mem a n_root) iso
       in
         (*					Printf.printf "DEBUG: pairs %s.\n"                                   *)
         (*						(String.concat " " (List.map (fun (a, b) ->                        *)
         (*													Printf.sprintf "(%d,%d)" a b) (Iso.elements pairs)));*)
         Iso.for_all
           (fun (a, b) ->
              let sib_p = Place.siblings (place_graph p) a
              and sib_t = Place.siblings (place_graph t) b
              in
                (*									Printf.printf "DEBUG: sib_p = %s  sib_t = %s.\n"      *)
                (*										(string_of_Int_set sib_p) (string_of_Int_set sib_t);*)
                if Int_set.is_empty sib_p
                then true
                else Int_set.subset (apply sib_p iso) sib_t)
           pairs)
    isos
  
let fix_bug_sites isos t p = (*	Printf.printf "DEBUG: fix_bug_sites.\n";*)
  List.filter
    (fun (iso, _) ->
       let n_site = (* nodes in the pattern being a parent of a site *)
         Place.parents_of_sites (place_graph p) in
       let pairs = Iso.filter (fun (a, _) -> Int_set.mem a n_site) iso
       in
         (*					Printf.printf "DEBUG: pairs %s.\n"                                   *)
         (*						(String.concat " " (List.map (fun (a, b) ->                        *)
         (*													Printf.sprintf "(%d,%d)" a b) (Iso.elements pairs)));*)
         Iso.for_all
           (fun (a, b) ->
              let par_p = Place.partners (place_graph p) a
              and par_t = Place.partners (place_graph t) b
              in
                (*									Printf.printf "DEBUG: par_p = %s  par_t = %s.\n"      *)
                (*										(string_of_Int_set par_p) (string_of_Int_set par_t);*)
                if Int_set.is_empty par_p
                then true
                else Int_set.subset (apply par_p iso) par_t)
           pairs)
    isos
*)

(* 
(* return a list of pairs of isos (nodes and edges) *)
let occurrences t p oc ic =
  if sub_multi (abs_nodes (nodes t)) (abs_nodes (nodes p))
  then
    (let out_1 = Match.match_A (match_string t) (match_string p) oc ic in
     (* BUG CORRECTION ROOTS *)
     (*	Printf.printf "DEBUG: %d matches before fix_bug_roots.\n" (List.length out_1);*)
     let out_2 = fix_bug_roots out_1 t p
     in
       (* BUG CORRECTION SITES *)
       (*	Printf.printf "DEBUG: %d matches before fix_bug_sites.\n" (List.length out_2);*)
       fix_bug_sites out_2 t p)
  else []
  
let occurs t p oc ic =
  if sub_multi (abs_nodes (nodes t)) (abs_nodes (nodes p))
  then Match.match_B (match_string t) (match_string p) oc ic
  else false
  
(* followed by next occurrence call *)
let occurrence t p oc ic =
  try
    if sub_multi (abs_nodes (nodes t)) (abs_nodes (nodes p))
    then Match.match_S (match_string t) (match_string p) oc ic
    else raise Not_found
  with | Match.NO_MATCH -> raise Not_found

(*  
let equals b0 b1 oc ic =
  if
    (inter_equals (inner b0) (inner b1)) &&
      ((inter_equals (outer b0) (outer b1)) &&
         (((abs_nodes (nodes b0)) = (abs_nodes (nodes b1))) &&
            ((num_edges b0) = (num_edges b1))))
  then (* lean? *)
    (try
       (*				Printf.printf "DEBUG: equals Checking equality ....\n";*)
       let (i_n, i_e) = occurrence b0 b1 oc ic
       in
         if
           ((Iso.cardinal i_n) = (num_nodes b0)) &&
             ((Iso.cardinal i_e) = (num_edges b0))
         then true
         else
           (*Try to apply the iso and compare the components or decompose*)
           false
     with | Not_found -> false)
  else false
 *)
  
(* [s] is a ctrl list list. It is assumed that a ctrl list does not        *)
(* contain duplicates and every ctrl is present in only one list. The      *)
(* algorithm consists of substituting every ctrl with its corresponding    *)
(* sort. The original control mapping is also returned.                    *)
(*let apply_sorting s b =
  let get_sort ss c = List.find (List.mem c) ss in
  let old_v = nodes b and pg = place_graph b and lg = link_graph b in
  let new_v =
    Nodes.fold
      (fun (n, c) acc ->
         try let sc = get_sort s c in Nodes.add (n, (sort sc)) acc
         with | Not_found -> Nodes.add (n, c) acc)
      old_v Nodes.empty
  in ((Bg (new_v, pg, lg)), old_v)
 *) 
*)

(* DEBUG *)
(*let _ =
  let parse =
    List.fold_left (fun s f -> Link.Lg.add f s) Link.Lg.empty
  and parse_p =
    List.fold_left (fun s f -> Ports.add f s) Ports.empty
  and parse_n  = 
    List.fold_left (fun s (i,c,a) -> Nodes.add (i, Ctrl(c,a)) s) Nodes.empty in
  (* Example from page 82 *)
  let t = {
    p = {Place.r = 1; Place.n = 8; Place.s = 2; Place.m = Matrix.make 9 10};
    l = parse [{Link.o = Link.parse_face ["x"];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(0,0);(3,0);(4,0);(5,0)]};
               {Link.o = Link.parse_face [];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(1,0);(7,0)]};
               {Link.o = Link.parse_face [];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(2,0)]};
               {Link.o = Link.parse_face [];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(6,0)]}];
    n = parse_n [(0, "A", 1); (1, "B", 1); (2, "B", 1);
                 (3, "A", 1); (4, "A", 1); (5, "A", 1);
                 (6, "B", 1); (7, "B", 1)];
    } in
  t.p.Place.m.Matrix.m.(0).(0) <- true;
  t.p.Place.m.Matrix.m.(1).(1) <- true;
  t.p.Place.m.Matrix.m.(1).(2) <- true;
  t.p.Place.m.Matrix.m.(1).(3) <- true;
  t.p.Place.m.Matrix.m.(2).(4) <- true;
  t.p.Place.m.Matrix.m.(3).(5) <- true;
  t.p.Place.m.Matrix.m.(4).(6) <- true;
  t.p.Place.m.Matrix.m.(5).(7) <- true;
  t.p.Place.m.Matrix.m.(6).(7) <- true;
  t.p.Place.m.Matrix.m.(7).(9) <- true;
  t.p.Place.m.Matrix.m.(8).(8) <- true;
  printf "target:\n%s\n" (string_of_bg t);
  printf "get_dot:\n%s\n" (get_dot t "target");
  let p = {
    p = {Place.r = 1; Place.n = 3; Place.s = 2; Place.m = Matrix.make 4 5};
    l = parse [{Link.o = Link.parse_face ["x"];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(0,0)]};
               {Link.o = Link.parse_face ["y"];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(2,0)]};
               {Link.o = Link.parse_face [];
                Link.i = Link.parse_face ["z"];
                Link.p = parse_p [(1,0)]}];
    n = parse_n [(0, "A", 1); (1, "B", 1); (2, "A", 1)];
    } in
  p.p.Place.m.Matrix.m.(0).(0) <- true;
  p.p.Place.m.Matrix.m.(1).(1) <- true;
  p.p.Place.m.Matrix.m.(1).(3) <- true;
  p.p.Place.m.Matrix.m.(2).(2) <- true;
  p.p.Place.m.Matrix.m.(3).(4) <- true;
  printf "pattern:\n%s\n" (string_of_bg p);
  printf "get_dot:\n%s\n" (get_dot p "pattern");
*)

