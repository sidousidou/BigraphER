open Bigraph

let lexeme e l = ignore (Jsonm.encode e (`Lexeme l))

let string e s = lexeme e (`String s)
let float e f = lexeme e (`Float f)
let int e i = float e (float_of_int i)
let option e f = function
  | None -> lexeme e `Null
  | Some v -> f e v

let field e n f v =
  lexeme e (`Name n); f v

let singleton e n f v =
  lexeme e `Os;
  field e n f v;
  lexeme e `Oe

let pair e (n0, f0, v0) (n1, f1, v1) =
  lexeme e `Os;
  field e n0 f0 v0;
  field e n1 f1 v1;
  lexeme e `Oe

let triple e (n0, f0, v0) (n1, f1, v1) (n2, f2, v2) =
  lexeme e `Os;
  field e n0 f0 v0;
  field e n1 f1 v1;
  field e n2 f2 v2;
  lexeme e `Oe

let ctrl e (Ctrl.C (n, ps, a)) =
  triple e
    ("ctrl_name", string e, n)
    ("ctrl_params", (fun l ->
         lexeme e `As;
         List.iter (function
             | Ctrl.I i -> singleton e "ctrl_int" (int e) i
             | Ctrl.F f -> singleton e "ctrl_float" (float e) f
             | Ctrl.S s -> singleton e "ctrl_string" (string e) s)
           l;
         lexeme e `Ae), ps)
    ("ctrl_arity", int e, a)

let nodes e ns =
  lexeme e `As;
  Nodes.iter (fun i c ->
      pair e ("node_id", int e, i) ("control", ctrl e, c))
    ns;
  lexeme e `Ae

let place e p =
  let bmatrix e l =
    lexeme e `As;
    List.iter (fun (i, j) ->
        pair e ("source", int e, i) ("target", int e, j))
      l;
    lexeme e `Ae in
  lexeme e `Os;
  field e "num_regions" (int e) p.Place.r;
  field e "num_nodes" (int e) p.Place.n;
  field e "num_sites" (int e) p.Place.s;
  field e "rn" (bmatrix e) (Place.edges p.Place.rn);
  field e "rs" (bmatrix e) (Place.edges p.Place.rs);
  field e "nn" (bmatrix e) (Place.edges p.Place.nn);
  field e "ns" (bmatrix e) (Place.edges p.Place.ns);
  lexeme e `Oe

let link e l =
  let face e f =
    lexeme e `As;
    Link.Face.iter (fun (Link.Nam x) ->
        singleton e "name" (string e) x)
      f;
    lexeme e `Ae
  and ports e p =
    lexeme e `As;
    Link.Ports.iter (fun i a ->
        pair e ("node_id", int e, i) ("port_arity", int e, a))
      p;
    lexeme e `Ae
  in
  lexeme e `As;
  Link.Lg.iter (fun edg ->
      triple e
        ("inner", face e, edg.Link.i)
        ("outer", face e, edg.Link.o)
        ("ports", ports e, edg.Link.p))
    l;
  lexeme e `Ae

let big e b =
  triple e
    ("nodes", nodes e, b.Big.n)
    ("place_graph", place e, b.Big.p)
    ("link_graph", link e, b.Big.l)

let eta e f =
  lexeme e `As;
  Fun.iter (fun x y ->
      pair e
        ("x", int e, x)
        ("y", int e, y))
    f;
  lexeme e `Ae

let b_size = 65536

let to_json ?(minify=true) f v =
  let b = Buffer.create b_size in
  let e = Jsonm.encoder ~minify (`Buffer b) in
  f e v;
  ignore (Jsonm.encode e `End);
  Buffer.contents b

let ctrl_to_json ?(minify=true) =
  to_json ~minify ctrl

let nodes_to_json ?(minify=true) =
  to_json ~minify nodes

let place_to_json ?(minify=true) =
  to_json ~minify place

let link_to_json ?(minify=true) =
  to_json ~minify link

let big_to_json ?(minify=true) =
  to_json ~minify big
