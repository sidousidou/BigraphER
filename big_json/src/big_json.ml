open Bigraph
module Default_S = Solver.Make_SAT (Solver.MS) (* Minisat *)

module BRS = Brs.Make (Default_S)
module SBRS = Sbrs.Make (Default_S)
module PBRS = Pbrs.Make (Default_S)
module NBRS = Nbrs.Make (Default_S)

(* Encoder *)

let lexeme e l = ignore (Jsonm.encode e (`Lexeme l))

let string e s = lexeme e (`String s)

let float e f = lexeme e (`Float f)

let int e i = float e (float_of_int i)

let option e f = function None -> lexeme e `Null | Some v -> f e v

let field e n f v =
  lexeme e (`Name n);
  f v

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

let quadruple e (n0, f0, v0) (n1, f1, v1) (n2, f2, v2) (n3, f3, v3) =
  lexeme e `Os;
  field e n0 f0 v0;
  field e n1 f1 v1;
  field e n2 f2 v2;
  field e n3 f3 v3;
  lexeme e `Oe

let quintuple e (n0, f0, v0) (n1, f1, v1) (n2, f2, v2) (n3, f3, v3)
    (n4, f4, v4) =
  lexeme e `Os;
  field e n0 f0 v0;
  field e n1 f1 v1;
  field e n2 f2 v2;
  field e n3 f3 v3;
  field e n4 f4 v4;
  lexeme e `Oe

let ctrl e (Ctrl.C (n, ps, a)) =
  triple e
    ("ctrl_name", string e, n)
    ( "ctrl_params",
      (fun l ->
        lexeme e `As;
        List.iter
          (function
            | Ctrl.I i -> singleton e "ctrl_int" (int e) i
            | Ctrl.F f -> singleton e "ctrl_float" (float e) f
            | Ctrl.S s -> singleton e "ctrl_string" (string e) s)
          l;
        lexeme e `Ae),
      ps )
    ("ctrl_arity", int e, a)

let nodes e ns =
  lexeme e `As;
  Nodes.iter
    (fun i c -> pair e ("node_id", int e, i) ("control", ctrl e, c))
    ns;
  lexeme e `Ae

let place e p =
  let bmatrix e l =
    lexeme e `As;
    List.iter
      (fun (i, j) -> pair e ("source", int e, i) ("target", int e, j))
      l;
    lexeme e `Ae
  in
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
    Link.Face.iter (fun (Link.Name x) -> singleton e "name" (string e) x) f;
    lexeme e `Ae
  and ports e p =
    lexeme e `As;
    Link.Ports.iter
      (fun i a -> pair e ("node_id", int e, i) ("port_arity", int e, a))
      p;
    lexeme e `Ae
  in
  lexeme e `As;
  Link.Lg.iter
    (fun edg ->
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
  Fun.iter (fun x y -> pair e ("x", int e, x) ("y", int e, y)) f;
  lexeme e `Ae

let iso e i =
  lexeme e `As;
  Iso.iter (fun x y -> pair e ("i", int e, x) ("j", int e, y)) i;
  lexeme e `Ae

let react e r =
  quadruple e
    ("brs_name", string e, BRS.name r)
    ("brs_lhs", big e, BRS.lhs r)
    ("brs_rhs", big e, BRS.rhs r)
    ("brs_eta", option e eta, BRS.map r)

let sreact e r =
  lexeme e `Os;
  field e "sbrs_name" (string e) (SBRS.name r);
  field e "sbrs_lhs" (big e) (SBRS.lhs r);
  field e "sbrs_rhs" (big e) (SBRS.rhs r);
  field e "sbrs_rate" (float e) (SBRS.rate r);
  field e "sbrs_eta" (option e eta) (SBRS.map r);
  lexeme e `Oe

let preact e r =
  lexeme e `Os;
  field e "pbrs_name" (string e) (PBRS.name r);
  field e "pbrs_lhs" (big e) (PBRS.lhs r);
  field e "pbrs_rhs" (big e) (PBRS.rhs r);
  field e "pbrs_p" (float e) (PBRS.weight r);
  field e "pbrs_eta" (option e eta) (PBRS.map r);
  lexeme e `Oe

let nreact e r =
  lexeme e `Os;
  field e "nbrs_name" (string e) (NBRS.name r);
  field e "nbrs_action" (string e) (NBRS.action r);
  field e "nbrs_lhs" (big e) (NBRS.lhs r);
  field e "nbrs_rhs" (big e) (NBRS.rhs r);
  field e "nbrs_p" (float e) (NBRS.weight r);
  field e "nbrs_eta" (option e eta) (NBRS.map r);
  lexeme e `Oe

let occs e l =
  lexeme e `As;
  List.iter (fun b -> big e b) l;
  lexeme e `Ae

let p_occs name e l =
  lexeme e `As;
  List.iter (fun (b, r) -> pair e ("state", big e, b) (name, float e, r)) l;
  lexeme e `Ae

let n_occs name1 name2 name3 e l =
  lexeme e `As;
  List.iter
    (fun (b, (a, r, p)) ->
      quadruple e
        ("state", big e, b)
        (name1, string e, a)
        (name2, int e, r)
        (name3, float e, p))
    l;
  lexeme e `Ae

let matches e l =
  lexeme e `As;
  List.map (fun o -> Solver.(o.nodes, o.edges, o.hyper_edges)) l
  |> List.iter (fun (i, i', f) ->
         triple e ("iso_n", iso e, i) ("iso_e", iso e, i') ("f_e", eta e, f));
  lexeme e `Ae

let aux_graph l f_i f e g =
  singleton e l
    (fun g ->
      lexeme e `As;
      f_i f g;
      lexeme e `Ae)
    g

let brs e rs =
  aux_graph "brs" BRS.iter_edges
    (fun i j _ -> pair e ("source", int e, i) ("target", int e, j))
    e rs

let sbrs e rs =
  aux_graph "sbrs" SBRS.iter_edges
    (fun i j l ->
      triple e ("source", int e, i) ("target", int e, j) ("rate", float e, l))
    e rs

let pbrs e rs =
  aux_graph "pbrs" PBRS.iter_edges
    (fun i j l ->
      triple e
        ("source", int e, i)
        ("target", int e, j)
        ("probability", float e, l))
    e rs

let nbrs e rs =
  aux_graph "nbrs" NBRS.iter_edges
    (fun i j (a, r, p) ->
      quintuple e
        ("source", int e, i)
        ("target", int e, j)
        ("action", string e, a)
        ("reward", int e, r)
        ("probability", float e, p))
    e rs

let b_size = 65536

let to_json ?(minify = true) f v =
  let b = Buffer.create b_size in
  let e = Jsonm.encoder ~minify (`Buffer b) in
  f e v;
  ignore (Jsonm.encode e `End);
  Buffer.contents b

let big_to_json ?(minify = true) = to_json ~minify big

let react_to_json ?(minify = true) = to_json ~minify react

let preact_to_json ?(minify = true) = to_json ~minify preact

let sreact_to_json ?(minify = true) = to_json ~minify sreact

let nreact_to_json ?(minify = true) = to_json ~minify nreact

let occs_to_json ?(minify = true) = to_json ~minify occs

let p_occs_to_json ?(minify = true) = to_json ~minify (p_occs "prob")

let s_occs_to_json ?(minify = true) = to_json ~minify (p_occs "rate")

let n_occs_to_json ?(minify = true) =
  to_json ~minify (n_occs "action" "reward" "prob")

let matches_to_json ?(minify = true) = to_json ~minify matches

let ts_to_json ?(minify = true) = to_json ~minify brs

let dtmc_to_json ?(minify = true) = to_json ~minify pbrs

let ctmc_to_json ?(minify = true) = to_json ~minify sbrs

let mdp_to_json ?(minify = true) = to_json ~minify nbrs

(* Decoder *)

type json =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of json list
  | `O of (string * json) list ]

exception Escape of ((int * int) * (int * int)) * Jsonm.error

(* Error messages *)

let dec_err (_, (l, c)) e =
  Jsonm.pp_error Format.str_formatter e;
  "Line " ^ string_of_int l ^ ", character " ^ string_of_int c ^ ":\nError:"
  ^ Buffer.contents @@ Format.stdbuf

let err_cmp l =
  List.find (fun (n, n') -> n <> n') l |> fun (n, n') ->
  "'" ^ n ^ "' = '" ^ n' ^ "'"

let type_err _ msg = "Error: " ^ msg ^ " expected."

let disj_type_err = String.concat "|"

let json_of_src ?(encoding = `UTF_8) src =
  let dec d =
    match Jsonm.decode d with
    | `Lexeme l -> l
    | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
    | `End | `Await -> assert false
  in
  let rec value v k d =
    match v with
    | `Os -> obj [] k d
    | `As -> arr [] k d
    | (`Null | `Bool _ | `String _ | `Float _) as v -> k v d
    | _ -> assert false
  and arr vs k d =
    match dec d with
    | `Ae -> k (`A (List.rev vs)) d
    | v -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d =
    match dec d with
    | `Oe -> k (`O (List.rev ms)) d
    | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
    | _ -> assert false
  in
  let d = Jsonm.decoder ~encoding src in
  try `JSON (value (dec d) (fun v _ -> v) d)
  with Escape (r, e) -> `Error (r, e)

let exp_string = function
  | `String s -> Ok s
  | (`A _ | `Bool _ | `Float _ | `Null | `O _) as j -> Error (j, "'string'")

let exp_float = function
  | `Float f -> Ok f
  | (`A _ | `Bool _ | `String _ | `Null | `O _) as j -> Error (j, "'float'")

let exp_int = function
  | `Float f -> Ok (int_of_float f)
  | (`A _ | `Bool _ | `String _ | `Null | `O _) as j -> Error (j, "'int'")

let bind f = function Ok v -> f v | Error _ as e -> e

let ( >>= ) f g = bind g f

let exp_singleton name f = function
  | `O [ (n, v) ] as j ->
      if name = n then f v else Error (j, err_cmp [ (name, n) ])
  | (`A _ | `Bool _ | `Float _ | `Null | `String _ | `O _) as j ->
      Error (j, "singleton")

let exp_pair (n0, f0) (n1, f1) = function
  | `O [ (n, v); (n', v') ] as t ->
      if n0 = n && n1 = n' then
        f0 v >>= fun v0 ->
        f1 v' >>= fun v1 -> Ok (v0, v1)
      else Error (t, err_cmp [ (n0, n); (n1, n') ])
  | (`A _ | `Bool _ | `Float _ | `Null | `String _ | `O _) as t ->
      Error (t, "pair")

let exp_triple (n0, f0) (n1, f1) (n2, f2) = function
  | `O [ (n, v); (n', v'); (n'', v'') ] as t ->
      if n0 = n && n1 = n' && n2 = n'' then
        f0 v >>= fun v0 ->
        f1 v' >>= fun v1 ->
        f2 v'' >>= fun v2 -> Ok (v0, v1, v2)
      else Error (t, err_cmp [ (n0, n); (n1, n'); (n2, n'') ])
  | (`A _ | `Bool _ | `Float _ | `Null | `String _ | `O _) as t ->
      Error (t, "triple")

let exp_quadruple (n0, f0) (n1, f1) (n2, f2) (n3, f3) = function
  | `O [ (n, v); (n', v'); (n'', v''); (n''', v''') ] as t ->
      if n0 = n && n1 = n' && n2 = n'' && n3 = n''' then
        f0 v >>= fun v0 ->
        f1 v' >>= fun v1 ->
        f2 v'' >>= fun v2 ->
        f3 v''' >>= fun v3 -> Ok (v0, v1, v2, v3)
      else Error (t, err_cmp [ (n0, n); (n1, n'); (n2, n''); (n3, n''') ])
  | (`A _ | `Bool _ | `Float _ | `Null | `String _ | `O _) as t ->
      Error (t, "4-tuple")

let exp_quintuple (n0, f0) (n1, f1) (n2, f2) (n3, f3) (n4, f4) = function
  | `O [ (n, v); (n', v'); (n'', v''); (n''', v'''); (n'''', v'''') ] as t ->
      if n0 = n && n1 = n' && n2 = n'' && n3 = n''' && n4 = n'''' then
        f0 v >>= fun v0 ->
        f1 v' >>= fun v1 ->
        f2 v'' >>= fun v2 ->
        f3 v''' >>= fun v3 ->
        f4 v'''' >>= fun v4 -> Ok (v0, v1, v2, v3, v4)
      else
        Error
          ( t,
            err_cmp [ (n0, n); (n1, n'); (n2, n''); (n3, n'''); (n4, n'''') ]
          )
  | (`A _ | `Bool _ | `Float _ | `Null | `String _ | `O _) as t ->
      Error (t, "5-tuple")

let _exp_sextuple (n0, f0) (n1, f1) (n2, f2) (n3, f3) (n4, f4) (n5, f5) =
  function
  | `O
      [
        (n, v);
        (n', v');
        (n'', v'');
        (n''', v''');
        (n'''', v'''');
        (n''''', v''''');
      ] as t ->
      if
        n0 = n && n1 = n' && n2 = n'' && n3 = n''' && n4 = n''''
        && n5 = n'''''
      then
        f0 v >>= fun v0 ->
        f1 v' >>= fun v1 ->
        f2 v'' >>= fun v2 ->
        f3 v''' >>= fun v3 ->
        f4 v'''' >>= fun v4 ->
        f5 v''''' >>= fun v5 -> Ok (v0, v1, v2, v3, v4, v5)
      else
        Error
          ( t,
            err_cmp
              [
                (n0, n);
                (n1, n');
                (n2, n'');
                (n3, n''');
                (n4, n'''');
                (n5, n''''');
              ] )
  | (`A _ | `Bool _ | `Float _ | `Null | `String _ | `O _) as t ->
      Error (t, "6-tuple")

let exp_septuple (n0, f0) (n1, f1) (n2, f2) (n3, f3) (n4, f4) (n5, f5)
    (n6, f6) = function
  | `O
      [
        (n, v);
        (n', v');
        (n'', v'');
        (n''', v''');
        (n'''', v'''');
        (n''''', v''''');
        (n'''''', v'''''');
      ] as t ->
      if
        n0 = n && n1 = n' && n2 = n'' && n3 = n''' && n4 = n''''
        && n5 = n''''' && n6 = n''''''
      then
        f0 v >>= fun v0 ->
        f1 v' >>= fun v1 ->
        f2 v'' >>= fun v2 ->
        f3 v''' >>= fun v3 ->
        f4 v'''' >>= fun v4 ->
        f5 v''''' >>= fun v5 ->
        f6 v'''''' >>= fun v6 -> Ok (v0, v1, v2, v3, v4, v5, v6)
      else
        Error
          ( t,
            err_cmp
              [
                (n0, n);
                (n1, n');
                (n2, n'');
                (n3, n''');
                (n4, n'''');
                (n5, n''''');
                (n6, n'''''');
              ] )
  | (`A _ | `Bool _ | `Float _ | `Null | `String _ | `O _) as t ->
      Error (t, "7-tuple")

let rec conv j msgs = function
  | [] -> Error (j, disj_type_err msgs)
  | f :: fs -> (
      match f j with Ok v -> Ok v | Error (_, e) -> conv j (e :: msgs) fs )

let rec map_bind f res = function
  | [] -> Ok (List.rev res)
  | j :: js -> (
      match conv j [] f with
      | Ok v -> map_bind f (v :: res) js
      | Error _ as e -> e )

let exp_array f = function
  | `A l -> map_bind f [] l
  | (`Bool _ | `Float _ | `Null | `String _ | `O _) as t -> Error (t, "array")

let exp_option f (j : json) =
  conv j []
    [
      (function
      | `Null -> Ok None
      | (`Bool _ | `Float _ | `A _ | `String _ | `O _) as t ->
          Error (t, "option"));
      (fun j -> f j >>= fun v -> Ok (Some v));
    ]

let exp_ctrl (j : json) =
  exp_triple ("ctrl_name", exp_string)
    ( "ctrl_params",
      exp_array
        [
          exp_singleton "ctrl_int" (fun j ->
              exp_int j >>= fun i -> Ok (Ctrl.I i));
          exp_singleton "ctrl_float" (fun j ->
              exp_float j >>= fun f -> Ok (Ctrl.F f));
          exp_singleton "ctrl_string" (fun j ->
              exp_string j >>= fun s -> Ok (Ctrl.S s));
        ] )
    ("ctrl_arity", exp_int) j
  >>= fun (n, ps, a) -> Ok (Ctrl.C (n, ps, a))

let exp_nodes (j : json) =
  exp_array [ exp_pair ("node_id", exp_int) ("control", exp_ctrl) ] j
  >>= fun l ->
  Ok (List.fold_left (fun ns (i, c) -> Nodes.add i c ns) Nodes.empty l)

let exp_bmatrix (j : json) r c =
  exp_array [ exp_pair ("source", exp_int) ("target", exp_int) ] j
  >>= fun l -> Ok (Sparse.add_list (Sparse.make r c) l)

let exp_place = function
  | `O
      [
        (n0, v0); (n1, v1); (n2, v2); (n3, v3); (n4, v4); (n5, v5); (n6, v6);
      ] as t ->
      if
        n0 = "num_regions" && n1 = "num_nodes" && n2 = "num_sites"
        && n3 = "rn" && n4 = "rs" && n5 = "nn" && n6 = "ns"
      then
        exp_int v0 >>= fun r ->
        exp_int v1 >>= fun n ->
        exp_int v2 >>= fun s ->
        exp_bmatrix v3 r n >>= fun rn ->
        exp_bmatrix v4 r s >>= fun rs ->
        exp_bmatrix v5 n n >>= fun nn ->
        exp_bmatrix v6 n s >>= fun ns -> Ok Place.{ r; n; s; rn; rs; nn; ns }
      else
        Error
          ( t,
            err_cmp
              [
                (n0, "num_regions");
                (n1, "num_nodes");
                (n2, "num_sites");
                (n3, "rn");
                (n4, "rs");
                (n5, "nn");
                (n6, "ns");
              ] )
  | (`A _ | `Bool _ | `Float _ | `Null | `String _ | `O _) as t ->
      Error (t, "7-tuple")

let exp_link (j : json) =
  let exp_face (j : json) =
    exp_array [ exp_singleton "name" exp_string ] j >>= fun l ->
    Ok (Link.parse_face l)
  and exp_ports (j : json) =
    exp_array [ exp_pair ("node_id", exp_int) ("port_arity", exp_int) ] j
    >>= fun l -> Ok (Link.Ports.of_list l)
  in
  let exp_edg (j : json) =
    exp_triple ("inner", exp_face) ("outer", exp_face) ("ports", exp_ports) j
    >>= fun (i, o, p) -> Ok Link.{ i; o; p }
  in
  exp_array [ exp_edg ] j >>= fun l ->
  Ok (List.fold_left (fun res e -> Link.Lg.add e res) Link.Lg.empty l)

let exp_big (j : json) =
  exp_triple ("nodes", exp_nodes)
    ("place_graph", exp_place)
    ("link_graph", exp_link) j
  >>= fun (n, p, l) -> Ok Big.{ p; l; n }

let exp_eta (j : json) =
  exp_array [ exp_pair ("x", exp_int) ("y", exp_int) ] j >>= fun l ->
  Ok (Fun.of_list l)

let exp_react (j : json) =
  exp_quadruple ("brs_name", exp_string) ("brs_lhs", exp_big)
    ("brs_rhs", exp_big)
    ("brs_eta", exp_option exp_eta)
    j
  >>= fun (name, lhs, rhs, e) ->
  Ok (BRS.parse_react_unsafe ~name ~lhs ~rhs () e)

let exp_sreact (j : json) =
  exp_quintuple ("sbrs_name", exp_string) ("sbrs_lhs", exp_big)
    ("sbrs_rhs", exp_big) ("sbrs_rate", exp_float)
    ("sbrs_eta", exp_option exp_eta)
    j
  >>= fun (name, lhs, rhs, r, e) ->
  Ok (SBRS.parse_react_unsafe ~name ~lhs ~rhs r e)

let exp_preact (j : json) =
  exp_quintuple ("pbrs_name", exp_string) ("pbrs_lhs", exp_big)
    ("pbrs_rhs", exp_big) ("pbrs_p", exp_float)
    ("pbrs_eta", exp_option exp_eta)
    j
  >>= fun (name, lhs, rhs, p, e) ->
  Ok (PBRS.parse_react_unsafe ~name ~lhs ~rhs p e)

let exp_nreact (j : json) =
  exp_septuple ("nbrs_name", exp_string)
    ("nbrs_action", exp_string)
    ("nbrs_reward", exp_int) ("nbrs_lhs", exp_big) ("nbrs_rhs", exp_big)
    ("nbrs_p", exp_float)
    ("nbrs_eta", exp_option exp_eta)
    j
  >>= fun (name, action, reward, lhs, rhs, p, e) ->
  Ok (NBRS.parse_react_unsafe ~name ~lhs ~rhs (action, reward, p) e)

let parse_err = function
  | Ok _ as v -> v
  | Error (j, msg) -> Error (type_err j msg)

let of_json encoding s f =
  match json_of_src ~encoding (`String s) with
  | `Error (r, e) -> Error (dec_err r e)
  | `JSON j -> parse_err @@ f j

let big_of_json ?(encoding = `UTF_8) s = of_json encoding s exp_big

let react_of_json ?(encoding = `UTF_8) s = of_json encoding s exp_react

let preact_of_json ?(encoding = `UTF_8) s = of_json encoding s exp_preact

let sreact_of_json ?(encoding = `UTF_8) s = of_json encoding s exp_sreact

let nreact_of_json ?(encoding = `UTF_8) s = of_json encoding s exp_nreact

(* INTERFACE TO MATCHING ENGINE *)

let exp_step_input (j : json) =
  let aux n f (j : json) = exp_pair ("state", exp_big) (n, f) j in
  let aux_r (j : json) =
    aux "reacts" (exp_array [ exp_react ]) j >>= fun (b, reacts) ->
    Ok (b, `B reacts)
  and aux_p (j : json) =
    aux "preacts" (exp_array [ exp_preact ]) j >>= fun (b, reacts) ->
    Ok (b, `P reacts)
  and aux_s (j : json) =
    aux "sreacts" (exp_array [ exp_sreact ]) j >>= fun (b, reacts) ->
    Ok (b, `S reacts)
  and aux_n (j : json) =
    aux "nreacts" (exp_array [ exp_nreact ]) j >>= fun (b, reacts) ->
    Ok (b, `N reacts)
  in
  conv j [] [ aux_r; aux_p; aux_s; aux_n ]

let check_aux f reacts =
  try if List.for_all f reacts then Ok true else assert false with
  | BRS.NOT_VALID e -> Error (BRS.string_of_react_err e)
  | PBRS.NOT_VALID e -> Error (PBRS.string_of_react_err e)
  | SBRS.NOT_VALID e -> Error (SBRS.string_of_react_err e)
  | NBRS.NOT_VALID e -> Error (NBRS.string_of_react_err e)

let check_validity = function
  | `B reacts -> check_aux BRS.is_valid_react_exn reacts
  | `P reacts -> check_aux PBRS.is_valid_react_exn reacts
  | `S reacts -> check_aux SBRS.is_valid_react_exn reacts
  | `N reacts -> check_aux NBRS.is_valid_react_exn reacts

let aux_step minify solver (b, reacts) =
  let solver_t =
    match solver with
    | "MSAT" -> Solver.MSAT
    | "MCARD" -> Solver.MCARD
    | s -> failwith ("Solver " ^ s ^ " not supported")
  in
  let wrapper1 func lst = List.map (fun (a, _, _) -> a) lst |> func
  and wrapper func lst = List.map (fun (a, b, _) -> (a, b)) lst |> func
  and aux s_f j_f b rs = Ok (s_f b rs) >>= fun (occs, _) -> Ok (j_f occs) in
  check_validity reacts >>= fun _ ->
  match reacts with
  | `B rs -> (
      match solver_t with
      | Solver.MSAT ->
          let open struct
            module BRS = Brs.Make (Solver.Make_SAT (Solver.MS))
          end in
          aux BRS.step (wrapper1 (occs_to_json ~minify)) b rs
      | Solver.MCARD ->
          let open struct
            module BRS = Brs.Make (Solver.Make_SAT (Solver.MC))
          end in
          aux BRS.step (wrapper1 (occs_to_json ~minify)) b rs )
  | `P rs -> (
      match solver_t with
      | Solver.MSAT ->
          let open struct
            module PBRS = Pbrs.Make (Solver.Make_SAT (Solver.MS))
          end in
          aux PBRS.step (wrapper (p_occs_to_json ~minify)) b rs
      | Solver.MCARD ->
          let open struct
            module PBRS = Pbrs.Make (Solver.Make_SAT (Solver.MC))
          end in
          aux PBRS.step (wrapper1 (occs_to_json ~minify)) b rs )
  | `S rs -> (
      match solver_t with
      | Solver.MSAT ->
          let open struct
            module SBRS = Sbrs.Make (Solver.Make_SAT (Solver.MS))
          end in
          aux SBRS.step (wrapper (s_occs_to_json ~minify)) b rs
      | Solver.MCARD ->
          let open struct
            module SBRS = Sbrs.Make (Solver.Make_SAT (Solver.MC))
          end in
          aux SBRS.step (wrapper (s_occs_to_json ~minify)) b rs )
  | `N rs -> (
      match solver_t with
      | Solver.MSAT ->
          let open struct
            module NBRS = Nbrs.Make (Solver.Make_SAT (Solver.MS))
          end in
          aux NBRS.step (wrapper (n_occs_to_json ~minify)) b rs
      | Solver.MCARD ->
          let open struct
            module NBRS = Nbrs.Make (Solver.Make_SAT (Solver.MC))
          end in
          aux NBRS.step (wrapper (n_occs_to_json ~minify)) b rs )

let aux_string minify = function
  | Ok s -> s
  | Error s -> to_json ~minify (fun e -> singleton e "error" (string e)) s

let step ?(encoding = `UTF_8) ?(minify = true) ?(solver = "MSAT") s =
  of_json encoding s exp_step_input
  >>= aux_step minify solver |> aux_string minify

let big_match ?(minify = true) ?(solver = "MSAT") in_ch out_ch =
  ( match json_of_src (`Channel in_ch) with
  | `Error (r, e) -> Error (dec_err r e)
  | `JSON j -> parse_err @@ exp_step_input j )
  >>= aux_step minify solver |> aux_string minify
  |> fun s -> output_string out_ch s
