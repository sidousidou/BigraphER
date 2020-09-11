(* Tests for the matching engine *)
open Printf
open Bigraph
module ST = CI_utils.Shippable_test
module IO = CI_utils.Io
module S = Solver.Make_SAT (Solver.MC)

type test = {
  target : Big.t;
  t_name : string;
  pattern : Big.t;
  p_name : string;
  exp_res : (Iso.t * Iso.t) list;
  mutable res : (Iso.t * Iso.t) list;
}

let sort_res =
  List.fast_sort (fun (iv0, ie0) (iv1, ie1) ->
      let x = Iso.compare iv0 iv1 in
      match x with 0 -> Iso.compare ie0 ie1 | _ -> x)

let print_res res =
  let out =
    List.map
      (fun (i, j) -> sprintf "%s -- %s" (Iso.to_string i) (Iso.to_string j))
      (sort_res res)
  in
  match out with [] -> "{ }" | _ -> "{\n" ^ String.concat "\n" out ^ "\n}"

let check_res res exp_res pattern =
  if List.length res <> List.length exp_res then false
  else
    List.for_all
      (fun ((i0, j0), (i1, j1)) ->
        (* Equality up to iso (autos on pattern) *)
        let autos = S.auto pattern in
        (i1, j1)
        :: List.combine
             (Iso.gen_isos i1 (List.map fst autos))
             (Iso.gen_isos j1 (List.map snd autos))
        |> List.exists (fun (i1', j1') ->
               Iso.equal i0 i1' && Iso.equal j0 j1'))
      (List.combine (sort_res res) (sort_res exp_res))

let test_decomposition t p
    { Solver.nodes = i_n; edges = i_e; hyper_edges = f_e } =
  let c, d, id_big = Big.decomp ~target:t ~pattern:p ~i_n ~i_e f_e in
  Big.(S.equal (comp c (comp (tens p id_big) d)) t)

let attr_match =
  [ ("type", "ASSERT_MATCH"); ("message", "No occurrence of pattern") ]

let module_name = __MODULE__

let do_tests =
  let success t =
    ( t.t_name ^ " &gt; " ^ t.p_name,
      module_name,
      ST.xml_block "system-out" [] [ "Result: " ^ print_res t.res ],
      [] )
  and failure_occ t msg =
    ( t.t_name ^ " &gt; " ^ t.p_name,
      module_name,
      ST.xml_block "system-out" []
        [
          "Result: " ^ print_res t.res ^ "\nExpected result: "
          ^ print_res t.exp_res ^ "\nTarget:\n" ^ Big.to_string t.target
          ^ "\n dot:\n"
          ^ Big.(to_dot t.target "target")
          ^ "\nPattern:\n" ^ Big.to_string t.pattern ^ "\n dot:\n"
          ^ Big.(to_dot t.pattern "pattern")
          ^ "\n";
        ],
      [ ST.xml_block "failure" attr_match [ msg ] ] )
  and failure_decomp t msg occs total =
    ( t.t_name ^ " &gt; " ^ t.p_name,
      module_name,
      ST.xml_block "system-out" []
        [
          (List.length occs |> string_of_int)
          ^ " errors out of " ^ string_of_int total ^ "\nDecompositions:\n"
          ^ ( List.mapi
                (fun i { Solver.nodes = i_n; edges = i_e; hyper_edges = f_e } ->
                  let c, d, id_big =
                    Big.decomp ~target:t.target ~pattern:t.pattern ~i_n ~i_e
                      f_e
                  in
                  "Occurrence " ^ string_of_int i ^ ":\nTarget:\n"
                  ^ Big.to_string t.target ^ "\n dot:\n"
                  ^ Big.(to_dot t.target "target")
                  ^ "\nPattern:\n" ^ Big.to_string t.pattern ^ "\nC:\n"
                  ^ Big.to_string c ^ "\nD:\n" ^ Big.to_string d
                  ^ "\nTensor:\n"
                  ^ Big.(to_string (tens t.pattern id_big))
                  ^ "\nComposition D:\n"
                  ^ Big.(to_string (comp (tens t.pattern id_big) d))
                  ^ "\nComposition C:\n"
                  ^ Big.(to_string (comp c (comp (tens t.pattern id_big) d)))
                  ^ "\n dot:\n"
                  ^ Big.(
                      to_dot
                        (comp c (comp (tens t.pattern id_big) d))
                        ("occ_" ^ string_of_int i)))
                occs
            |> String.concat "\n" );
        ],
      [ ST.xml_block "failure" attr_match [ msg ] ] )
  and error n0 b0 n1 b1 =
    n0 ^ "\n" ^ Big.to_string b0 ^ "\n" ^ n1 ^ "\n" ^ Big.to_string b1 ^ "\n"
    ^ Printexc.get_backtrace ()
  in
  List.map (fun t ->
      let default_fail_msg =
        sprintf "%s cannot be matched in %s." t.p_name t.t_name
      and decomp_fail_msg =
        sprintf "Malformed %s decompositions of %s." t.p_name t.t_name
      in
      try
        let occs = S.occurrences ~target:t.target ~pattern:t.pattern in
        t.res <- List.map (fun o -> Solver.(o.nodes, o.edges)) occs;
        if check_res t.res t.exp_res t.pattern then
          match
            List.filter
              (fun o -> not (test_decomposition t.target t.pattern o))
              occs
          with
          | [] -> success t
          | l -> failure_decomp t decomp_fail_msg l (List.length occs)
        else failure_occ t default_fail_msg
      with
      | S.NODE_FREE -> (
          (* tests 23 and 16 are special cases *)
          match (t.t_name, t.p_name) with
          | "T13", "P23" | "T10", "P16" -> success t
          | _ -> failure_occ t default_fail_msg )
      | Big.COMP_ERROR (x, y) -> (
          (* pattern in test 25 is not epi *)
          match (t.t_name, t.p_name) with
          | "T14", "P25" -> success t
          | _ ->
              failure_occ t
                (sprintf "Interfaces %s != %s" (Big.string_of_inter x)
                   (Big.string_of_inter y)) )
      | _ ->
          ( t.t_name ^ " &gt; " ^ t.p_name,
            module_name,
            ST.(xml_block "system-out" [] [ error_msg ]),
            [
              ST.(
                xml_block "error" attr_err
                  [ error t.t_name t.target t.p_name t.pattern ]);
            ] ))

let do_equality_tests l ts =
  let success s msg =
    (s, module_name, ST.xml_block "system-out" [] [ msg ], [])
  and failure s msg_out msg =
    ( s,
      module_name,
      ST.xml_block "system-out" [] [ msg_out ],
      [ ST.xml_block "failure" attr_match [ msg ] ] )
  and error n b =
    (* let aux m = *)
    (* "(" ^ (string_of_int m.Sparse.r) ^ " X " ^ (string_of_int m.Sparse.c)
       ^ ") " *)
    (* ^ (String.concat " " (Sparse.fold (fun i j acc -> *)
    (* (("(" ^ (string_of_int i) *)
    (* ^ "," ^ (string_of_int j) ^ ")") :: acc)) *)
    (* m [])) in *)
    n ^ "\n" ^ Big.to_string b ^ "\n"
    (* ^ "edges: " ^ (string_of_int (Sparse.entries (b.p.nn))) ^ "\n" *)
    (* ^ "rn:\n" ^  (aux b.p.rn) ^ "\n" *)
    (* ^ "rs:\n" ^  (aux b.p.rs) ^ "\n" *)
    (* ^ "nn:\n" ^  (aux b.p.nn) ^ "\n" *)
    (* ^ "ns:\n" ^  (aux b.p.ns) ^ "\n" *)
    (* ^ "top:\n" ^ (aux (Sparse.append b.p.rn b.p.rs)) ^ "\n" *)
    (* ^ "botton:\n" ^ (aux (Sparse.append b.p.nn b.p.ns)) ^ "\n" *)
    (* ^ "stack:\n" ^ (aux (Sparse.stack *)
    (* (Sparse.append b.p.rn b.p.rs) *)
    (* (Sparse.append b.p.nn b.p.ns))) ^ "\n" *)
    ^ Printexc.get_backtrace ()
  in
  List.map
    (fun (n, b) ->
      let s = n ^ " = " ^ n in
      try
        if S.equal b b then success s "Bigraphs are equal"
        else failure s "Bigraphs are not equal" (sprintf "%s != %s" n n)
      with _ ->
        ( s,
          module_name,
          ST.(xml_block "system-out" [] [ error_msg ]),
          [ ST.(xml_block "error" attr_err [ error n b ]) ] ))
    (List.sort (fun (x, _) (y, _) -> String.compare x y) l)
  @ List.map
      (fun t ->
        let s = t.t_name ^ " = " ^ t.p_name in
        try
          if S.equal t.target t.pattern then
            match (t.t_name, t.p_name) with
            | "T16", "T16" -> success s "Bigraphs are equal"
            | _ ->
                failure s "Bigraphs are equal"
                  ( Big.to_dot t.target t.t_name
                  ^ "\n"
                  ^ Big.to_dot t.pattern t.p_name )
          else success s "Bigraphs are not equal"
        with _ ->
          ( s,
            module_name,
            ST.(xml_block "system-out" [] [ error_msg ]),
            [
              ST.(
                xml_block "error" attr_err
                  [
                    t.t_name ^ "\n" ^ Big.to_string t.target ^ "\n"
                    ^ t.p_name ^ "\n" ^ Big.to_string t.pattern ^ "\n"
                    ^ Printexc.get_backtrace ();
                  ]);
            ] ))
      ts

let do_to_string_tests =
  let success s msg =
    (s, module_name, ST.xml_block "system-out" [] [ msg ], [])
  and failure s msg_out msg =
    ( s,
      module_name,
      ST.xml_block "system-out" [] [ msg_out ],
      [ ST.xml_block "failure" attr_match [ msg ] ] )
  and error n b e =
    n ^ "\n" ^ Big.to_string b ^ "\n" ^ Printexc.to_string e ^ "\n"
    ^ Printexc.get_backtrace ()
  in
  List.map (fun (n, b) ->
      let s = n ^ " = Big.of_string(Big.to_string(" ^ n ^ "))"
      and s_b = Big.to_string b in
      try
        if S.equal b (Big.of_string s_b) then
          success s "Bigraph parsed correctly"
        else
          failure s "Parse error"
            (sprintf "%s != %s" s_b (Big.to_string (Big.of_string s_b)))
      with e ->
        ( s,
          module_name,
          ST.(xml_block "system-out" [] [ error_msg ]),
          [ ST.(xml_block "error" attr_err [ error n b e ]) ] ))

let tests bgs =
  (* TEST 1 *)
  [
    {
      t_name = "T1";
      p_name = "P1";
      target = List.assoc "T1" bgs;
      pattern = List.assoc "P1" bgs;
      exp_res =
        [
          (Iso.of_list [ (0, 0); (1, 2) ], Iso.of_list []);
          (Iso.of_list [ (0, 0); (1, 3) ], Iso.of_list []);
        ];
      res = [];
    };
    (* TEST 2 *)
    {
      t_name = "T2";
      p_name = "P3";
      target = List.assoc "T2" bgs;
      pattern = List.assoc "P3" bgs;
      exp_res = [];
      res = [];
    };
    (* TEST 3 *)
    {
      t_name = "T2";
      p_name = "P2";
      target = List.assoc "T2" bgs;
      pattern = List.assoc "P2" bgs;
      exp_res = [ (Iso.of_list [ (0, 0) ], Iso.of_list []) ];
      res = [];
    };
    (* TEST 4 *)
    {
      t_name = "T3";
      p_name = "P4";
      target = List.assoc "T3" bgs;
      pattern = List.assoc "P4" bgs;
      exp_res = [];
      res = [];
    };
    (* TEST 5 *)
    {
      t_name = "T3";
      p_name = "P5";
      target = List.assoc "T3" bgs;
      pattern = List.assoc "P5" bgs;
      exp_res = [ (Iso.of_list [ (0, 1); (1, 0) ], Iso.of_list []) ];
      res = [];
    };
    (* TEST 6 *)
    {
      t_name = "T4";
      p_name = "P6";
      target = List.assoc "T4" bgs;
      pattern = List.assoc "P6" bgs;
      exp_res = [];
      res = [];
    };
    (* TEST 7 *)
    {
      t_name = "T5";
      p_name = "P7";
      target = List.assoc "T5" bgs;
      pattern = List.assoc "P7" bgs;
      exp_res = [];
      res = [];
    };
    (* TEST 8 *)
    {
      t_name = "T5";
      p_name = "P8";
      target = List.assoc "T5" bgs;
      pattern = List.assoc "P8" bgs;
      exp_res = [];
      res = [];
    };
    (* TEST 9 *)
    {
      t_name = "T5";
      p_name = "P9";
      target = List.assoc "T5" bgs;
      pattern = List.assoc "P9" bgs;
      exp_res = [ (Iso.of_list [ (0, 0); (1, 1) ], Iso.of_list []) ];
      res = [];
    };
    (* TEST 10 *)
    {
      t_name = "T6";
      p_name = "P10";
      target = List.assoc "T6" bgs;
      pattern = List.assoc "P10" bgs;
      exp_res = [ (Iso.of_list [ (0, 0); (1, 1) ], Iso.of_list [ (0, 0) ]) ];
      res = [];
    };
    (* TEST 11 *)
    {
      t_name = "T7";
      p_name = "P11";
      target = List.assoc "T7" bgs;
      pattern = List.assoc "P11" bgs;
      exp_res = [ (Iso.of_list [ (0, 0); (1, 1) ], Iso.of_list []) ];
      res = [];
    };
    (* TEST 12 *)
    {
      t_name = "T8";
      p_name = "P12";
      target = List.assoc "T8" bgs;
      pattern = List.assoc "P12" bgs;
      exp_res = [ (Iso.of_list [ (0, 0); (1, 1) ], Iso.of_list [ (0, 0) ]) ];
      res = [];
    };
    (* TEST 13 *)
    {
      t_name = "T9";
      p_name = "P13";
      target = List.assoc "T9" bgs;
      pattern = List.assoc "P13" bgs;
      exp_res = [ (Iso.of_list [ (0, 0); (1, 1) ], Iso.of_list []) ];
      res = [];
    };
    (* TEST 14 *)
    {
      t_name = "T9";
      p_name = "P14";
      target = List.assoc "T9" bgs;
      pattern = List.assoc "P14" bgs;
      exp_res = [ (Iso.of_list [ (0, 0); (1, 1) ], Iso.of_list [ (0, 0) ]) ];
      res = [];
    };
    (* TEST 15 *)
    {
      t_name = "T9";
      p_name = "P15";
      target = List.assoc "T9" bgs;
      pattern = List.assoc "P15" bgs;
      exp_res = [];
      res = [];
    };
    (* TEST 16 *)
    {
      t_name = "T10";
      p_name = "P16";
      target = List.assoc "T10" bgs;
      pattern = List.assoc "P16" bgs;
      exp_res = [];
      (* infinite matches *)
      res = [];
    };
    (* TEST 17 *)
    {
      t_name = "T11";
      p_name = "P17";
      target = List.assoc "T11" bgs;
      pattern = List.assoc "P17" bgs;
      exp_res =
        [
          (Iso.of_list [ (0, 0) ], Iso.of_list []);
          (Iso.of_list [ (0, 1) ], Iso.of_list []);
        ];
      res = [];
    };
    (* TEST 18 *)
    {
      t_name = "T11";
      p_name = "P18";
      target = List.assoc "T11" bgs;
      pattern = List.assoc "P18" bgs;
      exp_res = [];
      res = [];
    };
    (* TEST 19 *)
    {
      t_name = "T12";
      p_name = "P19";
      target = List.assoc "T12" bgs;
      pattern = List.assoc "P19" bgs;
      exp_res =
        [
          (Iso.of_list [ (0, 2) ], Iso.of_list []);
          (Iso.of_list [ (0, 1) ], Iso.of_list []);
        ];
      res = [];
    };
    (* TEST 20 *)
    {
      t_name = "T12";
      p_name = "P20";
      target = List.assoc "T12" bgs;
      pattern = List.assoc "P20" bgs;
      exp_res = [];
      res = [];
    };
    (* TEST 21 *)
    {
      t_name = "T12";
      p_name = "P21";
      target = List.assoc "T12" bgs;
      pattern = List.assoc "P21" bgs;
      exp_res = [ (Iso.of_list [ (0, 0) ], Iso.of_list []) ];
      res = [];
    };
    (* TEST 22 *)
    {
      t_name = "T12";
      p_name = "P12";
      target = List.assoc "T12" bgs;
      pattern = List.assoc "P22" bgs;
      exp_res = [];
      res = [];
    };
    (* TEST 23 *)
    {
      t_name = "T13";
      p_name = "P23";
      target = List.assoc "T13" bgs;
      pattern = List.assoc "P23" bgs;
      exp_res = [];
      (* infinite matches *)
      res = [];
    };
    (* TEST 24 *)
    {
      t_name = "T13";
      p_name = "P24";
      target = List.assoc "T13" bgs;
      pattern = List.assoc "P24" bgs;
      exp_res = [ (Iso.of_list [ (0, 0) ], Iso.of_list []) ];
      res = [];
    };
    (* TEST 25 *)
    {
      t_name = "T14";
      p_name = "P25";
      target = List.assoc "T14" bgs;
      pattern = List.assoc "P25" bgs;
      exp_res = [ (Iso.of_list [ (0, 2); (1, 3) ], Iso.of_list []) ];
      res = [];
    };
    (* TEST 26 *)
    {
      t_name = "T15";
      p_name = "P26";
      target = List.assoc "T15" bgs;
      pattern = List.assoc "P26" bgs;
      exp_res =
        [
          ( Iso.of_list [ (0, 2); (1, 3); (2, 5); (3, 4); (4, 6) ],
            Iso.of_list [] );
          ( Iso.of_list [ (0, 2); (1, 3); (2, 4); (3, 5); (4, 6) ],
            Iso.of_list [] );
        ];
      res = [];
    };
    (* TEST 27 *)
    {
      t_name = "T16";
      p_name = "T16";
      target = List.assoc "T16" bgs;
      pattern = List.assoc "T16" bgs;
      exp_res =
        [
          ( Iso.of_list [ (0, 0); (1, 1); (2, 2); (3, 3); (4, 4) ],
            Iso.of_list [] );
        ];
      res = [];
    };
    (* TEST 28 *)
    {
      t_name = "T17";
      p_name = "P27";
      target = List.assoc "T17" bgs;
      pattern = List.assoc "P27" bgs;
      exp_res = [ (Iso.of_list [ (0, 1); (1, 2) ], Iso.of_list [ (0, 0) ]) ];
      res = [];
    };
    (* TEST 29 *)
    {
      t_name = "T18";
      p_name = "P27";
      target = List.assoc "T18" bgs;
      pattern = List.assoc "P27" bgs;
      exp_res = [ (Iso.of_list [ (0, 1); (1, 5) ], Iso.of_list [ (0, 0) ]) ];
      (* or (0,1)? *)
      res = [];
    };
    (* TEST 30 *)
    (* closed edges have to be iso *)
    {
      t_name = "T19";
      p_name = "P27";
      target = List.assoc "T19" bgs;
      pattern = List.assoc "P27" bgs;
      exp_res = [ (Iso.of_list [ (0, 4); (1, 8) ], Iso.of_list [ (0, 2) ]) ];
      res = [];
    };
    (* TEST 31 *)
    {
      t_name = "T19";
      p_name = "P28";
      target = List.assoc "T19" bgs;
      pattern = List.assoc "P28" bgs;
      (* no edges *)
      exp_res =
        [
          (Iso.of_list [ (0, 0) ], Iso.of_list []);
          (Iso.of_list [ (0, 4) ], Iso.of_list []);
          (Iso.of_list [ (0, 5) ], Iso.of_list []);
        ];
      res = [];
    };
    (* TEST 32 *)
    {
      t_name = "T20";
      p_name = "P29";
      target = List.assoc "T20" bgs;
      pattern = List.assoc "P29" bgs;
      exp_res = [ (Iso.of_list [ (0, 2); (1, 3) ], Iso.of_list []) ];
      res = [];
    };
    (* TEST 33 *)
    {
      t_name = "T21";
      p_name = "P30";
      target = List.assoc "T21" bgs;
      pattern = List.assoc "P30" bgs;
      exp_res = [ (Iso.of_list [ (0, 1); (1, 2) ], Iso.of_list []) ];
      res = [];
    };
    (*vvvvvvvvvvvvvvvvvvvv EXAMPLES from the THESIS vvvvvvvvvvvvvvvvv*)
    (* TEST 34 *)
    {
      t_name = "T22";
      p_name = "P31";
      target = List.assoc "T22" bgs;
      pattern = List.assoc "P31" bgs;
      exp_res = [ (Iso.of_list [ (0, 3); (1, 6) ], Iso.of_list []) ];
      res = [];
    };
    (* TEST 35 *)
    {
      t_name = "T22";
      p_name = "P32";
      target = List.assoc "T22" bgs;
      pattern = List.assoc "P32" bgs;
      exp_res =
        [
          (Iso.of_list [ (0, 0); (1, 1); (2, 4) ], Iso.of_list []);
          (Iso.of_list [ (0, 0); (1, 2); (2, 5) ], Iso.of_list []);
        ];
      res = [];
    };
    (* TEST 36 *)
    {
      t_name = "T22";
      p_name = "P33";
      target = List.assoc "T22" bgs;
      pattern = List.assoc "P33" bgs;
      exp_res = [];
      res = [];
    };
    (* TEST 37 *)
    {
      t_name = "T23";
      p_name = "P34";
      target = List.assoc "T23" bgs;
      pattern = List.assoc "P34" bgs;
      exp_res = [];
      res = [];
    };
    (* TEST 38 *)
    {
      t_name = "T26";
      p_name = "P37";
      target = List.assoc "T26" bgs;
      pattern = List.assoc "P37" bgs;
      exp_res = [];
      res = [];
    };
    (* TEST 39 *)
    {
      t_name = "T27";
      p_name = "P38";
      target = List.assoc "T27" bgs;
      pattern = List.assoc "P38" bgs;
      exp_res = [];
      res = [];
    };
    (* TEST 40 *)
    {
      t_name = "T28";
      p_name = "P39";
      target = List.assoc "T28" bgs;
      pattern = List.assoc "P39" bgs;
      exp_res =
        [
          ( Iso.of_list [ (0, 2); (1, 1); (2, 6); (3, 5); (4, 8) ],
            Iso.of_list [] );
        ];
      res = [];
    };
  ]

(* Args: PATH PATH-out*)
let () =
  Printexc.record_backtrace true;
  let bgs =
    IO.parse_all Sys.argv.(1) (fun x -> Filename.check_suffix x ".big")
    |> List.map (fun (n, ls) -> (n, Big.of_string ls))
  in
  let ts = tests bgs in
  let testcases_match = do_tests ts
  and testcases_eq = do_equality_tests bgs ts
  and testcases_to_string = do_to_string_tests bgs in
  print_endline "OK";
  IO.mkdir Sys.argv.(2);
  ST.(
    write_xml
      (testsuite "test_match" testcases_match)
      Sys.argv.(2) "match-junit.xml";
    write_xml (testsuite "test_eq" testcases_eq) Sys.argv.(2) "eq-junit.xml";
    write_xml
      (testsuite "test_to_string" testcases_to_string)
      Sys.argv.(2) "to-string-junit.xml");
  print_endline "Done!";
  exit 0
