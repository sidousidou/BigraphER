(* Tests for various bigraph manipulation functions *)
open Junit

let test_rpo_epi ((b0', b1'), b') =
  (Big.is_epi b0') && (Big.is_epi b1') && (Big.is_epi b')

let test_rpo_comp (a0, a1) (d0, d1) ((b0, b1), b) ((b0', b1'), b') =
  (Big.equal (Big.comp d0 a0) (Big.comp d1 a1))
  && (Big.equal (Big.comp b b0) d0)
  && (Big.equal (Big.comp b b1) d1)
  && (Big.equal (Big.comp b b0) (Big.comp b' b0'))
  && (Big.equal (Big.comp b b1) (Big.comp b' b1'))

let test_rpo_bound (a0, a1) (d0, d1) ((b0', b1'), b') =
  (Big.equal (Big.comp b0' a0) (Big.comp b1' a1))
  && (Big.equal (Big.comp b' b0') d0)
  && (Big.equal (Big.comp b' b1') d1)


let attr_eq = [("type", "ASSERT_RPO");
	       ("message", "Bigraphs are not equal")]

let attr_epi = [("type", "ASSERT_RPO");
		("message", "Bigraph is not epi")]

let attr_bnd = [("type", "ASSERT_RPO");
                ("message", "RPO is not a relative bound")]
		 
let do_tests =
  List.map (fun (n, (a, d, b, isos)) ->
      try
        let b' = Rpo.rpo a d isos
        and path = "shippable/svg/rpoTest" ^ n in
        ignore (Big.write_svg (fst a) ~name:"A0Input.svg" ~path:path);
        ignore (Big.write_svg (snd a) ~name:"A1Input.svg" ~path:path);
        ignore (Big.write_svg (fst d) ~name:"D0Input.svg" ~path:path);
        ignore (Big.write_svg (snd d) ~name:"D1Input.svg" ~path:path);
        ignore (Big.write_svg (fst (fst b)) ~name:"B0Expected.svg" ~path:path);
        ignore (Big.write_svg (snd (fst b)) ~name:"B1Expected.svg" ~path:path);
        ignore (Big.write_svg (snd b) ~name:"BExpected.svg" ~path:path);
        ignore (Big.write_svg (fst (fst b')) ~name:"B0Actual.svg" ~path:path);
        ignore (Big.write_svg (snd (fst b')) ~name:"B1Actual.svg" ~path:path);
        ignore (Big.write_svg (snd b') ~name:"BActual.svg" ~path:path);
        if test_rpo_comp a d b b' then
          if test_rpo_bound a d b' then
            if test_rpo_epi b' then
              (n,
              __MODULE__,
              xml_block "system-out" [] ["Test passed."],
              [])
            else
              (n,
              __MODULE__,
              xml_block "system-out" [] ["Test failed."],
              [xml_block "failure" attr_epi []])
          else
            (n,
            __MODULE__,
            xml_block "system-out" [] ["Test failed."],
            [xml_block "failure" attr_bnd []])
        else
          (n,
          __MODULE__,
          xml_block "system-out" [] ["Test failed."],
          [xml_block "failure" attr_eq []])
      with
      | _ ->
	 (n,
	  __MODULE__,
	  xml_block "system-out" [] [error_msg],
	  [xml_block "error" attr_err [Printexc.get_backtrace ()]]))
	       
           
(* Args: OUT-PATH FNAME *)  
let () =
  print_endline "test_rpo";
  Printexc.record_backtrace true;
  let id n = Big.id (Big.Inter (n, Link.Face.empty))
  and node ct = (Big.ion (Link.Face.empty) (Ctrl.C (ct, 0)))
  and atom ct = (Big.atom (Link.Face.empty) (Ctrl.C (ct, 0))) in
  let bgs =
    (* ((a0, a1), (d0, d1), ((b0, b1), b), isos) *)
    (* (Rpo.bound * Rpo.bound * Rpo.rpo * (int Iso.t * ....))*)

    [(                            (********** TEST 0 (Sample 1) **********)

       (                          (* A Bound *)
         (Big.ppar                (* A0 *)
           (id 1)
           (Big.share
             (Big.ppar
               (Big.ppar
                 (node "V0")
                 (node "V1"))
               (atom "V2"))
             (Big.placing [[0;1];[1;2;3];[2;3]] 4 Link.Face.empty)
             (Big.ppar
               (Big.ppar
                 (Big.ppar
                   (id 1)
                   (node "V5"))
                 (id 1))
               (node "V7")))),
         (Big.share               (* A1 *)
           (Big.ppar
             (Big.ppar
               (Big.ppar
                 (id 1)
                 (node "V0"))
               (node "V1"))
             (atom "V2"))
           (Big.placing [[0];[0;1];[2];[3]] 4 Link.Face.empty)
           (Big.ppar
             (node "V3")
             (id 3)))),
       (                          (* D Bound *)
         (Big.ppar                (* D0 *)
           (Big.par
             (Big.par
               (Big.nest
                 (node "V3")
                 (Big.par
                   (id 1)
                   (id 1)))
               (atom "V4"))
             (id 1))
           (Big.par
             (node "V6")
             (id 1))),
         (Big.comp                (* D1 *)
           (Big.ppar
             (Big.par
               (id 1)
               (id 1))
             (id 1))
           (Big.ppar
             (id 1)
             (Big.share
               (id 3)
               (Big.placing [[0];[0;1;2];[1;2]] 3 Link.Face.empty)
               (Big.ppar
                 (Big.par
                   (atom "V4")
                   (node "V5"))
                 (Big.par
                   (node "V6")
                   (node "V7"))))))),
       (                          (* Expected RPO *)
         (                        (* B bound *)
           (Big.ppar              (* B0 *)
             (Big.share
               (id 2)
               (Big.placing [[0];[0;1]] 2 Link.Face.empty)
               (Big.ppar
                 (node "V3")
                 (id 1)))
             (id 3)),
           (Big.ppar              (* B1 *)
             (id 1)
             (Big.share
               (id 3)
               (Big.placing [[0;1];[1;2;3];[2;3]] 4 Link.Face.empty)
               (Big.ppar
                 (Big.ppar
                   (Big.ppar
                     (id 1)
                     (node "V5"))
                 (id 1))
               (node "V7"))))),
         (Big.comp                (* B *)
           (Big.ppar
             (Big.ppar
               (Big.par
                 (atom "V4")
                 (id 2))
               (Big.zero))
             (Big.par
               (node "V6")
               (id 1)))
           (Big.placing [[0];[2];[1];[3];[4]] 5 Link.Face.empty))),
       (                          (* ISOS *)  
         (Iso.of_list_exn [(2,1);(3,2);(4,3)]),
         (Iso.of_list_exn [(0,1);(1,3)]),
         (Iso.of_list_exn [(0,0)]),
         (Iso.of_list_exn [(1,0);(2,2)]))
     );

     (                            (********** TEST 1 (Sample 2) **********)

       (                          (* A bound *)
         (Big.ppar                (* A0 *)
           (node "V0")
           (id 1)),
         (Big.ppar                (* A1 *)
           (id 1)
           (Big.share
             (node "V2")
             (Big.placing [[0;1]] 2 Link.Face.empty)
             (Big.ppar
               (id 1)
               (node "V5"))))),
       (                          (* D bound *)
         (Big.comp                (* D0 *)
           (Big.share
             (id 3)
             (Big.placing [[0];[0;1];[1]] 2 Link.Face.empty)
             (Big.par
               (node "V6")
               (node "V4")))
           (Big.share
             (Big.ppar
               (node "V1")
               (node "V2"))
             (Big.placing [[0;1];[1;2]] 3 Link.Face.empty)
             (Big.ppar
               (Big.ppar
                 (id 1)
                 (node "V3"))
               (node "V5")))),
         (Big.comp                (* D1 *)
           (Big.share
             (id 3)
             (Big.placing [[0];[0;1];[1]] 2 Link.Face.empty)
             (Big.par
               (node "V6")
               (node "V4")))
           (Big.ppar
             (Big.share
               (Big.ppar
                 (Big.nest
                   (node "V1")
                   (node "V0"))
                 (id 1))
               (Big.placing [[0;1];[1]] 2 Link.Face.empty)
               (Big.ppar
                 (id 1)
                 (node "V3")))
             (id 1)))),
       (                          (* Expected RPO *)
         (                        (* B bound *)
           (Big.ppar              (* B0 *)
             (id 1)
             (Big.share
               (node "V2")
               (Big.placing [[0;1]] 2 Link.Face.empty)
               (Big.ppar
                 (id 1)
                 (node "V5")))),
           (Big.ppar              (* B1 *)
             (node "V0")
             (id 2))),
         (Big.comp                (* B *)
           (Big.share
             (id 3)
             (Big.placing [[0];[0;1];[1]] 2 Link.Face.empty)
             (Big.par
               (node "V6")
               (node "V4")))
           (Big.ppar
             (Big.share
               (Big.ppar
                 (node "V1")
                 (id 1))
               (Big.placing [[0;1];[1]] 2 Link.Face.empty)
               (Big.ppar
                 (id 1)
                 (node "V3")))
             (id 1)))),
       (                          (* ISOS *)
         (Iso.of_list_exn []),
         (Iso.of_list_exn [(0,4)]),
         (Iso.of_list_exn [(5,1);(3,0)]),
         (Iso.of_list_exn [(0,0);(1,1);(2,2);(4,3)]))
     );

     (                           (********** TEST 2 (Sample 3) **********)
       
       (                         (* A Bound *)
         (Big.ppar               (* A0 *)
           (node "V0")
           (id 1)),
         (Big.share               (* A1 *)
           (Big.ppar
             (Big.nest
               (node "V1")
               (node "V0"))
             (node "V2"))
           (Big.placing [[0;1];[1;2]] 3 Link.Face.empty)
           (Big.ppar
             (Big.ppar
               (node "V6")
               (node "V3"))
             (node "V5")))),
       (                          (* D Bound *)
         (Big.comp                (* D0 *)
           (Big.share
             (id 3)
             (Big.placing [[0];[1];[1]] 2 Link.Face.empty)
             (Big.par
               (node "V6")
               (node "V4")))
           (Big.share
             (Big.ppar
               (node "V1")
               (node "V2"))
             (Big.placing [[0;1];[1;2]] 3 Link.Face.empty)
             (Big.ppar
               (Big.ppar
                 (id 1)
                 (node "V3"))
               (node "V5")))),
         (Big.share               (* D1 *)
           (id 3)
           (Big.placing [[0];[1];[1]] 2 Link.Face.empty)
           (Big.par
             (id 1)
             (node "V4")))),
       (                          (* Expected RPO *)
         (                        (* B bound *)
           (Big.share             (* B0 *)
             (Big.ppar
               (node "V1")
               (node "V2"))
             (Big.placing [[0;1];[1;2]] 3 Link.Face.empty)
             (Big.ppar
               (Big.ppar
                 (node "V6")
                 (node "V3"))
               (node "V5"))),
           (id 3)),               (* B1 *)
         (Big.share               (* B *)
           (id 3)
           (Big.placing [[0];[1];[1]] 2 Link.Face.empty)
           (Big.par
             (id 1)
             (node "V4")))),
       (                          (* ISOS *)
         (Iso.of_list_exn [(0,4)]),
         (Iso.of_list_exn []),
         (Iso.of_list_exn [(0,0);(2,1);(3,2);(4,3);(5,5)]),
         (Iso.of_list_exn [(1,0)]))
     );

     (                            (********** TEST 3 (Sample 4) **********)

       (                          (* A bound *)
         (Big.ppar                (* A0 *)
           (Big.merge 2)
           (Big.merge 2)),
         (Big.ppar                (* A1 *)
           (Big.merge 3)
           (id 1))),
       (                          (* D bound *)
         (Big.nest                (* D0 *)
           (node "V0")
           (Big.merge 2)),
         (Big.nest                (* D1 *)
           (node "V0")
           (Big.merge 2))),
       (                          (* Expected RPO *)
         (                        (* B bound *)
           (Big.merge 2),         (* B0 *)
           (Big.merge 2)),        (* B1 *)
         (node "V0")),            (* B *)
       (                          (* ISOS *)
         (Iso.of_list_exn []),
         (Iso.of_list_exn []),
         (Iso.of_list_exn []),
         (Iso.of_list_exn [(0,0)]))
     );

     (                            (********** TEST 4 (Sample 5) **********)

       (                          (* A bound *)
         (Big.comp                (* A0 *)
           (Big.share
             (id 3)
             (Big.placing [[0];[1;2];[3]] 4 Link.Face.empty)
             (Big.ppar
               (Big.par
                 (id 1)
                 (node "V2"))
               (id 2)))
           (Big.ppar
             (node "V0")
             (Big.share
               (id 2)
               (Big.placing [[0];[0;1]] 2 Link.Face.empty)
               (Big.ppar
                 (node "V1")
                 (id 1))))),
         (Big.ppar                (* A1 *)
           (id 1)
           (Big.comp
             (Big.share
               (id 3)
               (Big.placing [[0];[1;2];[2]] 3 Link.Face.empty)
               (Big.ppar
                 (id 2)
                 (node "V7")))
             (Big.comp
               (Big.share
                 (id 2)
                 (Big.placing [[0;1];[2]] 3 Link.Face.empty)
                 (Big.ppar
                   (Big.ppar
                     (id 1)
                     (node "V3"))
                   (id 1)))
               (Big.share
                 (id 2)
                 (Big.placing [[0];[0;1]] 2 Link.Face.empty)
                 (Big.ppar
                   (node "V1")
                   (node "V4"))))))),
       (                          (* D bound *)
         (Big.ppar                (* D0 *)
           (Big.ppar
             (node "V5")
             (atom "V6"))
           (Big.share
             (Big.ppar
               (node "V3")
               (node "V4"))
             (Big.placing [[0;1];[1]] 2 Link.Face.empty)
             (Big.ppar
               (id 1)
               (node "V7")))),
         (Big.ppar                (* D1 *)
           (Big.ppar
             (Big.nest
               (node "V5")
               (Big.par
                 (node "V0")
                 (node "V2")))
             (atom "V6"))
           (id 2))),
       (                          (* Expected RPO *)
         (                        (* B bound *)
           (Big.comp              (* B0 *)
             (Big.ppar
               (id 2)
               (Big.share
                 (id 2)
                 (Big.placing [[0;1];[1]] 2 Link.Face.empty)
                 (Big.ppar
                   (id 1)
                   (node "V7"))))
             (Big.ppar
               (Big.ppar
                 (id 1)
                 (Big.share
                   (id 1)
                   (Big.placing [[0;1]] 2 Link.Face.empty)
                   (Big.ppar
                     (id 1)
                     (node "V3"))))
               (node "V4"))),
           (Big.ppar              (* B1 *)
             (Big.share
               (Big.ppar
                 (node "V0")
                 (id 1))
               (Big.placing [[0];[1;2]] 3 Link.Face.empty)
               (Big.ppar
                 (Big.par
                   (id 1)
                   (node "V2"))
                 (id 1)))
             (id 2))),
         (Big.ppar                (* B *)
           (Big.ppar
             (Big.ppar
               (node "V5")
               (atom "V6"))
             (Big.zero))
           (id 2))),
       (                          (* ISOS *)
         (Iso.of_list_exn [(2,2)]),
         (Iso.of_list_exn [(1,1);(0,2)]),
         (Iso.of_list_exn [(3,1);(4,3);(2,0)]),
         (Iso.of_list_exn [(0,0);(1,3)]))
     );

     (                            (********* TEST 5 (Sample 6) **********)
       
       (                          (* A bound *)
         (Big.comp                (* A0 *)
           (Big.share
             (Big.ppar
               (node "V0")
               (id 1))
             (Big.placing [[0;1];[1]] 2 Link.Face.empty)
             (Big.ppar
               (id 1)
               (node "V1")))
           (Big.share
             (id 1)
             (Big.placing [[0;1]] 2 Link.Face.empty)
             (id 2))),
         (Big.share               (* A1 *)
           (id 1)
           (Big.placing [[0;1]] 2 Link.Face.empty)
           (Big.par
             (node "V0")
             (id 1)))),
       (                          (* D bound *)
         (Big.ppar                (* D0 *)
           Big.zero
           (id 1)),
         (node "V1")),            (* D1 *)
       (                          (*Expected RPO *)
         (                        (* B bound *)
           (Big.ppar              (* B0 *)
             Big.zero
             (id 1)),
           (node "V1")),          (* B1 *)
         (id 1)),                 (* B *)
       (                          (* ISOS *)
         (Iso.of_list_exn [(1,0)]),
         (Iso.of_list_exn [(0,0)]),
         (Iso.of_list_exn []),
         (Iso.of_list_exn []))
     )
         
] in
  let testcases =
    List.length bgs
    |> IntSet.of_int
    |> IntSet.elements
    |> List.map string_of_int 
    |> Base.flip List.combine bgs
    |> do_tests in
  write_xml (testsuite "test_rpo" testcases) Sys.argv.(1) Sys.argv.(2)
