(* Tests for various bigraph manipulation functions *)
open Junit

let test_rpo_epi ((b0', b1'), b') =
  (Big.is_epi b0') && (Big.is_epi b1') && (Big.is_epi b')

let test_rpo_comp (a0, a1) (d0, d1) (b0, b1, b) ((b0', b1'), b') =
  (Big.equal (Big.comp d0 a0) (Big.comp d1 a1))
  && (Big.equal (Big.comp b b0) d0)
  && (Big.equal (Big.comp b b1) d1)
  && (Big.equal (Big.comp b b0) (Big.comp b' b0'))
  && (Big.equal (Big.comp b b1) (Big.comp b' b1'))

let attr_eq = [("type", "ASSERT_RPO");
	       ("message", "Bigraphs are not equal")]

let attr_epi = [("type", "ASSERT_RPO");
		("message", "Bigraph is not epi")]
		 
let do_tests =
  List.map (fun (n, (a, d, b, isos)) ->
      try
        let b' = Rpo.rpo a d isos in
        if test_rpo_comp a d b b' then
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
    (* ((a0, a1), (d0, d1), (b0, b1, b), isos) *)
    (* (Rpo.bound * Rpo.bound * Rpo.rpo * (int Iso.t * ....))*)

    [(                            (********** TEST 1 **********)

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
         (Big.ppar                (* B0 *)
           (Big.share
             (id 2)
             (Big.placing [[0];[0;1]] 2 Link.Face.empty)
             (Big.ppar
               (node "V3")
               (id 1)))
           (id 3)),
         (Big.ppar                (* B1 *)
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
             (node "V7")))),
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
         (Iso.of_list_exn [(1,0);(2,2)])));

     (                            (********** TEST 2 **********)

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
         (Big.ppar                (* B0 *)
           (id 1)
           (Big.share
             (node "V2")
             (Big.placing [[0;1]] 2 Link.Face.empty)
             (Big.ppar
               (id 1)
               (node "V5")))),
         (Big.ppar                (* B1 *)
           (node "V0")
           (id 2)),
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
