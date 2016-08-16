(* Tests for various bigraph manipulation functions *)
open Junit


  assert (Big.is_epi b0);
  assert (Big.is_epi b1);
  assert (Big.is_epi b);

       

let test_rpo_comp (a0, a1) (d0, d1) (b0, b1, b) (b0', b1', b') isos =
   (Big.equal (Big.comp d0 a0) (Big.comp d1 a1)) &&
       (Big.equal (Big.comp b b0) d0) &&
         (Big.equal (Big.comp b b1) d1) &&
  let (b0', b1', b') = Rpo.rpo (a0, a1) (d0, d1) isos in
  assert (Big.is_epi b0');
  assert (Big.is_epi b1');
  assert (Big.is_epi b');
  assert (Big.equal (Big.comp b b0) (Big.comp b0' b'));
  assert (Big.equal (Big.comp b b1) (Big.comp b1' b'));

let test_rpo_epi 
  
let attr_eq = [("type", "ASSERT_RPO");
		   ("message", "Bigraphs are not equal")]

let attr_epi = [("type", "ASSERT_RPO");
		   ("message", "Bigraph is not epi")]
		    		    
let do_tests =
  List.map (fun (n, a, d, b, isos) ->
      try
        let (b0', b1', b') = Rpo.rpo a d b isos in
              if test_rpo_comp a d b isos then
		(n,
		 __MODULE__,
		 xml_block "system-out" [] ["Test passed."],
		 [])
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
  let bgs =
    (* (index, (a0, a1), (d0, d1), (b0, b1, b), isos) *)
    (* (int * Rpo.bound * Rpo.bound * Rpo.rpo * (int Iso.t * ....))*)
    [ (0,
       (Big.ppar
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
               (node "V7"))),
       Big.share
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
            (id 3))),
       (Big.ppar
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
            (id 1)),
       Big.comp
         (Big.ppar
            (Big.par
               (id 1)
               (id 1))
            (id 1))
         (Big.ppar
            (id 1)
              Big.share
         (id 3)
         (Big.placing [[0];[0;1;2];[1;2]] 3 Link.Face.empty)
         (Big.ppar
            (Big.par
               (atom "V4")
               (node "V5"))
            (Big.par
               (node "V6")
               (node "V7")))),
       Big.ppar
         (Big.share
            (id 2)
            (Big.placing [[0];[0;1]] 2 Link.Face.empty)
            (Big.ppar
               (node "V3")
               (id 1)))
         (id 3)),
       (Big.ppar
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
               (node "V7"))),
       Big.comp
         (Big.ppar
            (Big.ppar
               (Big.par
                  (atom "V4")
                  (id 2))
               (Big.zero))
            (Big.par
               (node "V6")
               (id 1)))
         (Big.placing [[0];[2];[1];[3];[4]] 5 Link.Face.empty),
  (Iso.of_list_exn [(2,1);(3,2);(4,3)],
  Iso.of_list_exn [(0,1);(1,3)],
  Iso.of_list_exn [(0,0)],
  Iso.of_list_exn [(1,0);(2,2)])
      )
      (*      (1, ....)*)
    ] in
  let testcases = do_tests bgs in
  write_xml (testsuite "test_rpo" testcases) Sys.argv.(1) Sys.argv.(2)
  


(********************************)




(* Run tests here *)
(* let () = *)
(*   print_endline "Tests for RPO algorithm"; *)
(*   let id n = Big.id (Big.Inter (n, Link.Face.empty)) *)
(*   and node ct = (Big.ion (Link.Face.empty) (Ctrl.C (ct, 0))) *)
(*   and atom ct = (Big.atom (Link.Face.empty) (Ctrl.C (ct, 0))) in *)
  
(*   let d1' = Big.share *)
(*     (id 3) *)
(*     (Big.placing [[0];[0;1;2];[1;2]] 3 Link.Face.empty) *)
(*     (Big.ppar *)
(*       (Big.par *)
(*         (atom "V4") *)
(*         (node "V5")) *)
(*       (Big.par *)
(*         (node "V6") *)
(*         (node "V7"))) *)
(*   and b' = Big.ppar *)
(*     (Big.ppar *)
(*       (Big.par *)
(*         (atom "V4") *)
(*         (id 2)) *)
(*       (Big.zero)) *)
(*     (Big.par *)
(*       (node "V6") *)
(*       (id 1)) in *)

(*   let a0 = Big.ppar *)
(*     (id 1) *)
(*     (Big.share *)
(*       (Big.ppar *)
(*         (Big.ppar *)
(*           (node "V0") *)
(*           (node "V1")) *)
(*         (atom "V2")) *)
(*       (Big.placing [[0;1];[1;2;3];[2;3]] 4 Link.Face.empty) *)
(*       (Big.ppar *)
(*         (Big.ppar *)
(*           (Big.ppar *)
(*             (id 1) *)
(*             (node "V5")) *)
(*           (id 1)) *)
(*         (node "V7"))) *)
(*   and a1 = Big.share *)
(*     (Big.ppar *)
(*       (Big.ppar *)
(*         (Big.ppar *)
(*           (id 1) *)
(*           (node "V0")) *)
(*         (node "V1")) *)
(*       (atom "V2")) *)
(*     (Big.placing [[0];[0;1];[2];[3]] 4 Link.Face.empty) *)
(*     (Big.ppar *)
(*       (node "V3") *)
(*       (id 3)) *)
(*   and d0 = Big.ppar *)
(*     (Big.par *)
(*       (Big.par *)
(*         (Big.nest *)
(*           (node "V3") *)
(*           (Big.par *)
(*             (id 1) *)
(*             (id 1))) *)
(*         (atom "V4")) *)
(*       (id 1)) *)
(*     (Big.par *)
(*       (node "V6") *)
(*       (id 1)) *)
(*   and d1 = Big.comp *)
(*     (Big.ppar *)
(*       (Big.par *)
(*         (id 1) *)
(*         (id 1)) *)
(*       (id 1)) *)
(*     (Big.ppar *)
(*       (id 1) *)
(*       d1') *)
(*   and b0 = Big.ppar *)
(*     (Big.share *)
(*       (id 2) *)
(*       (Big.placing [[0];[0;1]] 2 Link.Face.empty) *)
(*       (Big.ppar *)
(*         (node "V3") *)
(*         (id 1))) *)
(*     (id 3) *)
(*   and b1 = Big.ppar *)
(*     (id 1) *)
(*     (Big.share *)
(*       (id 3) *)
(*       (Big.placing [[0;1];[1;2;3];[2;3]] 4 Link.Face.empty) *)
(*       (Big.ppar *)
(*         (Big.ppar *)
(*           (Big.ppar *)
(*             (id 1) *)
(*             (node "V5")) *)
(*           (id 1)) *)
(*         (node "V7"))) *)
(*   and b = Big.comp *)
(*     b' *)
(*     (Big.placing [[0];[2];[1];[3];[4]] 5 Link.Face.empty) *)
(*   and i_a = Iso.of_list_exn [(2,1);(3,2);(4,3)] *)
(*   and i_a' = Iso.of_list_exn [(0,1);(1,3)] *)
(*   and i_d = Iso.of_list_exn [(0,0)] *)
(*   and i_d' = Iso.of_list_exn [(1,0);(2,2)] *)

(*   and path = "../repository/BigraphER_API_Samples/bigrapher01" in *)

  (* ignore (Big.write_svg a0 ~name:"A0Input.svg" ~path:path); *)
  (* ignore (Big.write_svg a1 ~name:"A1Input.svg" ~path:path); *)
  (* ignore (Big.write_svg d0 ~name:"D0Input.svg" ~path:path); *)
  (* ignore (Big.write_svg d1 ~name:"D1Input.svg" ~path:path); *)
  (* ignore (Big.write_svg b0 ~name:"B0Expected.svg" ~path:path); *)
  (* ignore (Big.write_svg b1 ~name:"B1Expected.svg" ~path:path); *)
  (* ignore (Big.write_svg b ~name:"BExpected.svg" ~path:path); *)
  assert (Big.equal (Big.comp d0 a0) (Big.comp d1 a1));
  assert (Big.equal (Big.comp b b0) d0);
  assert (Big.equal (Big.comp b b1) d1);
  assert (Big.is_epi b0);
  assert (Big.is_epi b1);
  assert (Big.is_epi b);
  let (b0', b1', b') = rpo (a0, a1) (d0, d1) i_a i_a' i_d i_d' in
  assert (Big.is_epi b0');
  assert (Big.is_epi b1');
  assert (Big.is_epi b');
  assert (Big.equal (Big.comp b b0) (Big.comp b0' b'));
  assert (Big.equal (Big.comp b b1) (Big.comp b1' b'));
  ignore (Big.write_svg b0' ~name:"B0Actual.svg" ~path:path);
  ignore (Big.write_svg b1' ~name:"B1Actual.svg" ~path:path);
  ignore (Big.write_svg b' ~name:"BActual.svg" ~path:path);
  print_endline "Test OK"
                
