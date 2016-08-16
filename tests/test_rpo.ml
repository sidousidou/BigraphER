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
  let bgs =
    (* ((a0, a1), (d0, d1), (b0, b1, b), isos) *)
    (* (int * Rpo.bound * Rpo.bound * Rpo.rpo * (int Iso.t * ....))*)
    [   ] in
  let testcases =
    List.length bgs
    |> IntSet.of_int
    |> IntSet.elements
    |> List.map string_of_int 
    |> Base.flip List.combine bgs
    |> do_tests in
  write_xml (testsuite "test_rpo" testcases) Sys.argv.(1) Sys.argv.(2)
