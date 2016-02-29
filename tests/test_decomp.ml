(* Tests for various bigraph manipulation functions *)
open Junit
    
let test_prime_decomposition b =
  let comps  = List.map fst (Place.prime_components b.Big.p) in
  Big.(equal b { n = b.n;
            p = Place.tens_of_list comps;
            l = b.l;
          })

let attr_decomp = [("type", "ASSERT_DECOMP");
		   ("message", "Bigraphs are not equal")]
		    		    
let do_tests =
  List.map (fun (n, b) ->
	    try
              if test_prime_decomposition b then
		(n,
		 __MODULE__,
		 xml_block "system-out" [] ["Test passed."],
		 [])
              else
		(n,
		 __MODULE__,
		 xml_block "system-out" [] ["Test failed."],
		 [xml_block "failure" attr_decomp []])
            with
            | Place.NOT_PRIME ->
	       (n,
		__MODULE__,
		xml_block "system-out" [] ["Test passed. Place graph not decomposable into prime components."],
		[])
            | _ ->
	       (n,
		__MODULE__,
		xml_block "system-out" [] [error_msg],
		[xml_block "error" attr_err [Printexc.get_backtrace ()]]))
	       
 
(* Args: PATH OUT-PATH FNAME *)  
let () =
  Printexc.record_backtrace true;
  let bg_strings = Io.parse_all
		     Sys.argv.(1)
		     (fun x -> 
		      (Filename.check_suffix x ".big") && 
			((Filename.chop_extension x).[0] = 'T')) in
  let bgs = 
    List.filter (fun (_, b) -> 
		 b.Big.p.Place.s = 0
		) (List.map (fun (n, s) -> (n, Big.parse s)) bg_strings) in
  let testcases = do_tests bgs in
  write_xml (testsuite "test_decomp" testcases) Sys.argv.(2) Sys.argv.(3)
  

