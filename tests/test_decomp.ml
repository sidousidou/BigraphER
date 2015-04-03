(* Tests for various bigraph manipulation functions *)
open Big
open Junit
       
(* parse a .big file *)
let parse path =
  let file = open_in path in
  let rec read_lines out =
    try read_lines ((input_line file) :: out)
    with 
    | End_of_file -> (close_in file; List.rev out)
  in read_lines []
  
(* parse all the bigraphs in one dir with file name "T*.big" *)
let parse_all dir =
  let files = Array.to_list (Sys.readdir dir) in
  List.map (fun x -> 
      (Filename.chop_extension x, parse (Filename.concat dir x))
    ) (List.filter (fun x -> 
      (Filename.check_suffix x ".big") && 
      ((Filename.chop_extension x).[0] = 'T')
    ) files)
    
let test_prime_decomposition b =
  let comps  = List.map fst (Place.prime_components b.p) in
  equal b { n = b.n;
            p = Place.tens_of_list comps;
            l = b.l;
          }

let attr_decomp = [("type", "ASSERT_DECOMP");
		   ("message", "Bigraphs are not equal")]
		    		    
let do_tests =
  List.map (fun (n, b) ->
	    try
              if test_prime_decomposition b then
		(n,
		 "test_decomp.ml",
		 xml_block "system-out" [] ["Test passed."],
		 [])
              else
		(n,
		 "test_decomp.ml",
		 xml_block "system-out" [] ["Test failed."],
		 [xml_block "failure" attr_decomp []])
            with
            | Place.NOT_PRIME ->
	       (n,
		"test_decomp.ml",
		xml_block "system-out" [] ["Test passed. Place graph not decomposable into prime components."],
		[])
            | e ->
	       (n,
		"test_decomp.ml",
		xml_block "system-out" [] [error_msg],
		[xml_block "error" attr_err [Printexc.to_string e]]))
	       
 
(* Args: PATH OUT-PATH FNAME *)  
let () =
  let bg_strings = parse_all Sys.argv.(1) in
  let bgs = 
    List.filter (fun (_, b) -> 
		 b.p.Place.s = 0
		) (List.map (fun (n, s) -> (n, Big.parse s)) bg_strings) in
  let testcases = do_tests bgs in
  write_xml (testsuite "test_decomp" testcases) Sys.argv.(2) Sys.argv.(3)
  

