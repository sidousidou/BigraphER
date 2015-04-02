open Printf
open Junit

(* parse a list of files-names *)
let rec read_lines file out =
  try read_lines file ((input_line file) :: out)
  with 
  | End_of_file -> (close_in file; List.rev out)
 
let parse path =
  let file = open_in path in
  read_lines file []

let bin = "./bigrapher.native"
let flag = "--debug"
let path = "./tests/files"	     
let ext = ".reference"

let attr_string = [("type", "ASSERT_OUTPUT");
		   ("message", "Output is not the same")]

(* Args: PATH OUT-PATH FNAME *)  
let () =
  let tests_files = parse Sys.argv.(1) in
  let testcases =
    List.map (fun f ->
	      let name = Filename.chop_extension (Filename.basename f) in
	      let chan_in = Unix.open_process_in (bin ^ " " ^ f ^ " " ^ flag) in
	      let std_out = read_lines chan_in [] in	      
	      (name,
	       "test_parser.ml",
	       xml_block "system-out" [] [String.concat "\n" std_out],
	       let ref_out = Filename.concat path (name ^ ext) in
	       try List.map (fun (l, l_ref) ->
			     if String.compare l l_ref = 0 then ""
			     else xml_block "failure" attr_string [sprintf "%s != %s" l l_ref])
			    (List.combine std_out (read_lines (open_in ref_out) []))
	       with
	       | e ->
		  [xml_block "error" attr_err [Printexc.to_string e]])) tests_files in
  write_xml (testsuite "test_parser" testcases) Sys.argv.(2) Sys.argv.(3)
