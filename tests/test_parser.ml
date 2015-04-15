open Printf
open Junit

(* parse a list of files-names *)
let rec read_lines out file =
  try read_lines ((input_line file) :: out) file
  with 
  | End_of_file -> (close_in file; List.rev out)
 
let parse path =
  let file = open_in path in
  read_lines [] file 

let bin = "./bigrapher.native"
let flag = "--debug"
let path = "./tests/files"	     
let ext = ".reference"

let attr_string = [("type", "ASSERT_OUTPUT");
		   ("message", "Output is not the same")]

let rec diff (out : string list) (reference : string list) =
  match (out, reference) with
  | ([], []) -> []
  | (l :: out, l_ref :: reference) ->
     (if String.compare l l_ref = 0 then diff out reference
      else [xml_block "failure" attr_string [sprintf "%s != %s" l l_ref]])
  | ([], l_ref :: _) ->
     [xml_block "failure" attr_string [sprintf "nil != %s" l_ref]]
  | (l :: _ , []) ->
     [xml_block "failure" attr_string [sprintf "%s != nil" l]]

(* Args: PATH OUT-PATH FNAME *)  
let () =
  let (testcases : (string * string * string * string list) list) =
    parse Sys.argv.(1)
    |> List.map (fun f ->
		 let name = Filename.chop_extension (Filename.basename f) in
		 let chan_in = Unix.open_process_in (bin ^ " " ^ f ^ " " ^ flag) in
		 let std_out = read_lines [] chan_in in	      
		 (name,
		  "__MODULE__",
		  xml_block "system-out" [] [String.concat "\n" std_out],
		  let ref_out =
		    Filename.concat path (name ^ ext)
		    |> open_in
		    |> read_lines [] in
		  try diff std_out ref_out with
		  | e ->
		     [xml_block "error" attr_err [Printexc.to_string e]])) in
  write_xml (testsuite "test_parser" testcases) Sys.argv.(2) Sys.argv.(3)
