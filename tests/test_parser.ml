open Printf
open Junit
open Utils

let bin = "./bigrapher.native full"
let out_dir name = "./shippable/parser/" ^ name
let dec_out path = "-f svg,dot -d " ^ path
let l_out name = "-l " ^ name ^ ".csl"
let prism_out name = "-p " ^ name ^ ".tra"
let ts_out name = "-s -t " ^ name
let extra_flags = "--no-colors --debug -M 140"	     
let path = "./tests/files"	     
let ext = ".reference"

let set_args name =
  let path = out_dir name in
  mkdir path;
  let n = path / name in 
  [ dec_out path; 
    l_out n; 
    prism_out n;
    ts_out n;
    extra_flags ]
  |> String.concat " "

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
  printf "test_parser:\n";
  let (testcases : (string * string * string * string list) list) =
    Io.parse Sys.argv.(1)
    |> List.map (fun f ->
        printf "%s\n" f;
        let name = Filename.chop_extension (Filename.basename f) in
        let flags = set_args name in
        let chan_in = Unix.open_process_in (bin ^ " " ^ flags ^ " " ^ f) in
        let std_out = Io.read_lines [] chan_in in	      
        (name,
         __MODULE__,
         xml_block "system-out" [] [String.concat "\n" std_out],
         let ref_out =
           Filename.concat path (name ^ ext)
           |> open_in
           |> Io.read_lines [] in
         try diff std_out ref_out with
         | e ->
           [xml_block "error" attr_err [Printexc.to_string e]])) in
  write_xml (testsuite "test_parser" testcases) Sys.argv.(2) Sys.argv.(3)
