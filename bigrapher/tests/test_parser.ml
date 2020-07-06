open Printf
module ST = CI_utils.Shippable_test
module IO = CI_utils.Io

let out_dir name = "./shippable/parser/" ^ name

let out_dir_res = "./shippable/testresults/"

let l_out name = "-l " ^ name ^ ".csl"

let prism_out name = "-p " ^ name ^ ".tra"

let ts_out name = "-s -t " ^ name

let extra_flags = "--no-colors --debug -M 140"

let path = "./files"

let ext = ".reference"

let set_args name =
  let path = out_dir name in
  IO.mkdir path;
  let n = Filename.concat path name in
  [ l_out n; prism_out n; ts_out n; extra_flags ] |> String.concat " "

let attr_string =
  [ ("type", "ASSERT_OUTPUT"); ("message", "Output is not the same") ]

let rec diff (out : string list) (reference : string list) =
  match (out, reference) with
  | [], [] -> []
  | l :: out, l_ref :: reference ->
      if String.compare l l_ref = 0 then diff out reference
      else
        [ ST.xml_block "failure" attr_string [ sprintf "%s != %s" l l_ref ] ]
  | [], l_ref :: _ ->
      [ ST.xml_block "failure" attr_string [ sprintf "nil != %s" l_ref ] ]
  | l :: _, [] ->
      [ ST.xml_block "failure" attr_string [ sprintf "%s != nil" l ] ]

(* Args: BIGRAPHER_PATH PATH OUT-PATH FNAME *)
let () =
  printf "test_parser:\n";
  let bin = Sys.argv.(1) ^ " full " in
  let (testcases : (string * string * string * string list) list) =
    IO.parse Sys.argv.(2)
    |> List.map (fun f ->
           printf "%s\n" f;
           let name = Filename.chop_extension (Filename.basename f) in
           let flags = set_args name in
           let chan_in =
             Unix.open_process_in (bin ^ " " ^ flags ^ " " ^ f)
           in
           let std_out = IO.read_lines [] chan_in in
           ( name,
             __MODULE__,
             ST.xml_block "system-out" [] [ String.concat "\n" std_out ],
             let ref_out =
               Filename.concat path (name ^ ext)
               |> open_in |> IO.read_lines []
             in
             try diff std_out ref_out
             with e ->
               [ ST.(xml_block "error" attr_err [ Printexc.to_string e ]) ]
           ))
  in
  IO.mkdir out_dir_res;
  ST.(
    write_xml (testsuite "test_parser" testcases) Sys.argv.(3) Sys.argv.(4))
