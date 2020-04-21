(* Various IO utilities *)

(* Parse a list of files-names *)
let rec read_lines out file =
  try read_lines (input_line file :: out) file
  with End_of_file ->
    close_in file;
    List.rev out

let parse path = open_in path |> read_lines []

(* Parse all the bigraphs in one dir *)
let parse_all dir f_filter =
  Array.to_list (Sys.readdir dir)
  |> List.filter f_filter
  |> List.map (fun x ->
         ( Filename.chop_extension x,
           parse (Filename.concat dir x) |> String.concat "\n" ))

let safe_mkdir dir =
  if not (Sys.file_exists dir) then
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let mkdir dir =
  let rec aux dir =
    if not (Sys.file_exists dir) then (
      aux (Filename.dirname dir);
      safe_mkdir dir )
  in
  aux dir
