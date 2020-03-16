(* Various IO utilities *)

(* Parse a list of files-names *)
let rec read_lines out file =
  try read_lines ((input_line file) :: out) file
  with
  | End_of_file -> (close_in file; List.rev out)

let parse path =
  open_in path
  |> read_lines []

(* Parse all the bigraphs in one dir *)
let parse_all dir f_filter =
  Array.to_list (Sys.readdir dir)
  |> List.filter f_filter
  |> List.map (fun x ->
      ((Filename.chop_extension x),
       (parse (Filename.concat dir x)
        |> String.concat "\n")))
