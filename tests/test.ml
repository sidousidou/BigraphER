open Printf
open Global

let verbose = ref false
let match_path = ref ""
let out_path = ref ""

let usage = "Usage: test TESTS-PATH [-o OUT-PATH] [-v]"

let speclist =
  [ ("-v", Arg.Unit (fun () -> verbose := true ), " Verbose output");
    ("TEST-PATH", Arg.String (fun s -> match_path := s), " Path to the directory containing the tests");
    ("-o", Arg.String (fun s -> out_path := s), "OUT-PATH Path to the output directory");
  ]
    
let print_header () =
  printf "\n%s\n\
          ========================================\n\n\
          %s %s\n\
          %s %s\n\
          %s %s\n\n\
          ----------------------------------------\n\n"
    (colorise `bold "Tests for library bigraph") 
    (colorise `bold "Hostname:") (Unix.gethostname ())
    (colorise `bold "OS type:") Sys.os_type 
    (colorise `bold "Command line:") (String.concat " " (Array.to_list Sys.argv))

let _ =
  try
    Arg.parse (Arg.align speclist) (fun path ->
      match_path := path) usage;
    if !match_path <> "" then begin
      Unix.access !match_path [Unix.W_OK; Unix.F_OK]; 
      if !out_path <> "" then begin
	Unix.access !out_path [Unix.W_OK; Unix.F_OK]; 
	Export.check_graphviz ();
	print_header ();
	Test_match.main !match_path ~path_out:!out_path !verbose;
	Test_brs.main !out_path !verbose
      end else begin
	print_header ();
	Test_match.main !match_path !verbose
      end 
    end else 
      raise (Arg.Bad ("Error: Argument missing.\n" ^ usage))
  with
  | Arg.Bad m -> prerr_endline m; exit 1
  | Unix.Unix_error (err, _, arg) -> 
    (eprintf "Error: cannot acccess %s: %s.\n" arg (Unix.error_message err); 
     exit 1)
  | e -> prerr_endline (Printexc.to_string e); exit 1
