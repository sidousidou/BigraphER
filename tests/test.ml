open Printf
  
open Global
  
let verbose = ref false
  
let match_path = ref ""
  
let out_path = ref ""
  
let match_mask = ref 0b11
  
let brs_mask = ref 0b1111
  
let usage = "Usage: test TESTS-PATH [-o OUT-PATH] [-v] [-m MASK]\n"
  
let parse_mask mask =
  try
    let split =
      (String.sub mask 0 1) ^
        (" " ^ (String.sub mask 1 ((String.length mask) - 1))) in
    let (m, b) =
      Scanf.sscanf split "%d %d"
        (fun x y ->
           if (x <= 0b11) && ((x >= 0) && ((y >= 0) && (y <= 0b1111)))
           then (x, y)
           else raise (Arg.Bad (sprintf "Error: invalid mask %s" mask)))
    in (match_mask := m; brs_mask := b)
  with | _ -> raise (Arg.Bad (sprintf "Error: invalid mask %s" mask))
  
let speclist =
  [ ("TEST-PATH", (Arg.String (fun s -> match_path := s)),
     " Path to the directory containing the tests");
    ("-m", (Arg.String (fun s -> parse_mask s)),
     "MASK Binary mask to exclude/include tests");
    ("-o", (Arg.String (fun s -> out_path := s)),
     "OUT-PATH Path to the output directory");
    ("-v", (Arg.Unit (fun () -> verbose := true)), " Verbose output") ]
  
let print_header () =
  printf
    "\n%s\n\
          ========================================\n\n\
          %s %s\n\
          %s %s\n\
          %s %s\n\n\
          ----------------------------------------\n\n"
    (colorise `bold "Tests for library bigraph") (colorise `bold "Hostname:")
    (Unix.gethostname ()) (colorise `bold "OS type:") Sys.os_type
    (colorise `bold "Command line:")
    (String.concat " " (Array.to_list Sys.argv))
  
let _ =
  try
    (Arg.parse (Arg.align speclist) (fun path -> match_path := path) usage;
     if !match_path <> ""
     then
       (Unix.access !match_path [ Unix.W_OK; Unix.F_OK ];
        if !out_path <> ""
        then
          (Unix.access !out_path [ Unix.W_OK; Unix.F_OK ];
           Export.check_graphviz ();
           print_header ();
           Test_match.main !match_path ~path_out: !out_path !match_mask
             !verbose;
           Test_brs.main ~path: !out_path !brs_mask !verbose)
        else
          (print_header ();
           Test_match.main !match_path !match_mask !verbose;
           Test_brs.main !brs_mask !verbose))
     else raise (Arg.Bad ("Error: Argument missing.\n" ^ usage)))
  with | Arg.Bad m -> (prerr_endline m; exit 1)
  | Unix.Unix_error (err, _, arg) ->
      (eprintf "Error: cannot acccess %s: %s.\n" arg (Unix.error_message err);
       exit 1)
  | e -> (prerr_endline (Printexc.to_string e); exit 1)
  

