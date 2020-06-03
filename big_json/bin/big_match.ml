open Big_json

let () =
  let minify = ref false
  and solver = ref "MSAT"
  and in_ch = ref stdin
  and out_ch = ref stdout in
  let speclist =
    [
      ( "-i",
        Arg.String (fun s -> in_ch := open_in s),
        "<file>  Read input from <file>" );
      ( "-o",
        Arg.String (fun s -> out_ch := open_out s),
        "<file>  Write output to <file>" );
      ("-m", Arg.Set minify, "  Minify output");
      ( "-s",
        Arg.String (fun s -> solver := s),
        "  Select solver. Allowed values are `MSAT` and `MCARD`" );
    ]
  in
  Arg.parse speclist (fun _ -> ()) "Usage: big_match <options>\noptions are:";
  big_match ~minify:!minify ~solver:!solver !in_ch !out_ch;
  exit 0
