open Big_json

let () =
  let minify = ref false and in_ch = ref stdin and out_ch = ref stdout in
  let speclist =
    [
      ( "-i",
        Arg.String (fun s -> in_ch := open_in s),
        "<file>  Read input from <file>" );
      ( "-o",
        Arg.String (fun s -> out_ch := open_out s),
        "<file>  Write output to <file>" );
      ("-m", Arg.Set minify, "  Minify output");
    ]
  in
  Arg.parse speclist (fun _ -> ()) "Usage: big_match <options>\noptions are:";
  big_match ~minify:!minify !in_ch !out_ch;
  exit 0
