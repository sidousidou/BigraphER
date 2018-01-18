(* Run some tests *)
open Big_json
open Bigraph


let c0 = Ctrl.C ("Park", [], 3)
and c1 = Ctrl.C ("Hospital", [Ctrl.I 12; Ctrl.S "Church Street"], 0)
and c2 = Ctrl.C ("Bridge", [Ctrl.F 3.6], 2)

let ctrl_test = [ c0; c1; c2 ]

let nodes_test =
  List.fold_left (fun n (i, c) -> Nodes.add i c n)
    Nodes.empty
    [ (0, c0) ; (1, c1); (2, c0); (3, c2) ]

let place_test =
  Place.parse ~regions:2 ~nodes:4 ~sites:2
    [ "110000";
      "010000";
      "001000";
      "001100";
      "000011";
      "000000" ]

let link_test =
  let open Link in
  List.fold_left (fun l (i, o, p) ->
    Lg.add { i = i; o = o; p = p} l)
    Lg.empty
    [ (Face.empty, Face.empty, Ports.add 0 Ports.empty);
      (Face.empty, Face.empty, Ports.add 0 Ports.empty);
      (Face.empty, Face.empty, Ports.add 0 Ports.empty
                               |> Ports.add 2
                               |> Ports.add 3);
      (Face.empty, Face.empty, Ports.add 2 Ports.empty);
      (Face.empty, parse_face ["x"], Ports.add 2 Ports.empty);
      (Face.empty, Face.empty, Ports.add 3 Ports.empty) ]

let big_test =
  let open Big in
    { p = place_test;
      n = nodes_test;
      l = link_test }

let r_test =
  Brs.parse_react_unsafe ~lhs:big_test ~rhs:big_test None

let pr_test =
  Pbrs.parse_react_unsafe ~lhs:big_test ~rhs:big_test 0.476 None

let sr_test =
  let eta = (Fun.of_list [ (0, 1); (1, 1) ]) in
  Sbrs.parse_react_unsafe ~lhs:big_test ~rhs:big_test 8.031 (Some eta)

let test v e d eq f =
  match d @@ e v with
  | Ok v' -> (if (eq v v') then "true"
              else "false "^ "\n" ^ (f v') ^ "\n" ^ (f v))
  | Error msg -> msg

let input b rs t f =
  let rs_j =
    List.map (fun r -> f r) rs
    |> String.concat ",\n" in
  "{\"state\": " ^ (big_to_json ~minify:false b) ^ ",\n\""
  ^ t ^ "\": [" ^ rs_j ^ "\n]\n}"

let () =
  print_endline "ENCODING";
  print_endline "Testing big_to_json:\n";
  print_endline @@ big_to_json ~minify:false big_test;
  print_newline ();
  print_endline "Testing react_to_json:\n";
  print_endline @@ react_to_json ~minify:false r_test;
  print_newline ();
  print_endline "Testing preact_to_json:\n";
  print_endline @@ preact_to_json ~minify:false pr_test;
  print_newline ();
  print_endline "Testing sreact_to_json:\n";
  print_endline @@ sreact_to_json ~minify:false sr_test;
  print_newline ();
  print_endline "Testing s_occs_to_json:\n";
  print_endline
  @@ s_occs_to_json ~minify:false @@ fst @@ Sbrs.step big_test [ sr_test ];
  print_newline ();
  print_endline "Testing matches_to_json:\n";
  print_endline
  @@ matches_to_json ~minify:false @@ Big.occurrences ~target:big_test ~pattern:big_test;
  print_newline ();
  print_endline "DECODING";
  print_string "Testing big_of_json: ";
  print_endline @@ test big_test big_to_json big_of_json Big.equal Big.to_string;
  print_string "Testing react_of_json: ";
  print_endline @@ test r_test react_to_json react_of_json Brs.equal_react Brs.string_of_react;
  print_string "Testing preact_of_json: ";
  print_endline @@ test pr_test preact_to_json preact_of_json Pbrs.equal_react Pbrs.string_of_react;
  print_string "Testing sreact_of_json: ";
  print_endline @@ test sr_test sreact_to_json sreact_of_json Sbrs.equal_react Sbrs.string_of_react;
  print_newline ();
  print_endline "MATCHING ENGINE";
  print_string "Testing Brs.step: ";
  print_endline @@ step ~minify:true @@ input big_test [ r_test ] "reacts" (react_to_json ~minify:true);   
  print_string "Testing Pbrs.step: ";
  print_endline @@ step ~minify:true @@ input big_test [ pr_test ] "preacts" (preact_to_json ~minify:false);   
  print_string "Testing Sbrs.step: ";
  print_endline @@ step ~minify:true @@ input big_test [ sr_test ] "sreacts" (sreact_to_json ~minify:false)

