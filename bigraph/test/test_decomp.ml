(* Tests for various bigraph manipulation functions *)
open Bigraph
module ST = CI_utils.Shippable_test
module IO = CI_utils.Io
module S = Solver.Make_SAT (Solver.MS)

let test_prime_decomposition b =
  let comps = List.map fst (Place.prime_components b.Big.p) in
  Big.(S.equal b { n = b.n; p = Place.tens_of_list comps; l = b.l })

let attr_decomp =
  [ ("type", "ASSERT_DECOMP"); ("message", "Bigraphs are not equal") ]

let do_tests =
  List.map (fun (n, b) ->
      try
        if test_prime_decomposition b then
          (n, __MODULE__, ST.xml_block "system-out" [] [ "Test passed." ], [])
        else
          ( n,
            __MODULE__,
            ST.xml_block "system-out" [] [ "Test failed." ],
            [ ST.xml_block "failure" attr_decomp [] ] )
      with
      | Place.NOT_PRIME ->
          ( n,
            __MODULE__,
            ST.xml_block "system-out" []
              [
                "Test passed. Place graph not decomposable into prime \
                 components.";
              ],
            [] )
      | _ ->
          ( n,
            __MODULE__,
            ST.xml_block "system-out" [] [ ST.error_msg ],
            [
              ST.xml_block "error" ST.attr_err [ Printexc.get_backtrace () ];
            ] ))

(* Args: PATH OUT-PATH FNAME *)
let () =
  Printexc.record_backtrace true;
  let bg_strings =
    IO.parse_all Sys.argv.(1) (fun x ->
        Filename.check_suffix x ".big"
        && (Filename.chop_extension x).[0] = 'T')
  in
  let bgs =
    List.map (fun (n, s) -> (n, Big.of_string s)) bg_strings
    |> List.filter (fun (_, b) -> b.Big.p.Place.s = 0)
  in
  let testcases = do_tests bgs in
  print_endline "OK";
  IO.mkdir Sys.argv.(2);
  ST.(
    write_xml (testsuite "test_decomp" testcases) Sys.argv.(2) Sys.argv.(3));
  print_endline "Done!";
  exit 0
