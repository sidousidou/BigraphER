(* Tests for various bigraph manipulation functions *)
open Bigraph
module IO = CI_utils.Io
module S = Solver.Make_SAT (Solver.MS)

let test_prime_decomposition b =
  let comps = List.map fst (Place.prime_components b.Big.p) in
  Big.(S.equal b { n = b.n; p = Place.tens_of_list comps; l = b.l })

let attr_decomp =
  [ ("type", "ASSERT_DECOMP"); ("message", "Bigraphs are not equal") ]

let do_tests =
  List.iter (fun (n, b) ->
      try
        if test_prime_decomposition b then ()
        else Printf.printf "Test %s failed.\n" n
      with
      | Place.NOT_PRIME ->
          Printf.printf
            "Test %s passed. Place graph not decomposable into prime \
             components.\n"
            n
      | _ ->
          Printf.printf "Test %s error: %s\n" n (Printexc.get_backtrace ()))

(* Args: PATH OUT-PATH FNAME *)
let () =
  Printexc.record_backtrace true;
  let bg_strings =
    IO.parse_all Sys.argv.(1) (fun x ->
        Filename.check_suffix x ".big"
        && (Filename.chop_extension x).[0] = 'T')
  in
  List.map (fun (n, s) -> (n, Big.of_string s)) bg_strings
  |> List.filter (fun (_, b) -> b.Big.p.Place.s = 0)
  |> do_tests;
  exit 0
