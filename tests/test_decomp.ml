(* Tests for various bigraph manipulation functions *)
open Printf
open Big
open Export
open Utils
  
(* parse a .big file *)
let parse path =
  let file = open_in path in
  let rec read_lines out =
    try read_lines ((input_line file) :: out)
    with 
    | End_of_file -> (close_in file; List.rev out)
  in read_lines []
  
(* parse all the bigraphs in one dir with file name "T*.big" *)
let parse_all dir =
  let files = Array.to_list (Sys.readdir dir) in
  List.map (fun x -> 
      (Filename.chop_extension x, parse (Filename.concat dir x))
    ) (List.filter (fun x -> 
      (Filename.check_suffix x ".big") && 
      ((Filename.chop_extension x).[0] = 'T')
    ) files)
    
let test_prime_decomposition b =
  let comps  = List.map fst (Place.prime_components b.p) in
  equal b { n = b.n;
            p = Place.tens_of_list comps;
            l = b.l;
          }

let do_tests l = 
  let c =
    List.fold_left (fun acc (n, b) ->
        try
          if test_prime_decomposition b then (
            (* printf "%s\n" *)
            (*   (colorise `bold (colorise `green (sprintf "Test %s passed." n))); *)
            acc + 1
          ) else (
            printf "%s\n"
              (colorise `bold (colorise `red (sprintf "Test %s failed." n)));
            acc) 
        with
        | Place.NOT_PRIME -> ( 
            (* printf "%s\n" *)
            (*   (colorise `bold  *)
            (*      (colorise `green  *)
            (*         (sprintf "Test %s passed. Place graph not decomposable into prime components." n))); *)
            acc + 1)
        | e -> (
            printf "%s\n"
              (colorise `bold (colorise `red (sprintf "Test %s failed.\n%s\n" n (Printexc.to_string e))));
            acc)
      ) 0 l 
  and n = (List.length l) in
  if c = n then
    printf "%s\n"
      (colorise `bold (colorise `green (sprintf "%d/%d tests passed." c n)))
  else
    printf "%s\n"
      (colorise `bold (colorise `red (sprintf "%d/%d tests passed." c n)))

(* Args: PATH [INDEX] *)  
let () =
  let bg_strings = parse_all Sys.argv.(1) in
  let bgs = 
    List.filter (fun (_, b) -> 
        b.p.Place.s = 0
      ) (List.map (fun (n, s) -> (n, Big.parse s)) bg_strings) in
  try 
    do_tests [List.nth bgs (int_of_string Sys.argv.(3))]
  with
  | _ -> do_tests bgs
  

