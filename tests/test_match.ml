(* Tests for the matching engine *)
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
  
(* parse all the bigraphs in one dir *)
let parse_all dir =
  let files = Array.to_list (Sys.readdir dir)
  in
    List.map (fun x -> 
      ((Filename.chop_extension x), (parse (Filename.concat dir x)))
    ) (List.filter (fun x -> 
      Filename.check_suffix x ".big") files)
  
type test =
  { target : Big.bg; pattern : Big.bg;
    exp_res : (Base.Iso.t * Base.Iso.t) list;
    mutable res : (Base.Iso.t * Base.Iso.t) list
  }

let sort_res =
  List.fast_sort
    (fun (iv0, ie0) (iv1, ie1) ->
      let x = Base.Iso.compare iv0 iv1
      in match x with 
      | 0 -> Base.Iso.compare ie0 ie1 
      | _ -> x)
  
let print_res res =
  sprintf "{\n%s\n}\n"
    (String.concat "\n"
       (List.map
          (fun (i, j) ->
             sprintf "%s -- %s" (Base.Iso.to_string i) (Base.Iso.to_string j))
          (sort_res res)))
  
let check_res res exp_res =
  if (List.length res) <> (List.length exp_res)
  then false
  else
    List.for_all
      (fun ((i0, j0), (i1, j1)) ->
         (Base.Iso.equal i0 i1) && (Base.Iso.equal j0 j1)
      ) (List.combine (sort_res res) (sort_res exp_res))

let print_test (c, n) =
  if c = n then
    printf "%s\n"
      (colorise `bold (colorise `green (sprintf "%d/%d tests passed." c n)))
  else
    printf "%s\n"
      (colorise `bold (colorise `red (sprintf "%d/%d tests passed." c n)))

let do_tests ts =
  let (count, _) =
    List.fold_left (fun (count, i) t ->
        try
          (t.res <- List.map (fun (a, b, _) ->
               (a, b)
             ) (occurrences t.target t.pattern);
           if check_res t.res t.exp_res then
             ((count + 1), (i + 1))
           else (
             printf "%s\n"
               (colorise `red (sprintf "Test %2d failed." (i + 1)));
             (count, (i + 1))
           )
          )
        with
        | NODE_FREE -> (* tests 23 and 16 are special cases *)
          if (i = 15) || (i = 22) then
            ((count + 1), (i + 1))
          else (
            printf "%s\n"
              (colorise `red (sprintf "Test %2d failed." (i + 1)));
            (count, (i + 1))
          )
        | e -> (
            printf "%s"
              (colorise `red (sprintf "Test %2d failed: " (i + 1)));
            printf "%s\n" (Printexc.to_string e);
            (count, (i + 1))
          )
      ) (0, 0) ts
  and n = List.length ts in
  print_test (count, n)

let do_equality_tests l ts =
  let count0 =
    List.fold_left (fun x (n, b) ->
        try
          if Big.equal b b then
            x + 1
          else (
            printf "%s\n"
              (colorise `red (sprintf "Test %3s=%3s failed.\n" n n));
            x
          )
        with
        | e -> (
            printf "%s" (colorise `red (sprintf "Test %3s=%3s failed: " n n));
            printf "%s\n" (Printexc.to_string e);
            x
          )
      ) 0 (List.sort (fun (x, _) (y, _) -> String.compare x y) l)
  and count1 =
    snd (
      List.fold_left (fun (i, x) t ->
          try
            if (equal t.target t.pattern) && (i <> 27) then (
              printf "%s\n" (colorise `red (sprintf "Test %2d failed." i));
              ((i + 1), x)
            )
            else
              ((i + 1), (x + 1))
          with
          | e -> (
              printf "%s" (colorise `red (sprintf "Test %2d failed: " i));
              printf "%s\n" (Printexc.to_string e);
              ((i + 1), x)
            )
        ) (1, 0) ts
    )
  and n = (List.length l) + (List.length ts) in
  print_test ((count0 + count1), n)
  
let tests bgs = (* TEST 1 *)
  [ {
      target = List.assoc "T1" bgs;
      pattern = List.assoc "P1" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 0); (1, 2) ]), (Base.Iso.of_list []));
          ((Base.Iso.of_list [ (0, 0); (1, 3) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 2 *)
    {
      target = List.assoc "T2" bgs;
      pattern = List.assoc "P3" bgs;
      exp_res = [];
      res = [];
    }; (* TEST 3 *)
    {
      target = List.assoc "T2" bgs;
      pattern = List.assoc "P2" bgs;
      exp_res = [ ((Base.Iso.of_list [ (0, 0) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 4 *)
    {
      target = List.assoc "T3" bgs;
      pattern = List.assoc "P4" bgs;
      exp_res = [];
      res = [];
    }; (* TEST 5 *)
    {
      target = List.assoc "T3" bgs;
      pattern = List.assoc "P5" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 1); (1, 0) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 6 *)
    {
      target = List.assoc "T4" bgs;
      pattern = List.assoc "P6" bgs;
      exp_res = [];
      res = [];
    }; (* TEST 7 *)
    {
      target = List.assoc "T5" bgs;
      pattern = List.assoc "P7" bgs;
      exp_res = [];
      res = [];
    }; (* TEST 8 *)
    {
      target = List.assoc "T5" bgs;
      pattern = List.assoc "P8" bgs;
      exp_res = [];
      res = [];
    }; (* TEST 9 *)
    {
      target = List.assoc "T5" bgs;
      pattern = List.assoc "P9" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 0); (1, 1) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 10 *)
    {
      target = List.assoc "T6" bgs;
      pattern = List.assoc "P10" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 0); (1, 1) ]),
           (Base.Iso.of_list [ (0, 0) ])) ];
      res = [];
    }; (* TEST 11 *)
    {
      target = List.assoc "T7" bgs;
      pattern = List.assoc "P11" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 0); (1, 1) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 12 *)
    {
      target = List.assoc "T8" bgs;
      pattern = List.assoc "P12" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 0); (1, 1) ]),
           (Base.Iso.of_list [ (0, 0) ])) ];
      res = [];
    }; (* TEST 13 *)
    {
      target = List.assoc "T9" bgs;
      pattern = List.assoc "P13" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 0); (1, 1) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 14 *)
    {
      target = List.assoc "T9" bgs;
      pattern = List.assoc "P14" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 0); (1, 1) ]),
           (Base.Iso.of_list [ (0, 1) ])) ];
      res = [];
    }; (* TEST 15 *)
    {
      target = List.assoc "T9" bgs;
      pattern = List.assoc "P15" bgs;
      exp_res = [];
      res = [];
    }; (* TEST 16 *)
    {
      target = List.assoc "T10" bgs;
      pattern = List.assoc "P16" bgs;
      exp_res = [];(* infinite matches *)
      
      res = [];
    }; (* TEST 17 *)
    {
      target = List.assoc "T11" bgs;
      pattern = List.assoc "P17" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 0) ]), (Base.Iso.of_list []));
          ((Base.Iso.of_list [ (0, 1) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 18 *)
    {
      target = List.assoc "T11" bgs;
      pattern = List.assoc "P18" bgs;
      exp_res = [];
      res = [];
    }; (* TEST 19 *)
    {
      target = List.assoc "T12" bgs;
      pattern = List.assoc "P19" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 2) ]), (Base.Iso.of_list []));
          ((Base.Iso.of_list [ (0, 1) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 20 *)
    {
      target = List.assoc "T12" bgs;
      pattern = List.assoc "P20" bgs;
      exp_res = [];
      res = [];
    }; (* TEST 21 *)
    {
      target = List.assoc "T12" bgs;
      pattern = List.assoc "P21" bgs;
      exp_res = [ ((Base.Iso.of_list [ (0, 0) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 22 *)
    {
      target = List.assoc "T12" bgs;
      pattern = List.assoc "P22" bgs;
      exp_res = [];
      res = [];
    }; (* TEST 23 *)
    {
      target = List.assoc "T13" bgs;
      pattern = List.assoc "P23" bgs;
      exp_res = [];(* infinite matches *)
      
      res = [];
    }; (* TEST 24 *)
    {
      target = List.assoc "T13" bgs;
      pattern = List.assoc "P24" bgs;
      exp_res = [ ((Base.Iso.of_list [ (0, 0) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 25 *)
    {
      target = List.assoc "T14" bgs;
      pattern = List.assoc "P25" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 2); (1, 3) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 26 *)
    {
      target = List.assoc "T15" bgs;
      pattern = List.assoc "P26" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 2); (1, 3); (2, 5); (3, 4); (4, 6) ]),
           (Base.Iso.of_list []));
          ((Base.Iso.of_list [ (0, 2); (1, 3); (2, 4); (3, 5); (4, 6) ]),
           (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 27 *)
    {
      target = List.assoc "T16" bgs;
      pattern = List.assoc "T16" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 0); (1, 1); (2, 2); (3, 3); (4, 4) ]),
           (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 28 *)
    {
      target = List.assoc "T17" bgs;
      pattern = List.assoc "P27" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 1); (1, 2) ]),
           (Base.Iso.of_list [ (0, 0) ])) ];
      res = [];
    }; (* TEST 29 *)
    {
      target = List.assoc "T18" bgs;
      pattern = List.assoc "P27" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 1); (1, 5) ]),
           (Base.Iso.of_list [ (0, 0) ])) ];(* or (0,1)? *)
      
      res = [];
    }; (* TEST 30 *) (* closed edges have to be iso *)
    {
      target = List.assoc "T19" bgs;
      pattern = List.assoc "P27" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 4); (1, 8) ]),
           (Base.Iso.of_list [ (0, 2) ])) ];
      res = [];
    }; (* TEST 31 *)
    {
      target = List.assoc "T19" bgs;
      pattern = List.assoc "P28" bgs;(* no edges *)
      
      exp_res =
        [ ((Base.Iso.of_list [ (0, 0) ]), (Base.Iso.of_list []));
          ((Base.Iso.of_list [ (0, 4) ]), (Base.Iso.of_list []));
          ((Base.Iso.of_list [ (0, 5) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 32 *)
    {
      target = List.assoc "T20" bgs;
      pattern = List.assoc "P29" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 2); (1, 3) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 33 *)
    {
      target = List.assoc "T21" bgs;
      pattern = List.assoc "P30" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 1); (1, 2) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (*vvvvvvvvvvvvvvvvvvvv   EXAMPLES from the THESIS    vvvvvvvvvvvvvvvvv*)
    (* TEST 34 *)
    {
      target = List.assoc "T22" bgs;
      pattern = List.assoc "P31" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 3); (1, 6) ]), (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 35 *)
    {
      target = List.assoc "T22" bgs;
      pattern = List.assoc "P32" bgs;
      exp_res =
        [ ((Base.Iso.of_list [ (0, 0); (1, 1); (2, 4) ]),
           (Base.Iso.of_list []));
          ((Base.Iso.of_list [ (0, 0); (1, 2); (2, 5) ]),
           (Base.Iso.of_list [])) ];
      res = [];
    }; (* TEST 36 *)
    {
      target = List.assoc "T22" bgs;
      pattern = List.assoc "P33" bgs;
      exp_res = [];
      res = [];
    }; (* TEST 37 *)
    {
      target = List.assoc "T23" bgs;
      pattern = List.assoc "P34" bgs;
      exp_res = [];
      res = [];
    }; (* TEST 38 *)
    {
      target = List.assoc "T26" bgs;
      pattern = List.assoc "P37" bgs;
      exp_res = [];
      res = [];
    } ]
  
let () =
  let bg_strings = parse_all Sys.argv.(2) in
  let bgs =
    List.map (fun (n, ls) -> (n, (Big.parse ls))) bg_strings in
  let ts = tests bgs in
  match Sys.argv.(1) with
  | "match" -> do_tests ts
  | "equality" -> do_equality_tests bgs ts
  | _ -> exit 1
  

