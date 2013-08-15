(* Tests for the matching engine *)

open Printf
open Big
open Export

(* parse a .big file *)
let parse path = 
  let file = open_in path in
  let rec read_lines out = 
    try
      read_lines ((input_line file) :: out)
    with
      End_of_file ->
        (close_in file;
         List.rev out) in
  read_lines []

(* parse all the bigraphs in one dir *)
let parse_all dir = 
  let files =  Array.to_list (Sys.readdir dir) in
  List.map (fun x ->
      (Filename.chop_extension x, parse (Filename.concat dir x)))
    (List.filter (fun x ->
      Filename.check_suffix x ".big") files)

(* ALGORITHM:
   1. select an element x
   2. filter all the elements equal to x
   3. add them to out with index
   4. remove them from l
   5. repeat on new l *)
 
(* build a bigraph *)
let build_big l =
  let lines =
    Array.of_list l in
  let tokens s =
    Str.split (Str.regexp_string " ") s in
  (* line 0: roots, nodes, sites, links *)
  (* line 1: node 0, node 1, .... *)
  (* lines (roots + nodes + 2) - EOF: node, node , node , t/f *)  
  let parse_line s =
    Array.of_list (tokens s)
  (* str to bool array *)   
  and bools_of_line s = 
    let a = Array.make (String.length s) false in
    for i = 0 to (String.length s) - 1 do
      match s.[i] with
      | '1' -> a.(i) <- true
      | _ -> ()
    done;
    a in
  (* build bmatrix *)    
  let build_matrix r0 rn =
    let m = Matrix.make (rn + 1 - r0) (String.length lines.(r0)) in
    for i = 0 to rn - r0 do
      let bools = bools_of_line lines.(r0 + i) in
      for j = 0 to (String.length lines.(r0)) - 1 do
	if bools.(j) then m.{i,j} <- 1 else m.{i,j} <- 0
      done;
    done;
    m; 
  and build_edge s n h =
    let a  = parse_line s in 
    { Link.p =
	List.fold_left (fun acc x ->
	  try
	    let j = Hashtbl.find h x in
	    Hashtbl.add h x (j + 1);
	    Base.Ports.add (x, j) acc
	  with
	    | Not_found -> 
	      begin
		Hashtbl.add h x 1;
		Base.Ports.add (x, 0) acc
	      end)
	  Base.Ports.empty
	  (Array.to_list (Array.map (fun x ->
	    (int_of_string x) - 1) (Array.sub a 0 ((Array.length a) - 1))));
      Link.i = Link.Face.empty;
      Link.o = 
	begin
	  match a.((Array.length a) - 1) with 
	    | "t" -> Link.parse_face [sprintf "n%d" n]
	    | _ -> Link.Face.empty
	end;
    } in 
  let l0 =
    Array.map int_of_string (parse_line lines.(0)) 
  and l1 = (* check if it works with no nodes *)
    parse_line lines.(1) in 
  let build_place =
    { Place.r = l0.(0);
      Place.n = l0.(1);
      Place.s = l0.(2);
      Place.m = build_matrix 2 (l0.(0) + l0.(1) + 1);
    }
  and (build_link, h) =
    let h = Hashtbl.create l0.(1) in
    (fst (Array.fold_left (fun (acc, i) l -> 
      (Link.Lg.add (build_edge l i h) acc), i + 1) (Link.Lg.empty, 0)
	    (Array.sub lines (l0.(0) + l0.(1) + 2) l0.(3))), h) in
  let build_names =
    Array.fold_left (fun acc x ->
      Base.Nodes.add x acc) Base.Nodes.empty
      (Array.mapi (fun i c ->
	(i, Base.Ctrl (c,
		       try
			 Hashtbl.find h i
		       with
			 | Not_found -> 0))) l1) in
  { Big.p = build_place;
    Big.l = build_link;
    Big.n = build_names;
  }

type test =
    { target : Big.bg;
      pattern : Big.bg;
      exp_res : (Base.Iso.t * Base.Iso.t) list;
      mutable res : (Base.Iso.t * Base.Iso.t) list;
    }
      
let sort_res = 
  List.fast_sort (fun (iv0, ie0) (iv1, ie1) ->
    let x = Base.Iso.compare iv0 iv1 in
    match x with
      |	0 -> Base.Iso.compare ie0 ie1
      | _ -> x
  ) 

let print_res res =
  let string_of_iso i = 
    sprintf "[%s]" (String.concat " " (List.map (fun (x,y) ->
      sprintf "(%d,%d)" x y) (Base.Iso.elements i))) in
  sprintf "{\n%s\n}\n" (String.concat "\n" (List.map (fun (i,j) ->
    sprintf "%s --  %s" (string_of_iso i) (string_of_iso j)) (sort_res res)))

let check_res res exp_res  = 
  if (List.length res) != (List.length exp_res) then
    false
  else
    List.for_all (fun ((i0,j0), (i1,j1)) ->
      (Base.Iso.equal i0 i1) & (Base.Iso.equal j0 j1))
      (List.combine (sort_res res) (sort_res exp_res))

let beep () = 
  printf "\007"
      
let do_tests ts = 
  let count = ref 0 in
  flush_all ();
  printf "Starting tests ...\n";
  let t0 = Unix.gettimeofday () in
  Array.iteri (fun i t -> 
    try 
      t.res <- occurrences t.target t.pattern;
      if check_res t.res t.exp_res then
	begin
	  count := !count + 1;
	  (*printf "Test %d passed.\nResult :\n%s\n" (i + 1) (print_res t.res)*)
	  printf "Test %d passed.\n" (i + 1)
	end
      else
	begin
	  beep ();
	  printf "Test %d failed.\nExpected result :\n%s\nResult :\n%s\n"
	    (i + 1) (print_res t.exp_res) (print_res t.res);
	  printf "Target:\n%s\nPattern:\n%s\n"
	    (string_of_bg t.target) (string_of_bg t.pattern)
	end
    with
      | NODE_FREE -> (* tests 23 and 16 are special cases *) 
	(if (i = 15 || i = 22) then
	    (count := !count + 1;
	     printf "Test %d passed.\n" (i + 1))
	 else
	    (beep ();
	     printf "Test %d failed.\nExpected result :\nInfinite matches\nResult :\n%s\n"
	       (i + 1) (print_res t.res)))
      | e -> print_endline (Printexc.to_string e)) (Array.of_list ts);
  printf "Finished in %f seconds.\n" ((Unix.gettimeofday ()) -. t0);
  printf "%d/%d tests passed.\n" !count (List.length ts)

let do_equality_tests l ts = 
  flush_all ();
  printf "Starting equality tests ...\n";
  let t0 = Unix.gettimeofday () in
  try
    let count = 
      (List.fold_left (fun x (n, b) ->
	if Big.equal b b then (printf "Test %s=%s passed.\n" n n; x + 1)
	else (printf "Test %s=%s failed.\n" n n; x)) 0 (List.sort (fun (x, _) (y, _) ->
	  String.compare x y) l)) +
	(snd (List.fold_left (fun (i, x) t ->
	  if (equal t.target t.pattern) && (i <> 27) then (printf "Test %d failed.\n" i; (i + 1, x))
	  else (printf "Test %d passed.\n" i; (i + 1, x + 1))) (1, 0) ts)) in
    printf "Finished in %f seconds.\n" ((Unix.gettimeofday ()) -. t0);
    printf "%d/%d tests passed.\n" count ((List.length l) + List.length ts)
  with
    | _ -> printf "Internal error\n"
    
(* The first argument is the path of the directory containing the tests. The
   second optional argument is the path for the svg output. *)
let _ =
  check_graphviz ();
  let args = Sys.argv in
  assert (Array.length args = 2 || Array.length args = 3);
  printf "%s %s\n" args.(0) args.(1);
  let bg_strings = parse_all args.(1) in
  let bgs = 
    List.map (fun (n, ls) ->
      printf "building %s\n" n;
      (n, build_big ls)) bg_strings in
  if Array.length args = 3 then
    List.iter (fun (n, b) -> 
      write_big b n (Filename.concat args.(1) ("svg" ^ Filename.dir_sep))
	true) bgs;
  let tests =
    try
      [ (* TEST 1 *)
	{ target = List.assoc "T1" bgs;
	  pattern = List.assoc "P1" bgs;
	  exp_res = [ (Base.of_list [(0,0); (1,2)], Base.of_list  []);
		      (Base.of_list [(0,0); (1,3)], Base.of_list  []);
		    ];
	  res = [ ];
	};
	(* TEST 2 *)
	{ target = List.assoc "T2" bgs;
	  pattern = List.assoc "P3" bgs;
	  exp_res = [ ];
	  res = [ ];
	};
	(* TEST 3 *)
	{ target = List.assoc "T2" bgs;
	  pattern = List.assoc "P2" bgs;
	  exp_res = [ (Base.of_list [(0,0)], Base.of_list  []) ];
	  res = [ ];
	};
	(* TEST 4 *)
	{ target = List.assoc "T3" bgs;
	  pattern = List.assoc "P4" bgs;
	  exp_res = [ ];
	  res = [ ];
	};
	(* TEST 5 *)
	{ target = List.assoc "T3" bgs;
	  pattern = List.assoc "P5" bgs;
	  exp_res = [ (Base.of_list [(0,1); (1,0)], 
		       Base.of_list  [ ]) ];
	  res = [ ];
	};
	(* TEST 6 *) 
	{ target = List.assoc "T4" bgs;
	  pattern = List.assoc "P6" bgs;
	  exp_res = [ ];
	  res = [ ];
	};
	(* TEST 7 *)
	{ target = List.assoc "T5" bgs;
	  pattern = List.assoc "P7" bgs;
	  exp_res = [ ];
	  res = [ ];
	};
	(* TEST 8 *) 
	{ target = List.assoc "T5" bgs;
	  pattern = List.assoc "P8" bgs;
	  exp_res = [ ];
	  res = [ ]; 
	};
	(* TEST 9 *)
	{ target = List.assoc "T5" bgs;
	  pattern = List.assoc "P9" bgs;
	  exp_res = [ (Base.of_list [(0,0); (1,1)], Base.of_list  [ ]) ];
	  res = [ ];
	};
	(* TEST 10 *)
	{ target = List.assoc "T6" bgs;
	  pattern = List.assoc "P10" bgs;
	  exp_res = [ (Base.of_list [(0,0); (1,1)], 
		       Base.of_list  [(0,0)]) ];
	  res = [ ];
	};
	(* TEST 11 *)
	{ target = List.assoc "T7" bgs;
	  pattern = List.assoc "P11" bgs;
	  exp_res = [ (Base.of_list [(0,0); (1,1)], Base.of_list [ ]) ];
	  res = [ ];
	};
	(* TEST 12 *)
	{ target = List.assoc "T8" bgs;
	  pattern = List.assoc "P12" bgs;
	  exp_res = [ (Base.of_list [(0,0); (1,1)], Base.of_list  [(0,0)]) ];
	  res = [ ];
	};
	(* TEST 13 *)
	{ target = List.assoc "T9" bgs;
	  pattern = List.assoc "P13" bgs;
	  exp_res = [ (Base.of_list [(0,0); (1,1)], 
		       Base.of_list  []) ];
	  res = [ ];
	};
	(* TEST 14 *)
	{ target = List.assoc "T9" bgs;
	  pattern = List.assoc "P14" bgs;
	  exp_res = [ (Base.of_list [(0,0); (1,1)], 
		       Base.of_list  [(0,1)]) ];
	  res = [ ];
	};
	(* TEST 15 *)
	{ target = List.assoc "T9" bgs;
	  pattern = List.assoc "P15" bgs;
	  exp_res = [ ];
	  res = [ ];
	};
	(* TEST 16 *)
	{ target = List.assoc "T10" bgs;
	  pattern = List.assoc "P16" bgs;
	  exp_res = [ ]; (* infinite matches *)
	  res = [ ];
	};
	(* TEST 17 *)     
	{ target = List.assoc "T11" bgs;
	  pattern = List.assoc "P17" bgs;
	  exp_res = [ (Base.of_list [(0,0)], Base.of_list [ ]);
		      (Base.of_list [(0,1)], Base.of_list [ ]) ];
	  res = [ ];
	};
	(* TEST 18 *)
	{ target = List.assoc "T11" bgs;
	  pattern = List.assoc "P18" bgs;
	  exp_res = [ ];
	  res = [ ];
	};
	(* TEST 19 *)
	{ target = List.assoc "T12" bgs;
	  pattern = List.assoc "P19" bgs;
	  exp_res = [ (Base.of_list [(0,2)], Base.of_list [ ]);
		      (Base.of_list [(0,1)], Base.of_list [ ]) ];
	  res = [ ];
	};
	(* TEST 20 *)
	{ target = List.assoc "T12" bgs;
	  pattern = List.assoc "P20" bgs;
	  exp_res = [ ];
	  res = [ ];
	};
	(* TEST 21 *)
	{ target = List.assoc "T12" bgs;
	  pattern = List.assoc "P21" bgs;
	  exp_res = [ (Base.of_list [(0,0)], Base.of_list [ ]) ];
	  res = [ ];
	};
	(* TEST 22 *)
	{ target = List.assoc "T12" bgs;
	  pattern = List.assoc "P22" bgs;
	  exp_res = [ ];
	  res = [ ];
	};
	(* TEST 23 *)
	{ target = List.assoc "T13" bgs;
	  pattern = List.assoc "P23" bgs;
	  exp_res = [ ]; (* infinite matches *)
	  res = [ ];
	};
	(* TEST 24 *)
	{ target = List.assoc "T13" bgs;
	  pattern = List.assoc "P24" bgs;
	  exp_res = [ (Base.of_list [(0,0)], Base.of_list [ ]) ];
	  res = [ ];
	};
	(* TEST 25 *)
	{ target = List.assoc "T14" bgs;
	  pattern = List.assoc "P25" bgs;
	  exp_res = [ (Base.of_list [(0,2); (1,3)], Base.of_list [ ]) ];
	  res = [ ];
	};
	(* TEST 26 *)
	{ target = List.assoc "T15" bgs;
	  pattern = List.assoc "P26" bgs;
	  exp_res = [ (Base.of_list [(0,2); (1,3); (2,5); (3,4); (4,6)], 
		       Base.of_list [ ]);
		      (Base.of_list [(0,2); (1,3); (2,4); (3,5); (4,6)], 
		       Base.of_list [ ]);
		    ];
	  res = [ ];
	};
	(* TEST 27 *) 
	{ target = List.assoc "T16" bgs;
	  pattern = List.assoc "T16" bgs;
	  exp_res = [ (Base.of_list [(0,0); (1,1); (2,2); (3,3); (4,4) ], 
		       Base.of_list [ ]) ];
	  res = [ ];
	};
	(* TEST 28 *)
	{ target = List.assoc "T17" bgs;
	  pattern = List.assoc "P27" bgs;
	  exp_res = [ (Base.of_list [(0,1); (1,2) ], 
		       Base.of_list [ (0,0) ]) ];
	  res = [ ];
	};
	(* TEST 29 *)
	{ target = List.assoc "T18" bgs;
	  pattern = List.assoc "P27" bgs;
	  exp_res = [ (Base.of_list [(0,1); (1,5) ], 
		       Base.of_list [ (0,0) ]) ]; (* or (0,1)? *)
	  res = [ ];
	};
	(* TEST 30 *)(* closed edges have to be iso *)
	{ target = List.assoc "T19" bgs;
	  pattern = List.assoc "P27" bgs;
	  exp_res = [ (Base.of_list [(0,4); (1,8) ], 
		       Base.of_list [ (0,2) ]) ];
	  res = [ ];
	};
	(* TEST 31 *)
	{ target = List.assoc "T19" bgs;
	  pattern = List.assoc "P28" bgs; (* no edges *)
	  exp_res = [ (Base.of_list [(0,0);], Base.of_list [ ]);
		      (Base.of_list [(0,4);], Base.of_list [ ]);
		      (Base.of_list [(0,5);], Base.of_list [ ]);
		    ];
	  res = [ ];
	};
	(* TEST 32 *)
	{ target = List.assoc "T20" bgs;
	  pattern = List.assoc "P29" bgs;
	  exp_res = [ (Base.of_list [(0,2); (1,3)], Base.of_list [ ]) ];
	  res = [ ];
	};
	(* TEST 33 *)
	{ target = List.assoc "T21" bgs;
	  pattern = List.assoc "P30" bgs;
	  exp_res = [ (Base.of_list [(0,1); (1,2)], Base.of_list [ ]) ];
	  res = [ ];
	};
	(*vvvvvvvvvvvvvvvvvvvv   EXAMPLES from the THESIS    vvvvvvvvvvvvvvvvv*)
	(* TEST 34 *)
	{ target = List.assoc "T22" bgs;
	  pattern = List.assoc "P31" bgs;
	  exp_res = [ (Base.of_list [(0,3); (1,6)], Base.of_list [ ]) ];
	  res = [ ];
	};
	(* TEST 35 *)
	{ target = List.assoc "T22" bgs;
	  pattern = List.assoc "P32" bgs;
	  exp_res = [ (Base.of_list [(0,0); (1,1); (2,4)], Base.of_list [ ]);
		      (Base.of_list [(0,0); (1,2); (2,5)], Base.of_list [ ]) ];
	  res = [ ];
	};
	(* TEST 36 *)
	{ target = List.assoc "T22" bgs;
	  pattern = List.assoc "P33" bgs;
	  exp_res = [ ];
	  res = [ ];
	};
	(* TEST 37 *)
	{ target = List.assoc "T23" bgs;
	  pattern = List.assoc "P34" bgs;
	  exp_res = [ ];
	  res = [ ];
	};
	(* TEST 38 *)
	{ target = List.assoc "T26" bgs;
	  pattern = List.assoc "P37" bgs;
	  exp_res = [ ];
	  res = [ ];
	};
      ]
    with
      | Not_found -> failwith ("Error loading tests.\n") in
  do_tests tests;
  do_equality_tests bgs tests;
  Gc.full_major ();
  wait_before_exit true
(* can compile with -noassert *)
