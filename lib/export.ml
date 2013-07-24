open Printf
open Big
  
exception SVG_MISSING of string
  
let end_of_file =
  let os = Sys.os_type
  in match os with | "Unix" -> '\004' | "Win32" -> '\026' | _ -> '\026'
  
(* Not sure here! *)
let end_of_line =
  let os = Sys.os_type
  in match os with | "Unix" -> "\n" | "Win32" -> "\r\n" | _ -> "\r\n"

(* convert a relative path to an absolute path *)
(* FIX symbolic links *)
let abs_path path = 
  if Filename.is_relative path
  then
    (let current = Sys.getcwd ()
     and home = Sys.getenv "HOME"
     and tokens = Str.split (Str.regexp Filename.dir_sep) path
     and par path = Filename.dirname path  
     in let rec retrieve tokens curr =
      match tokens with
       | [] -> curr ^ (Filename.dir_sep)
       | x::xs -> (if x = "~" then
        retrieve xs home
       else if x = Filename.current_dir_name then
        retrieve xs curr
       else if x = Filename.parent_dir_name then
        retrieve xs (par curr)
       else retrieve xs (Filename.concat curr x))
     in retrieve tokens current)
  else path

(* Convert path_b to a path relative to path_a *)
let relative_to path_a path_b =
 let a = Str.split (Str.regexp Filename.dir_sep) (abs_path path_a)
 and b = Str.split (Str.regexp Filename.dir_sep) (abs_path path_b)
 in let rec aux c p = 
  match (c, p) with
    | ([], _) -> (Filename.current_dir_name)::p
    | (_, []) -> List.map (fun x -> Filename.parent_dir_name) c
    | (x::xs, y::ys) -> if x=y then aux xs ys
        else (List.map (fun x -> Filename.parent_dir_name) xs)@ys
 in List.fold_left (fun p f -> Filename.concat p f) "" (aux a b)

(* Write a string in dot format to an svg file *)
let write_svg s name path verb =
  let (dot_in, bigmc_out) = Unix.pipe ()
  and svg_file = Unix.openfile (sprintf "%s%s.svg" path name)
    [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY ] 0o777 in
  Unix.set_close_on_exec dot_in;
  Unix.set_close_on_exec svg_file;
  if verb then
    printf "Writing %s%s.svg\n" path name
  else (); 
  let b_w = Unix.write bigmc_out s 0 (String.length s) in
  flush_all ();
  Unix.close bigmc_out;
  let pid = Unix.create_process "dot" [| "dot"; "-Tsvg" |]
    dot_in svg_file Unix.stderr in
  match Unix.waitpid [ Unix.WNOHANG ] pid with
     | (_, Unix.WSTOPPED _) -> failwith "Error: dot terminated unexpectedly\n"
     | _ -> 
       if verb then
	 print_endline (sprintf "%d bytes written" b_w)  
       else ()
  
(* Write a bigraph to svg *)
let dot_out b n path verb = write_svg (get_dot b n) n (abs_path path) verb
  
(* Auxiliary - list iterator with index. String output *)
let rec scan l i f =
  match l with | [] -> "" | x :: xs -> (f x i) ^ (scan xs (i + 1) f)
  
(************************** Write a trace to a file ***************************)
let trace_s_out tr path verb =
  let oc = open_out path
  in
    (if verb then printf "Writing %s\n" path else ();
     let n = List.length tr
     in
       (fprintf oc "%d %d\n" n (n - 1);
        fprintf oc "%s%c"
          (scan (List.tl tr) 0
             (fun (_, rho) i -> sprintf "%d %d %f\n" i (i + 1) rho))
          end_of_file;
        close_out oc))
  
let trace_out tr path verb =
  let oc = open_out path
  in
    (if verb then printf "Writing %s\n" path else ();
     let n = List.length tr
     in
       (fprintf oc "%d %d\n" n (n - 1);
        fprintf oc "%s%c"
          (scan (List.tl tr) 0 (fun _ i -> sprintf "%d %d\n" i (i + 1)))
          end_of_file;
        close_out oc))
  
(************************** Write a trace to dot ******************************)
let string_of_time () =
  let t = Unix.localtime (Unix.time ())
  in
    sprintf "%02d-%02d-%4d-%02d-%02d-%02d" t.Unix.tm_mday (t.Unix.tm_mon + 1)
      (1900 + t.Unix.tm_year) t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec
  
let trace_s_out_dot tr path flag verb =
  let name = sprintf "trace_%s" (string_of_time ()) in
  let states =
    scan tr 0
      (fun (_, t) i ->
         if flag
         then sprintf "%d [URL=\"./%s.svg\"];\n" i (string_of_float t)
         else sprintf "%d [label=\"%d\", shape=circle];\n" i i)
  and edges =
    scan (List.tl tr) 0
      (fun (_, t) i -> sprintf "%d -> %d [label=\"%f\"];\n" i (i + 1) t) in
  let dot = sprintf "digraph trace {\n%s%s}" states edges
  in write_svg dot name path verb
  
let trace_out_dot tr path flag verb =
  let name = sprintf "trace_%s" (string_of_time ()) in
  let states =
    scan tr 0
      (fun _ i ->
         if flag
         then sprintf "%d [URL=\"./%d.svg\"];\n" i i
         else sprintf "%d [label=\"%d\", shape=circle];\n" i i)
  and edges =
    scan (List.tl tr) 0 (fun _ i -> sprintf "%d -> %d;\n" i (i + 1)) in
  let dot = sprintf "digraph trace {\n%s%s}" states edges
  in write_svg dot name path verb
  
(********************** Write the states of a trace to dot ********************)
let trace_s_dot tr path verb =
  List.iter (fun (b, t) -> dot_out b (string_of_float t) path verb) tr
  
let trace_dot (tr : bg list) path verb =
  let rec loop l i =
    match l with
    | [] -> ()
    | b :: xs -> (dot_out b (string_of_int i) path verb; loop xs (i + 1))
  in loop tr 0
  
(*************************** Write a CTMC to dot ******************************)
let markov_out_dot (s, e) path flag verb =
  let name = sprintf "ctmc_%s" (string_of_time ()) in
  let states =
    String.concat "\n" (List.map (fun (i, _) ->
      if flag
         then sprintf "%d [URL=\"./%d.svg\"];" i i
         else sprintf "%d [label=\"%d\", shape=circle];" i i
      ) s)
  and edges =
    Hashtbl.fold (fun v (u, rho) buff -> 
      sprintf "%s%d -> %d [label=\"%f\"];\n" buff v u rho) e "" in
  let dot = sprintf "digraph ctmc {\n%s\n%s}" states edges
  in write_svg dot name path verb

(********************** Write the states of a CTMC to dot *********************)
let markov_dot states path verb = 
  List.iter (fun (i, b) -> dot_out b (string_of_int i) path verb) states   

(*************************** Write a CTMC to a file ***************************)
let markov_out (s, e) path verb =
  let oc = open_out path
  in
    if verb then printf "Writing %s\n" path else ();
    let n = List.length s
    and m = Hashtbl.length e
    in
      fprintf oc "%d %d\n" n m;
      Hashtbl.iter (fun v (u, rho) -> fprintf oc "%d %d %f\n" v u rho) e;
      fprintf oc "%c" end_of_file;
      close_out oc

(******************** Write a transition system to a file *********************)
let ts_out (s, e) path verb =
  let oc = open_out path
  in
    if verb then printf "Writing %s\n" path else ();
    let n = List.length s
    and m = Hashtbl.length e
    in
      fprintf oc "%d %d\n" n m;
      Hashtbl.iter (fun v u -> fprintf oc "%d %d\n" v u) e;
      fprintf oc "%c" end_of_file;
      close_out oc

(************** Write the states of a transition system to dot ****************)
let ts_dot states path verb = markov_dot states path verb   

(********************* Write a transition system to dot ***********************)
let ts_out_dot (s, e) path flag verb =
  let name = sprintf "ts_%s" (string_of_time ()) in
  let states =
    String.concat "\n" (List.map (fun (i, _) ->
      if flag
         then sprintf "%d [URL=\"./%d.svg\"];" i i
         else sprintf "%d [label=\"%d\", shape=circle];" i i
      ) s)
  and edges =
    Hashtbl.fold (fun v u buff -> 
      sprintf "%s%d -> %d;\n" buff v u) e "" in
  let dot = sprintf "digraph ts {\n%s\n%s}" states edges
  in write_svg dot name path verb

(***************** Write the labelling funtion to a csl file ******************)
let csl_out lab path verb =
  let rec add_mem l (ide : string) (x : int) =
    match l with
      | [] -> [(ide, [x])]
      | (i, xs)::rest ->
          (if i = ide
          then (if List.mem x xs
            then (i, xs)::rest
            else (i, x::xs)::rest)
          else (i, xs)::(add_mem rest ide x))
  in let lab_list = 
    Hashtbl.fold (fun ide x acc ->
      add_mem acc ide x) lab []
  in let oc = open_out path
  in
    if verb then printf "Writing %s\n" path else ();
    List.iter (fun (ide, xs) ->
      fprintf oc "label \"%s\" = %s;\n" ide (
        String.concat " | " (List.map (fun x ->
        sprintf "x = %d" x) xs)
      )) lab_list;
    fprintf oc "%c" end_of_file;  
    close_out oc

(********************** DEBUG write the store to file *************************)
(*let export_dot store path verb =
  Hashtbl.iter
    (fun i v ->
       match v with
       | Env.V_big x -> dot_out x i path verb
       | Env.V_react x ->
           (dot_out (Brs.redex x) (i ^ "_rdx") path verb;
            dot_out (Brs.reactum x) (i ^ "_rtm") path verb)
       | Env.V_sreact x ->
           (dot_out (Brs.sredex x) (i ^ "_rdx") path verb;
            dot_out (Brs.sreactum x) (i ^ "_rtm") path verb))
    store;
  flush stdout  

let export_dot_bilog l path verb =
  List.iter (fun (i, b) -> dot_out b i path verb) l;
  flush stdout 
  
(* Writes an html showing the dynamical evolution on one path *)
let path_out states path f_name =
  let svg_err i =
    Printf.sprintf "Error: File %s%d.svg is missing.\n" path i in
  (* -exportstates *)
  let undef =
    List.filter (fun i -> Sys.file_exists (Printf.sprintf "%s%d.svg" path i))
      states
  in
    match undef with
    | [] ->
        let js =
          String.concat "\n"
            (Array.to_list
               (Array.mapi
                  (fun i s -> Printf.sprintf "states[%d] = \"%d.svg\";" i s)
                  (Array.of_list states)))
        and fst = Printf.sprintf "%d.svg" (List.hd states) in
        let oc = open_out (Filename.concat path f_name)
        in
          (Printf.fprintf oc "<html>\n<head>\n";
           Printf.fprintf oc "<title>System evolution</title>\n";
           Printf.fprintf oc "<script type=\"text/javascript\">\n";
           Printf.fprintf oc "var states = new Array();\n%s" js;
           Printf.fprintf oc "\nvar c = 0;\nvar t;\n";
           Printf.fprintf oc "function timer() {\n";
           Printf.fprintf oc "if (c < %d) {\n" (List.length states);
           Printf.fprintf oc
             "document.getElementById(\"canvas\").innerHTML =\n";
           Printf.fprintf oc
             "'<object id=\"state\" type=\"image/svg+xml\" data=\"' + states[c] + '\" width=100%%/>';\n";
           Printf.fprintf oc
             "c = c + 1;\nt = setTimeout(\"timer()\", 1500);\n";
           Printf.fprintf oc "}\n}\n</script>\n</head>\n";
           Printf.fprintf oc
             "<body onload=\"timer()\">\n<div id=\"canvas\">\n";
           Printf.fprintf oc
             "<object id=\"state\" type=\"image/svg+xml\" data=\"%s\" width=100%% />\n"
             fst;
           Printf.fprintf oc "</div>\n</body>\n</html>";
           Printf.printf "Writing %s.\n" (Filename.concat path f_name);
           output_char oc end_of_file;
           close_out oc)
    | _ -> raise (SVG_MISSING (svg_err (List.hd undef)))
*)  

(* DEBUG *)
(*let _ =
  let parse =
    List.fold_left (fun s f -> Link.Lg.add f s) Link.Lg.empty
  and parse_p =
    List.fold_left (fun s f -> Base.Ports.add f s) Base.Ports.empty
  and parse_n  = 
    List.fold_left (fun s (i,c,a) ->
      Base.Nodes.add (i, Base.Ctrl(c,a)) s) Base.Nodes.empty in
  (* Example from page 82 *)
  let t = {
    p = {Place.r = 1; Place.n = 8; Place.s = 2; Place.m = Matrix.make 9 10};
    l = parse [{Link.o = Link.parse_face ["x"];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(0,0);(3,0);(4,0);(5,0)]};
               {Link.o = Link.parse_face [];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(1,0);(7,0)]};
               {Link.o = Link.parse_face [];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(2,0)]};
               {Link.o = Link.parse_face [];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(6,0)]}];
    n = parse_n [(0, "A", 1); (1, "B", 1); (2, "B", 1);
                 (3, "A", 1); (4, "A", 1); (5, "A", 1);
                 (6, "B", 1); (7, "B", 1)];
    } in
  t.p.Place.m.Matrix.m.(0).(0) <- true;
  t.p.Place.m.Matrix.m.(1).(1) <- true;
  t.p.Place.m.Matrix.m.(1).(2) <- true;
  t.p.Place.m.Matrix.m.(1).(3) <- true;
  t.p.Place.m.Matrix.m.(2).(4) <- true;
  t.p.Place.m.Matrix.m.(3).(5) <- true;
  t.p.Place.m.Matrix.m.(4).(6) <- true;
  t.p.Place.m.Matrix.m.(5).(7) <- true;
  t.p.Place.m.Matrix.m.(6).(7) <- true;
  t.p.Place.m.Matrix.m.(7).(9) <- true;
  t.p.Place.m.Matrix.m.(8).(8) <- true;
  printf "target:\n%s\n" (string_of_bg t);
  printf "get_dot:\n%s\n" (get_dot t "target");
  let p = {
    p = {Place.r = 1; Place.n = 3; Place.s = 2; Place.m = Matrix.make 4 5};
    l = parse [{Link.o = Link.parse_face ["x"];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(0,0)]};
               {Link.o = Link.parse_face ["y"];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(2,0)]};
               {Link.o = Link.parse_face [];
                Link.i = Link.parse_face ["z"];
                Link.p = parse_p [(1,0)]}];
    n = parse_n [(0, "A", 1); (1, "B", 1); (2, "A", 1)];
    } in
  p.p.Place.m.Matrix.m.(0).(0) <- true;
  p.p.Place.m.Matrix.m.(1).(1) <- true;
  p.p.Place.m.Matrix.m.(1).(3) <- true;
  p.p.Place.m.Matrix.m.(2).(2) <- true;
  p.p.Place.m.Matrix.m.(3).(4) <- true;
  printf "pattern:\n%s\n" (string_of_bg p);
  printf "get_dot:\n%s\n" (get_dot p "pattern");
  dot_out t "target" "~/tmp/" true; 
  dot_out p "pattern" "~/tmp/" true; 
  let (c, d, i) =
    Big.decomp t p (Base.of_list [(0,0);(1,1);(2,4)])
      (Base.of_list [(0,3);(1,3);(2,0)]) in
  printf "context:\n%s\n" (string_of_bg c);
  printf "get_dot:\n%s\n" (get_dot c "context");
  dot_out c "context" "~/tmp/" true;     
  printf "parameter:\n%s\n" (string_of_bg d);
  printf "get_dot:\n%s\n" (get_dot d "parameter");
  dot_out d "parameter" "~/tmp/" true;
  printf "id:\n%s\n" (string_of_bg i);
  printf "get_dot:\n%s\n" (get_dot i "id");
  dot_out i "id" "~/tmp/" true;
  let ls = Big.levels t in
  Array.iteri (fun i l ->
    printf "level %d:\n%s\n" i (string_of_bg l);
    printf "%s\n" (get_dot l "level");
    dot_out l (sprintf "level%d" i) "~/tmp/" true) (Array.of_list ls);
*)
