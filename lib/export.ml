open Printf
open Filename
open Big
 
let _end_with_sep s =
  if Str.string_match (Str.regexp (".*" ^ dir_sep ^ "$")) s 0 then s
  else s ^ dir_sep

(* Write a string in dot format to an svg file *)
let _write_svg s name path verb =
  let (dot_in, bigmc_out) = Unix.pipe ()
  and n_path = _end_with_sep path in
  let svg_file =  
    Unix.openfile (concat n_path (name ^ ".svg")) 
      [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY ] 0o777 in
  if verb then printf "Writing %s%s.svg\n%!" n_path name; 
  let b_w = Unix.write bigmc_out s 0 (String.length s) in
  Unix.close bigmc_out;
  let pid = Unix.create_process "dot" [| "dot"; "-Tsvg" |]
    dot_in svg_file Unix.stderr in
  Unix.close dot_in;
  Unix.close svg_file;
  match Unix.waitpid [ Unix.WUNTRACED ] pid with
  | (_, Unix.WSTOPPED _) -> eprintf "Warning: dot process was stopped.\n"
  | (_, Unix.WSIGNALED _) | (_, Unix.WEXITED _) ->
    if verb then printf "%d bytes written\n" b_w

let _write_string s name path verb =
  let f_name = concat (_end_with_sep path) name in
  if verb then printf "Writing %s\n" f_name; 
  let out_ch = open_out f_name in
  output_string out_ch s;
  close_out out_ch;
  if verb then printf "%d bytes written\n" (String.length s)
      
let write_big b n path verb =
  _write_svg (get_dot b n) n path verb

let write_ts ts n path verb =
  _write_svg (Brs.to_dot ts) n path verb

let write_ctmc ctmc n path verb =
  _write_svg (Sbrs.to_dot ctmc) n path verb

let write_ts_prism ts n path verb =
  _write_string (Brs.to_prism ts) n path verb

let write_ctmc_prism ctmc n path verb =
  _write_string (Sbrs.to_prism ctmc) n path verb

(* check if cmd returns code when executed with arguments a *)
let _check_cmd cmd a code =
  let (read, write) = Unix.pipe () in
  let _ = Unix.create_process cmd [| cmd; a |] Unix.stdin write write in
  let (_, status) = Unix.wait () in
  Unix.close read;
  Unix.close write;
  match status with
  | Unix.WEXITED i ->
    if i = code then ()
    else failwith (sprintf "Error: %s is not installed in the system" cmd)
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> 
    failwith (sprintf "Error: %s is not installed in the system" cmd)
    
(* bimatch does not return the correct exit code*)
let check_graphviz () = 
  _check_cmd "dot" "-V" 0
 
(* Avoid zombies *)
let wait_before_exit v =
  let rec loop () =
    try
      ignore (Unix.wait ());
      loop ()
    with
    | _ -> if v then printf "Terminating ...\n" else () in
  loop ()

let string_of_l l =
  let inv = Hashtbl.create (Hashtbl.length l) in
  let properties = 
    Hashtbl.fold (fun s p acc -> 
      Hashtbl.add inv p s;
      p :: acc) l [] in
  String.concat "\n" (List.map (fun p ->
    sprintf "label \"p_%d\" = %s;" p 
      (String.concat " | " (List.map (fun s ->
       sprintf "x = %d" s) (Hashtbl.find_all inv p)))) properties)

let write_csl l n path verb =
  _write_string (string_of_l l) n path verb

