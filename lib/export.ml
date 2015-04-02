open Printf
open Filename
open Big

(* let _end_with_sep s = *)
(*   if Str.string_match (Str.regexp (".*" ^ dir_sep ^ "$")) s 0 then s *)
(*   else s ^ dir_sep *)

(* Write a string in dot format to an svg file *)
let _write_svg s name path verb =
  let (dot_in, bigmc_out) = Unix.pipe () 
  and n_path = concat path name in
  match Unix.fork () with
  | 0 -> (
      (* child *) 
      Unix.close bigmc_out;    
      Unix.dup2 dot_in Unix.stdin;
      Unix.close dot_in;
      let svg_file =  
        Unix.openfile 
          n_path [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY ] 0o600 in
      Unix.dup2 svg_file Unix.stdout;
      Unix.close svg_file;
      Unix.execvp "dot" [| "dot"; "-Tsvg" |]
    )
  | pid -> (
      (* parent *)
      Unix.close dot_in;    
      if verb then printf "Writing %s\n%!" n_path;
      let b_w = Unix.write_substring bigmc_out s 0 (String.length s) in
      Unix.close bigmc_out;
      match Unix.waitpid [ Unix.WNOHANG ] pid with
      | (_, Unix.WSTOPPED _) -> 
        eprintf "Warning: process %d \"dot\" was stopped.\n" pid
      | (_, Unix.WSIGNALED _) | (_, Unix.WEXITED _) ->
        if verb then printf "%d bytes written\n" b_w
    )

let _write_string s name path verb =
  let f_name = concat path name in
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

(* bimatch does not return the correct exit code*)
(* let check_graphviz () =  *)
(*   match Unix.fork () with *)
(*   | 0 -> ( *)
(*       (\* child *\) *)
(*       try *)
(*         let null =  *)
(*           Unix.openfile  *)
(*             "/dev/null" [ Unix.O_WRONLY; Unix.O_NONBLOCK ] 0o200  in *)
(*         Unix.dup2 null Unix.stderr; *)
(*         Unix.dup2 null Unix.stdout; *)
(*         Unix.close null; *)
(*         Unix.execvp "dot" [| "dot"; "-V" |] *)
(*       with *)
(*       | Unix.Unix_error (err, _, _) -> *)
(*         failwith (Unix.error_message err) *)
(*     ) *)
(*   | pid -> ( *)
(*       (\* parent *\) *)
(*       ignore (Unix.wait ()) *)
(*     ) *)

let string_of_l l =
  let inv = Hashtbl.create (Hashtbl.length l) in
  let properties = 
    Hashtbl.fold (fun s p acc -> 
        Hashtbl.add inv p s;
        p :: acc
      ) l [] in
  String.concat "\n" (List.map (fun p ->
      sprintf "label \"p_%d\" = %s;" p 
        (String.concat " | " (List.map (fun s ->
             sprintf "x = %d" s) (Hashtbl.find_all inv p)))
    ) properties)

let write_csl l n path verb =
  _write_string (string_of_l l) n path verb

