open Format
open Filename
open Big
 
let _end_with_sep s =
  if Str.string_match (Str.regexp (".*" ^ dir_sep ^ "$")) s 0 then s
  else s ^ dir_sep

(* Write a string in dot format to an svg file *)
let write_svg s name path verb =
  let dot_in, bigmc_out = Unix.pipe ()
  and n_path = _end_with_sep path in
  let svg_file =  
    Unix.openfile (concat n_path (name ^ ".svg")) 
      [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY ] 0o777 in
  Unix.set_close_on_exec dot_in;
  Unix.set_close_on_exec svg_file;
  if verb then printf "@[Writing@ %s%s.svg@]@." n_path name else (); 
  let b_w = Unix.write bigmc_out s 0 (String.length s) in
  flush_all ();
  Unix.close bigmc_out;
  let pid = Unix.create_process "dot" [| "dot"; "-Tsvg" |]
    dot_in svg_file Unix.stderr in
  match Unix.waitpid [ Unix.WNOHANG ] pid with
  | _, Unix.WSTOPPED _ -> failwith "Error: dot terminated unexpectedly"
  | _, Unix.WSIGNALED _ | _, Unix.WEXITED _ -> 
    if verb then printf "@[%d bytes written@]@." b_w else ()
      
(* Write a bigraph to svg *)
let dot_out b n path verb = 
  write_svg (get_dot b n) n path verb
  
(* check if cmd returns code when executed with arguments a *)
let _check_cmd cmd a code =
  let read, write = Unix.pipe () in
  let _ = Unix.create_process cmd [| cmd; a |] Unix.stdin write write in
  let _, status = Unix.wait () in
  Unix.close read;
  Unix.close write;
  match status with
  | Unix.WEXITED i ->
    if i = code then ()
    else failwith (sprintf "Error: %s is not installed in the system" cmd)
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> 
    failwith (sprintf "Error: %s is not installed in the system" cmd)
    
(* bimatch does not return the correct exit code*)
let check_setup () = 
  _check_cmd "dot" "-V" 0
 
(* Avoid zombies *)
let wait_before_exit v =
  let rec loop () =
    try
      ignore (Unix.wait ());
      loop ()
    with
    | _ -> if v then printf "@[Terminating ...@]@." else () in
  loop ()


(***************** Write the labelling funtion to a csl file ******************)
(*let csl_out lab path verb =
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
    close_out oc*)

