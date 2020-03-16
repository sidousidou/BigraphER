type error =
  | Dot_not_found
  | Dot_stopped of int
  | Dot_killed of int
  | Dot_internal_error of int		    
  | Internal_error of Unix.error * string * string
  | Sys of string
       
exception ERROR of error

let report_error = function
  | Dot_not_found -> "`dot' command not found"
  | Dot_stopped i -> "`dot' stopped by signal " ^ (string_of_int i)
  | Dot_killed i -> "`dot' killed by signal " ^ (string_of_int i)
  | Dot_internal_error i -> "`dot' returned with code " ^ (string_of_int i)
  | Sys s -> s
  | Internal_error (e, fname, arg) ->
     (Unix.error_message e) ^ " at \""^ fname ^ "\" \"" ^ arg ^ "\""
		     
(* Write a string in dot format to an svg file *)
let _write_svg s name path =
  let (dot_in, bigmc_out) = Unix.pipe () 
  and n_path = Filename.concat path name in
  match Unix.fork () with
  | 0 ->
     (* child *) 
     (try
	 Unix.close bigmc_out;    
	 Unix.dup2 dot_in Unix.stdin;
	 Unix.close dot_in;
	 let svg_file =  
	   Unix.openfile 
             n_path [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY ] 0o600 in
	 Unix.dup2 svg_file Unix.stdout;
	 Unix.close svg_file;
	 Unix.execvp "dot" [| "dot"; "-Tsvg" |]
       with
       | _ -> exit 127)
  | pid ->
     (* parent *)
     (Unix.close dot_in;    
      let b_w = Unix.write_substring bigmc_out s 0 (String.length s) in
      Unix.close bigmc_out;
      match snd (Unix.waitpid [] pid) with
      | Unix.WSTOPPED i -> raise (ERROR (Dot_stopped i))
      | Unix.WSIGNALED i -> raise (ERROR (Dot_killed i))
      | Unix.WEXITED 0 -> b_w
      | Unix.WEXITED 127 -> raise (ERROR (Dot_not_found))
      | Unix.WEXITED i -> raise (ERROR (Dot_internal_error i)))

let catch_unix_errors f arg name path =
  try f arg name path with
  | Unix.Unix_error (e,fname,args) ->
     raise (ERROR (Internal_error (e, fname, args)))

let write_svg s ~name ~path =
  catch_unix_errors _write_svg s name path
	   
let write_string s ~name ~path =
  try
    let out_ch =
      Filename.concat path name
      |> open_out in
    output_string out_ch s;
    close_out out_ch;
    String.length s
  with
  | Sys_error s -> raise (ERROR (Sys s))
