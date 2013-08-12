open Lexing
open Printf

exception UNKNOWN_CHAR of char * position
exception INVALID_INTERVAL
exception PARSE_ERROR

(* Raise Sys_error if the file could not be opened *)
let open_lex path = 
  let file = open_in path in
  let lexbuf = Lexing.from_channel file in
  lexbuf.Lexing.lex_curr_p <-
    { Lexing.pos_fname = Filename.basename path;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0;
    };
  lexbuf, file          

let incr_linenum lexbuf =
  let pos = lexbuf.lex_curr_p in 
  lexbuf.lex_curr_p <- 
    { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum
    }

let add_pos () =
  Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()

let print_pos (p0, p1) =
  eprintf "File \"%s\", line %d, charachters %d-%d:\n"
    p0.pos_fname p0.pos_lnum (p0.pos_cnum - p0.pos_bol)
    (p1.pos_cnum - p1.pos_bol)

let parse_error_msg msg tok =
  let start_pos = Parsing.symbol_start_pos ()
  and end_pos = Parsing.symbol_end_pos () in
  print_pos (start_pos, end_pos);
  prerr_string ("Syntax error: " ^ msg);
  match tok with
  | "" -> prerr_newline ()
  | _ -> prerr_endline (" " ^ tok)

let int_interval s h e =
  if e <= s || h <=0 then raise INVALID_INTERVAL
  else let rec aux s h e res =
	 if s < e then aux (s + h) h e (s :: res)
	 else e :: res
       in List.fast_sort (fun x y -> x - y) (aux s h e [])
  
let float_interval s h e =
  if e <= s || h <=0.0 then raise INVALID_INTERVAL
  else let rec aux s h e res =
	 if s < e then aux (s +. h) h e (s :: res)
	 else e :: res
       in List.fast_sort compare (aux s h e [])

let rec par_comb pars = 
  match pars with
  | [x] -> List.map (fun v -> [v]) x
  | x::xs -> 
    begin
	let aux1 v ls =
	  List.map (fun l -> v :: l) ls
	in let rec aux2 l ls = 
	     match l with
	     | [] -> []
	     | x :: xs -> (aux1 x ls) @ (aux2 xs ls)
	   in aux2 x (par_comb xs) 
    end
  | [] -> []
    
