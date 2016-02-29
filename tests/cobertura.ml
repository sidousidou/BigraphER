(* parser for bisect files *)

type item = String.t * String.t
			 
type mod_cover =
  { name : String.t;
    items: item list; }

let is_item l =
  (String.sub l 0 3) = " - "

let first_token l c_start c_end =
  let i = String.index l c_start in
  let j = String.index_from l (i + 1)
			    c_end in
  String.sub l (i + 1) (j - i - 1)
			 
let parse_item l =
  let name =
    try first_token l '\'' '\'' with
    | Not_found -> "total"
  and percent =
    try first_token l '(' ')' with
    | Not_found -> "none" in
  (name, percent)
    
let parse_bisect_report = function
  | [] -> assert false
  | _ :: xs ->
     List.fold_left (fun (acc, acc_i, acc_n) l ->
		     if is_item l
		     then (acc, (parse_item l) :: acc_i, acc_n)
		     else try ({name = acc_n; items = List.rev acc_i} :: acc,
			       [],
			       first_token l '\'' '\'')
			  with
			  | Not_found -> (acc, acc_i , acc_n))
		    ([], [], "Summary") xs
  |> fun (a, _, _) -> a
  |> List.rev

let get_rate m =
  m.items |> List.rev |> List.hd |> snd
       
let to_attribs m v =
  "line-rate=\"" ^ (get_rate m) ^ "\" "
  ^ "version=\"" ^ v ^ "\" "  
  ^ "timestamp=\"" ^ (string_of_float (Unix.time ())) ^ "\""

(* <!ATTLIST coverage line-rate        CDATA #REQUIRED> *)
(* <!ATTLIST coverage branch-rate      CDATA #REQUIRED> *)
(* <!ATTLIST coverage lines-covered    CDATA #REQUIRED> *)
(* <!ATTLIST coverage lines-valid      CDATA #REQUIRED> *)
(* <!ATTLIST coverage branches-covered CDATA #REQUIRED> *)
(* <!ATTLIST coverage branches-valid   CDATA #REQUIRED> *)
(* <!ATTLIST coverage complexity       CDATA #REQUIRED> *)
(* <!ATTLIST coverage version          CDATA #REQUIRED> *)
(* <!ATTLIST coverage timestamp        CDATA #REQUIRED> *)

let to_packages ms =
  List.map (fun m ->
	    "<package "
	    ^ "name=\"" ^ m.name ^ "\" "
	    ^ "line-rate=\"" ^ (get_rate m) ^ "\""
	    ^ ">\n<classes></classes>\n</package>") ms
  |> String.concat "\n" 

(* <!ATTLIST package name        CDATA #REQUIRED> *)
(* <!ATTLIST package line-rate   CDATA #REQUIRED> *)
(* <!ATTLIST package branch-rate CDATA #REQUIRED> *)
(* <!ATTLIST package complexity  CDATA #REQUIRED> *)
		   
let to_cobertura v = function
  | [] -> assert false
  | summary :: modules ->
     Junit.header
     ^ "\n<coverage "
     ^ (to_attribs summary v)
     ^ ">\n<packages>\n"
     ^ (to_packages modules)
     ^ "\n</packages>\n</coverage>"
       
(* cobertura.native report path name version *)
let () =  
  Io.parse Sys.argv.(1)
  |> parse_bisect_report
  |> to_cobertura Sys.argv.(4)
  |> fun s -> Junit.write_xml s Sys.argv.(2) Sys.argv.(3)
