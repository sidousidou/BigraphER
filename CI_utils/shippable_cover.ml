(* Utilities to convert bisect coverage reports to the cobertura format. *)

module ST = Shippable_test

(* Example input

   Summary: 7764/13396 (57.96%) File 'bin/ast.ml': 27/84 (32.14%) File
   'bin/bigrapher.ml': 178/299 (59.53%) File 'bin/cmd.ml': 22/229 (9.61%)
   File 'bin/lexer.ml': 88/160 (55.00%) *)

let first_token l c_start c_end =
  String.(
    let i = index l c_start in
    let j = index_from l (i + 1) c_end in
    sub l (i + 1) (j - i - 1))

let parse_filename s = first_token s '\'' '\''

let parse_coverage s =
  let s' = first_token s '(' ')' in
  String.(sub s' 0 (length s' - 1))
  |> float_of_string
  |> (fun x -> x /. 100.0)
  |> string_of_float

let to_attribs m v =
  "line-rate=\"" ^ parse_coverage m ^ "\" " ^ "version=\"" ^ v ^ "\" "
  ^ "timestamp=\""
  ^ string_of_float (Unix.time ())
  ^ "\""

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
  List.map
    (fun m ->
      "<package " ^ "name=\"" ^ parse_filename m ^ "\" " ^ "line-rate=\""
      ^ parse_coverage m ^ "\" " ^ ">\n<classes></classes>\n</package>")
    ms
  |> String.concat "\n"

(* <!ATTLIST package name        CDATA #REQUIRED> *)
(* <!ATTLIST package line-rate   CDATA #REQUIRED> *)
(* <!ATTLIST package branch-rate CDATA #REQUIRED> *)
(* <!ATTLIST package complexity  CDATA #REQUIRED> *)

let to_cobertura v = function
  | [] -> assert false
  | summary :: modules ->
      ST.header ^ "\n<coverage " ^ to_attribs summary v ^ ">\n<packages>\n"
      ^ to_packages modules ^ "\n</packages>\n</coverage>"

(* cobertura.native report path name version *)
(* let () =
 *   Io.parse Sys.argv.(1) |> to_cobertura Sys.argv.(4) |> fun s ->
 *   ST.write_xml s Sys.argv.(2) Sys.argv.(3) *)
