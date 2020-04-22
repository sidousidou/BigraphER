open Printf

let header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

let string_of_attributes tag attr =
  String.concat " "
    (tag :: List.map (fun (a, v) -> a ^ "=\"" ^ v ^ "\"") attr)

let xml_block tag attr chl_blocks =
  sprintf "<%s>\n%s</%s>"
    (string_of_attributes tag attr)
    (String.concat "\n"
       (List.filter (fun s -> String.length s > 0) chl_blocks))
    tag

let attr_eq_int =
  [ ("type", "ASSERT_EQ_INT"); ("message", "Values do not match") ]

let attr_err = [ ("type", "INTERNAL_ERROR"); ("message", "Internal error") ]

let error_msg = "INTERNAL_ERROR"

let assert_eq_int id reference out =
  if out = reference then ""
  else
    xml_block "failure" attr_eq_int
      [ sprintf "%s: %-8d != %-8d" id out reference ]

let testsuite name cases =
  header ^ "\n<testsuites name=\"bigraph\">\n"
  ^ xml_block "testsuite"
      [ ("name", name); ("tests", string_of_int (List.length cases)) ]
      (List.map
         (fun (name, mod_name, out, fail) ->
           xml_block "testcase"
             [ ("name", name); ("class", mod_name) ]
             (out :: fail))
         cases)
  ^ "\n</testsuites>\n"

let write_xml s path name =
  let f_name = Filename.concat path name in
  let out_ch = open_out f_name in
  output_string out_ch s;
  close_out out_ch
