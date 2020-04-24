(* Decompose a bigraph based on a given match *)
open Bigraph
open Printf

let b_in_ch = ref stdin
let m_in_ch = ref stdin
let basename = ref "decomp"

let options =
[
    ( "-b",
    Arg.String (fun s -> b_in_ch := open_in s),
    "<file> Bigraph file <file>" );
    ( "-m",
    Arg.String (fun s -> m_in_ch := open_in s),
    "<file> Match file <file>" );
    ( "-n",
    Arg.String (fun s -> basename := s),
    "Basename for output files, e.g. <basename>_d.svg"
    );
]

let write_dot b extn =
  let oc = open_out (!basename ^ "_" ^ extn ^ ".dot") in
  fprintf oc "%s\n" (Big.to_dot b "");
  close_out oc

let () =
  Arg.parse options (fun _ -> ()) "Usage: big_decomp <options>\noptions are:";
  let bigraph = really_input_string !b_in_ch (in_channel_length !b_in_ch) in
  let match_ = really_input_string !m_in_ch (in_channel_length !m_in_ch) in
  let occs = Big.occurrences ~target:(Big.of_string bigraph) ~pattern:(Big.of_string match_)
  in
  if List.length occs > 0 then
    begin
        let i_n, i_e, i_h = List.hd occs in
        let (c, d, id_) =
          Big.decomp
           ~target:(Big.of_string bigraph)
           ~pattern:(Big.of_string match_) ~i_n ~i_e i_h in
        write_dot (Big.of_string bigraph) "bigraph";
        write_dot c "ctx";
        write_dot d "param";
        write_dot id_ "id";
        write_dot (Big.of_string match_) "match";
        close_in !b_in_ch;
        close_in !m_in_ch;
        print_endline "Decomposition outputted"
    end
  else
    begin
        close_in !b_in_ch;
        close_in !m_in_ch;
        print_endline "No match found in bigraph"
    end
