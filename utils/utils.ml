let int_compare a b = a - b

let int_equal (a : int) (b : int) = a = b

let safe = function
  | Some v -> v
  | None -> assert false

type text_style =
  [ `bold
  | `underline
  | `black
  | `red
  | `green
  | `yellow
  | `blue
  | `magenta
  | `cyan
  | `white ]

let colorise (c: text_style) s =
  let code = 
    match c with
    | `bold      -> "01"
    | `underline -> "04"
    | `black     -> "30"
    | `red       -> "31"
    | `green     -> "32"
    | `yellow    -> "33"
    | `blue      -> "1;34"
    | `magenta   -> "35"
    | `cyan      -> "36"
    | `white     -> "37"
  in
  Printf.sprintf "\027[%sm%s\027[m" code s

let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
             
let format_time () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%s %s %2d %02d:%02d:%02d %04d"
    days.(tm.Unix.tm_wday)
    months.(tm.Unix.tm_mon)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec
    (tm.Unix.tm_year + 1900)

let (/) = Filename.concat

let safe_mkdir dir =
  if not (Sys.file_exists dir) then
    try
      Unix.mkdir dir 0o755
    with
      Unix.Unix_error(Unix.EEXIST,_,_) -> ()

let mkdir dir =
  let rec aux dir =
    if not (Sys.file_exists dir) then (
      aux (Filename.dirname dir);
      safe_mkdir dir;
    ) in
  aux dir
