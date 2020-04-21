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

let colorise (c : text_style) s =
  let code =
    match c with
    | `bold -> "01"
    | `underline -> "04"
    | `black -> "30"
    | `red -> "31"
    | `green -> "32"
    | `yellow -> "33"
    | `blue -> "1;34"
    | `magenta -> "35"
    | `cyan -> "36"
    | `white -> "37"
  in
  "\027[" ^ code ^ "m" ^ s ^ "\027[m"

let warn = colorise `yellow "Warning" |> colorise `bold

let err = colorise `red "Error" |> colorise `bold

let aux_opt f v s = if f then v else s

let err_opt f = aux_opt f err "Error"

let warn_opt f = aux_opt f warn "Warning"

let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let months =
  [|
    "Jan";
    "Feb";
    "Mar";
    "Apr";
    "May";
    "Jun";
    "Jul";
    "Aug";
    "Sep";
    "Oct";
    "Nov";
    "Dec";
  |]

let format_time () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%s %s %02d %02d:%02d:%02d %04d" days.(tm.Unix.tm_wday)
    months.(tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec (tm.Unix.tm_year + 1900)

let dot_installed () =
  try
    match Unix.fork () with
    | 0 -> (
        try
          let null =
            Unix.openfile "/dev/null"
              [ Unix.O_WRONLY; Unix.O_NONBLOCK ]
              0o200
          in
          Unix.dup2 null Unix.stderr;
          Unix.dup2 null Unix.stdout;
          Unix.execvp "dot" [| "dot"; "-V" |]
        with _ -> exit 127 )
    | p -> (
        match snd (Unix.waitpid [] p) with
        | Unix.WEXITED 0 -> true
        | Unix.WEXITED _ | Unix.WSTOPPED _ | Unix.WSIGNALED _ -> false )
  with Unix.Unix_error _ -> false
