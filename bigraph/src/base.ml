let int_equal (a : int) (b : int) = a = b

let int_compare a b = a - b

module type Pp = sig
  type t

  val pp : Format.formatter -> t -> unit
end

let pp_print_pair ~open_b ~first ~last ~sep a b out p =
  open_b out ();
  first out;
  a out (fst p);
  sep out;
  b out (snd p);
  last out;
  Format.pp_close_box out ()

module M_opt (M_lib : Map.S) (P : Pp with type t = M_lib.key) = struct
  include M_lib

  let find = find_opt

  let choose = choose_opt

  let min_binding = min_binding_opt

  let max_binding = max_binding_opt

  let pp ~open_b ~first ~last ~sep pp_b out m =
    let open Format in
    let pp_binding out a b =
      pp_print_pair ~open_b:pp_open_hbox
        ~first:(fun out -> pp_print_string out "(")
        ~last:(fun out -> pp_print_string out ")")
        ~sep:(fun out ->
          pp_print_string out ",";
          pp_print_space out ())
        P.pp pp_b out (a, b)
    in
    open_b out ();
    first out;
    ( match min_binding m with
    | None -> ()
    | Some (a, b) ->
        let m' = remove a m in
        pp_binding out a b;
        iter
          (fun a b ->
            sep out;
            pp_binding out a b)
          m' );
    last out;
    pp_close_box out ()
end

module S_opt (S_lib : Set.S) (P : Pp with type t = S_lib.elt) = struct
  include S_lib

  let find = find_opt

  let min_elt = min_elt_opt

  let max_elt = max_elt_opt

  let choose = choose_opt

  let pp ~open_b ~first ~last ~sep out s =
    let open Format in
    open_b out ();
    first out;
    ( match min_elt s with
    | None -> ()
    | Some min ->
        let s' = remove min s in
        P.pp out min;
        iter
          (fun x ->
            sep out;
            P.pp out x)
          s' );
    last out;
    pp_close_box out ()
end

module M_int =
  M_opt
    (Map.Make (struct
      type t = int

      let compare = int_compare
    end))
    (struct
      type t = int

      let pp = Format.pp_print_int
    end)

module M_string =
  M_opt
    (Map.Make
       (String))
       (struct
         type t = String.t

         let pp = Format.pp_print_string
       end)

module S_string =
  S_opt
    (Set.Make
       (String))
       (struct
         type t = String.t

         let pp = Format.pp_print_string
       end)

module H_int = struct
  include Hashtbl.Make (struct
    type t = int

    let equal = int_equal

    let hash = Hashtbl.hash
  end)

  let find = find_opt
end

module H_string = struct
  include Hashtbl.Make (struct
    type t = string

    let equal (x : string) y = x = y

    let hash = Hashtbl.hash
  end)

  let find = find_opt
end

let safe = function Some v -> v | None -> assert false

let ints_compare (i0, p0) (i1, p1) =
  match i0 - i1 with 0 -> p0 - p1 | x -> x

let opt_equal eq a b =
  match (a, b) with
  | None, None -> true
  | None, _ | _, None -> false
  | Some v, Some v' -> eq v v'

let flip f x y = f y x

let flip2 f a b c = f a c b

let list_equal f a b =
  let rec aux = function
    | [], [] -> true
    | [], _ | _, [] -> false
    | x :: xs, y :: ys -> f x y && aux (xs, ys)
  in
  aux (a, b)

(* "\n12\n4\n678\n" -> ["12"; "4"; "678"] "01234\n" -> ["01234"] "0123" ->
   ["0123"] *)
let parse_lines = Str.(split (regexp_string "\n"))

(* "{}" -> "" "[123]" -> "123" *)
let remove_block_delims s =
  if String.length s >= 2 then String.(sub s 1 (length s - 2))
  else invalid_arg "String \"" ^ s ^ "\" has no block delimiters"

(** List pretty printer *)
let pp_list (out : Format.formatter) open_b pp_x sep l =
  let rec pp = function
    | [] -> ()
    | [ x ] -> pp_x out x
    | x :: xs ->
        pp_x out x;
        sep out ();
        pp xs
  in
  open_b out;
  pp l;
  Format.pp_close_box out ()
