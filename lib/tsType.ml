module type T = sig

    type label

    type t = {
	s : (Big.bg_key, (int * Big.bg)) Hashtbl.t;
	e : (int, (int * label)) Hashtbl.t;
	l : (int, int) Hashtbl.t;
      }

    type stats = {
	mutable t : float;    (** Execution time *)
	mutable s : int;      (** Number of states *)
	mutable r : int;      (** Number of reactions *)
	mutable o : int;      (** Number of occurrences *)
      }

    val empty : int -> t
    val init_stats : unit -> stats

    val is_new : Big.bg -> t -> int option

    (* With side-effects on stats *)
    val add_s : Big.bg -> int -> stats -> t -> unit
    val add_e : int -> int -> label -> stats -> t -> unit
    val add_l : int -> int -> t -> unit
    val fold_s : (Big.bg -> int -> 'a -> 'a) -> 'a -> t -> 'a
    val fold_e : (int -> int -> label -> 'a -> 'a) -> 'a -> t -> 'a
    val fold_l : (int -> int -> 'a -> 'a) -> 'a -> t -> 'a
    val iter_s : (int -> Big.bg -> unit) -> t -> unit

    (** Textual representation of a transition system. The format is compatible
      with PRISM input format. *)
    val to_prism : t -> string

    (** Compute the string representation in [dot] format of a transition
      system. *)
    val to_dot : t -> string

  end
