(******************************************************************************)
(*                                                                            *)
(*        ______ __                          __     _______ ______            *)
(*       |   __ \__|.-----.----.---.-.-----.|  |--.|    ___|   __ \           *)
(*       |   __ <  ||  _  |   _|  _  |  _  ||     ||    ___|      <           *)
(*       |______/__||___  |__| |___._|   __||__|__||_______|___|__|           *)
(*                  |_____|          |__|                                     *)
(*                                                                            *)
(*       Bigraph Evaluator & Rewriting                                        *)
(*                                                                            *)
(*                                                                            *)
(*     Copyright (c) 2010-2013, Michele Sevegnani - University of Glasgow     *)       
(*                                                                            *)
(******************************************************************************)

(** This module provides export operations on bigraphs.
    @author Michele Sevegnani *)

(** [write_big b name path v] writes an svg representation of bigraph
    [b] on file [name] in directory [path]. Argument [v] is a flag for
    verbosity.
    @raise Unix.Unix_error when an underlying system call signals an error. *)
val write_big : Big.bg -> string -> string -> bool -> unit 

(** Same as {!Export.write_big} but for transition systems. 
    @raise Unix.Unix_error when an underlying system call signals an error. *)
val write_ts: Brs.ts -> string -> string -> bool -> unit

(** Same as {!Export.write_big} but for Continuous Time Markov Chains. 
    @raise Unix.Unix_error when an underlying system call signals an error. *)
val write_ctmc: Sbrs.ctmc -> string -> string -> bool -> unit

(** Export a transition system to a text file in PRISM format. 
    @raise Unix.Unix_error when an underlying system call signals an error. *)
val write_ts_prism: Brs.ts -> string -> string -> bool -> unit

(** Same as {!Export.write_ts_prism} but for Continuous Time Markov Chains. *)
val write_ctmc_prism: Sbrs.ctmc -> string -> string -> bool -> unit

(** Export a labelling function to a text file in PRISM format. *) 
val write_csl: (int, int) Hashtbl.t -> string -> string -> bool -> unit
