(** This file is meant to be invoked by a toplevel and performs
    initialization of Bigraph library. *)

;;
#require "bigraph"

open Bigraph

;;
#install_printer Big.pp

;;
#install_printer Big.pp_inter

;;
#install_printer Place.pp

;;
#install_printer Link.pp

;;
#install_printer Link.pp_face

;;
#install_printer Nodes.pp

;;
#install_printer Iso.pp

;;
#install_printer Fun.pp

;;
#install_printer Rel.pp

;;
#install_printer Solver.pp_occ
