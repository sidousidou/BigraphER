(* Types of rective systems *)
type t = BRS | PBRS | SBRS | NBRS

exception EXPORT_ERROR of string

let to_string = function
  | BRS -> "BRS"
  | PBRS -> "Probabilistic BRS"
  | SBRS -> "Stochastic BRS"
  | NBRS -> "Nondeterministic BRS"

let to_string_ext = function
  | BRS -> "Bigraphical Reactive System"
  | PBRS -> "Probabilistic Bigraphical Reactive System"
  | SBRS -> "Stochastic Bigraphical Reactive System"
  | NBRS -> "Nondeterministic Bigraphical Reactive System"

let sim_type = function
  | BRS | PBRS | NBRS -> "simulation"
  | SBRS -> "stochastic simulation"

let ts_type = function
  | BRS -> "transition system"
  | PBRS -> "DTMC" (* Discrete Time Markov Chain *)
  | SBRS -> "CTMC" (* Continuous Time Markov Chain *)
  | NBRS -> "MDP"  (* Markov Decision Process *)

let limit_type = function
  | BRS | PBRS | NBRS -> "step"
  | SBRS -> "time"

let limit_msg = function
  | BRS | PBRS | NBRS -> "number of simulation steps"
  | SBRS -> "simulation time"

let module_id = function
  | BRS -> "Brs"
  | PBRS -> "Pbrs"
  | SBRS -> "Sbrs"
  | NBRS -> "Nbrs"
