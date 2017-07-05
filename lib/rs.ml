(* Types of rective systems *)
type t = BRS | PBRS | SBRS

let to_string = function
  | BRS -> "BRS"
  | PBRS -> "Probabilistic BRS"
  | SBRS -> "Stochastic BRS"

let to_string_ext = function
  | BRS -> "Bigraphical Reactive System"
  | PBRS -> "Probabilistic Bigraphical Reactive System"
  | SBRS -> "Stochastic Bigraphical Reactive System"

let sim_type = function
  | BRS | PBRS -> "simulation"
  | SBRS -> "stochastic simulation"

let ts_type = function
  | BRS -> "transition system"
  | PBRS -> "DTMC" (* Discrete Time Markov Chain *)
  | SBRS -> "CTMC" (* Continuous Time Markov Chain *)

let limit_type = function
  | BRS | PBRS -> "step"
  | SBRS -> "time"

let limit_msg = function
  | BRS | PBRS -> "number of simulation steps"
  | SBRS -> "simulation time"

let module_id = function
  | BRS -> "Brs"
  | PBRS -> "Pbrs"
  | SBRS -> "Sbrs"
