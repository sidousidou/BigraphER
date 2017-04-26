(* Types of rective systems *)
type t = BRS | SBRS  (*PBRS*)
         
let to_string = function
  | BRS -> "BRS"
  | SBRS -> "Stochastic BRS"
    
let to_string_ext = function
  | BRS -> "Bigraphical Reactive System"
  | SBRS -> "Stochastic Bigraphical Reactive System"

let sim_type = function
  | BRS -> "simulation"
  | SBRS -> "stochastic simulation"

let ts_type = function
  | BRS -> "transition system"
  | SBRS -> "CTMC" (* Continuous Time Markov Chain *)

let limit_type = function
  | BRS -> "step"
  | SBRS -> "time" 

let limit_msg = function
  | BRS -> "number of simulation steps"
  | SBRS -> "simulation time" 
