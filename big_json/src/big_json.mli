(** {2:enc Encoder} *)

val big_to_json : ?minify:bool -> Bigraph.Big.t -> String.t
(** [big_to_json minify b] is a JSON encoder that outputs bigraph [b] to a
    string. If [minify] is [true] (default) the output is made as compact as
    possible, otherwise the output is indented. Below is an example output:

    {[
      {
        "nodes": [
          {
            "node_id": 0,
            "control": {
              "ctrl_name": "Park",
              "ctrl_params": [],
              "ctrl_arity": 3
            }
          },
          {
            "node_id": 1,
            "control": {
              "ctrl_name": "Hospital",
              "ctrl_params": [
                {
                  "ctrl_int": 12
                },
                {
                  "ctrl_string": "Church Street"
                }
              ],
              "ctrl_arity": 0
            }
          },
          {
            "node_id": 2,
            "control": {
              "ctrl_name": "Park",
              "ctrl_params": [],
              "ctrl_arity": 3
            }
          },
          {
            "node_id": 3,
            "control": {
              "ctrl_name": "Bridge",
              "ctrl_params": [
                {
                  "ctrl_float": 3.6
                }
              ],
              "ctrl_arity": 2
            }
          }
        ],
        "place_graph": {
          "num_regions": 2,
          "num_nodes": 4,
          "num_sites": 2,
          "rn": [
            {
              "source": 0,
              "target": 0
            },
            {
              "source": 1,
              "target": 1
            }
          ],
          "rs": [],
          "nn": [
            {
              "source": 0,
              "target": 2
            },
            {
              "source": 1,
              "target": 2
            },
            {
              "source": 1,
              "target": 3
            }
          ],
          "ns": [
            {
              "source": 2,
              "target": 0
            },
            {
              "source": 3,
              "target": 1
            }
          ]
        },
        "link_graph": [
          {
            "inner": [],
            "outer": [],
            "ports": [
              {
                "node_id": 0,
                "port_arity": 1
              }
            ]
          },
          {
            "inner": [],
            "outer": [],
            "ports": [
              {
                "node_id": 0,
                "port_arity": 1
              }
            ]
          },
          {
            "inner": [],
            "outer": [],
            "ports": [
              {
                "node_id": 0,
                "port_arity": 1
              },
              {
                "node_id": 2,
                "port_arity": 1
              },
              {
                "node_id": 3,
                "port_arity": 1
              }
            ]
          },
          {
            "inner": [],
            "outer": [],
            "ports": [
              {
                "node_id": 2,
                "port_arity": 1
              }
            ]
          },
          {
            "inner": [],
            "outer": [],
            "ports": [
              {
                "node_id": 3,
                "port_arity": 1
              }
            ]
          },
          {
            "inner": [],
            "outer": [
              {
                "name": "x"
              }
            ],
            "ports": [
              {
                "node_id": 2,
                "port_arity": 1
              }
            ]
          }
        ]
      }
    ]}*)

val react_to_json : ?minify:bool -> Bigraph.Brs.react -> String.t
(** Encoder for reaction rules. Below is an example output:

    {[
      {
        "brs_lhs": { ... },
        "brs_rhs": { ... },
        "brs_eta": null
      }
    ]}

    Note that in the following we will write [{ ... }] in the example outputs
    to indicate the JSON encoding of a bigraph produced by
    {!val:Big_json.big_to_json}. Optional member [brs_eta] is encoded as
    [sbrs_eta] in {!val:Big_json.sreact_to_json} when present. *)

val preact_to_json : ?minify:bool -> Bigraph.Pbrs.react -> String.t
(** Encoder for probabilistic reaction rules. Below is an example output:

    {[
      {
        "pbrs_lhs": { ... },
        "pbrs_rhs": { ... },
        "pbrs_p": 0.476,
        "pbrs_eta": null
      }
    ]} *)

val sreact_to_json : ?minify:bool -> Bigraph.Sbrs.react -> String.t
(** Encoder for stochastic reaction rules. Below is an example output:

    {[
      {
        "sbrs_lhs": { ... },
        "sbrs_rhs": { ... },
        "sbrs_rate": 8.031000000000001,
        "sbrs_eta": [
          {
            "x": 0,
            "y": 1
          },
          {
            "x": 1,
            "y": 1
          }
        ]
      }
    ]} *)

(** [occs_to_json minify occs] encodes set of occurrences [occs] to a string
    in JSON format. Note [occs] is the set of reachable states obtained by
    applying some reaction rules to an initial bigraph (typically via
    {!val:Bigraph.Brs.step}). Below is an example output:

    {[
      [
        { ... },
        { ... }
      ]
    ]} *)

(* Encoder for non-deterministic reaction rules *)
val nreact_to_json : ?minify:bool -> Bigraph.Nbrs.react -> String.t

val occs_to_json : ?minify:bool -> Bigraph.Big.t list -> String.t

val p_occs_to_json : ?minify:bool -> (Bigraph.Big.t * float) list -> String.t
(** Similar to {!val:Big_json.occs_to_json} but for probabilistic reactive
    systems. Below is an example output:

    {[
      [
        {
          "state": { ... },
          "prob": 1
        }
      ]
    ]} *)

val s_occs_to_json : ?minify:bool -> (Bigraph.Big.t * float) list -> String.t
(** Similar to {!val:Big_json.occs_to_json} but for stochastic reactive
    systems. Below is an example output:

    {[
      [
        {
          "state": { ... },
          "rate": 8.031000000000001
        },
        {
          "state": { ... },
          "rate": 2.457580000000001
        }
      ]
    ]} *)

val n_occs_to_json :
  ?minify:bool -> (Bigraph.Big.t * (string * int * float)) list -> String.t

val matches_to_json : ?minify:bool -> Bigraph.Solver.occ list -> String.t
(** Encoder for bigraphical matches. Below is an example output:

    {[
      [
        {
          "iso_n": [
            {
              "i": 0,
              "j": 0
            },
            {
              "i": 1,
              "j": 1
            },
            {
              "i": 2,
              "j": 2
            },
            {
              "i": 3,
              "j": 3
            }
          ],
          "iso_e": [
            {
              "i": 0,
              "j": 1
            },
            {
              "i": 1,
              "j": 0
            },
            {
              "i": 2,
              "j": 2
            },
            {
              "i": 3,
              "j": 3
            },
            {
              "i": 4,
              "j": 4
            }
          ],
          "f_e": [
            {
              "x": 5,
              "y": 5
            }
          ]
        }
      ]
    ]} *)

val ts_to_json : ?minify:bool -> Bigraph.Brs.graph -> String.t
(** Encoder for transition systems generated by Bigraphical Reactive Systems. *)

val dtmc_to_json : ?minify:bool -> Bigraph.Pbrs.graph -> String.t
(** Encoder for transition systems generated by Probabilistic Bigraphical
    Reactive Systems. *)

val ctmc_to_json : ?minify:bool -> Bigraph.Sbrs.graph -> String.t
(** Encoder for transition systems generated by Stochastic Bigraphical
    Reactive Systems. *)

val mdp_to_json : ?minify:bool -> Bigraph.Nbrs.graph -> String.t
(** Encoder for transition systems generated by Action Bigraphical Reactive
    Systems. *)

(** {2:dec Decoder} *)

val big_of_json :
  ?encoding:Jsonm.encoding -> String.t -> (Bigraph.Big.t, String.t) result
(** [big_of_json encoding j] decodes [j] which is a string expressing a
    bigraph in the JSON format specified by
    {!val:Big_json.big_to_json}.[encoding] specifies the character encoding
    of the data as in the
    {{:http://erratique.ch/software/jsonm/doc/Jsonm.html#TYPEencoding} Jsonm}
    library. *)

val react_of_json :
  ?encoding:Jsonm.encoding ->
  String.t ->
  (Bigraph.Brs.react, String.t) result
(** Similar to {!val:Big_json.big_of_json} but for reaction rules. *)

val preact_of_json :
  ?encoding:Jsonm.encoding ->
  String.t ->
  (Bigraph.Pbrs.react, String.t) result
(** Similar to {!val:Big_json.big_of_json} but for probabilistic reaction
    rules. *)

val sreact_of_json :
  ?encoding:Jsonm.encoding ->
  String.t ->
  (Bigraph.Sbrs.react, String.t) result
(** Similar to {!val:Big_json.big_of_json} but for stochastic reaction rules. *)

val nreact_of_json :
  ?encoding:Jsonm.encoding ->
  String.t ->
  (Bigraph.Nbrs.react, String.t) result
(** Similar to {!val:Big_json.big_of_json} but for non-deterministic reaction
    rules. *)

(** {2:match Interface to the matching engine} *)

val step :
  ?encoding:Jsonm.encoding ->
  ?minify:bool ->
  ?solver:String.t ->
  String.t ->
  String.t
(** [step encoding solver j] computes the set of states reachable in one
    rewriting step. [solver] is a string taking values ["MSAT"] and ["MCARD"]
    for MiniSAT and MiniCARD solvers, respectively. [j] is assumed to be in
    the following JSON format:

    {[
      {
        "state": { ... },
        "reacts": [ ... ]
      }
    ]}

    where [reacts] is an array of reaction rules and [state] the bigraph to
    which they are applied. Each element in [reacts] is in the format
    specified by {!val:Big_json.react_to_json}. Names [sreacts] and [preacts]
    can be used in place of [reacts] to specify stochastic and probabilistic
    reactive systems, respectively.

    Output is as in the formats specified by {!val:Big_json.occs_to_json},
    {!val:Big_json.p_occs_to_json}, and {!val:Big_json.s_occs_to_json}
    depending on the type of reaction rules contained in the input. When an
    error occurs, the output takes the following form:

    {[
      {
        "error": "Not a probability"
      }
    ]}

    Refer to {!val:Bigraph.Brs.step}, {!val:Bigraph.Pbrs.step} and
    {!val:Bigraph.Sbrs.step} for more details on how reachable states are
    computed. *)

val big_match :
  ?minify:bool -> ?solver:String.t -> in_channel -> out_channel -> unit
(** Similar to {!val:Big_json.step} but input and output are channels. *)

(**/**)
