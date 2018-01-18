(** JSON codec for bigraphical structures
    @author Michele Sevegnani *)

(** {1:enc Encoder} *)

(**
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
}]}
*)
val big_to_json : ?minify:bool -> Bigraph.Big.t -> String.t 


(**
{[{
  "brs_lhs": { ... },
  "brs_rhs": { ... },
  "brs_eta": null
}]}
*)
val react_to_json : ?minify:bool -> Bigraph.Brs.react -> String.t 


(**
{[{
  "pbrs_lhs": { ... },
  "pbrs_rhs": { ... },
  "pbrs_p": 0.476,
  "pbrs_eta": null
}]}
*)
val preact_to_json : ?minify:bool -> Bigraph.Pbrs.react -> String.t 


(**
{[{
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
}]}
*)
val sreact_to_json : ?minify:bool -> Bigraph.Sbrs.react -> String.t 

val occs_to_json : ?minify:bool -> Bigraph.Brs.occ list -> String.t 

val p_occs_to_json : ?minify:bool -> Bigraph.Pbrs.occ list -> String.t 


(**
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
]}
*)
val s_occs_to_json : ?minify:bool -> Bigraph.Sbrs.occ list -> String.t 

(**
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
]}
*)
val matches_to_json : ?minify:bool -> Bigraph.Big.occ list -> String.t 

(** {1:dec Decoder} *)

val big_of_json : ?encoding:Jsonm.encoding -> String.t  -> (Bigraph.Big.t, String.t) result

val react_of_json : ?encoding:Jsonm.encoding -> String.t  -> (Bigraph.Brs.react, String.t) result

val preact_of_json : ?encoding:Jsonm.encoding -> String.t  -> (Bigraph.Pbrs.react, String.t) result

val sreact_of_json : ?encoding:Jsonm.encoding -> String.t  -> (Bigraph.Sbrs.react, String.t) result

(** {1:match Interface to the matching engine} *)

(** Compute the set of reachable states in one step. Note that isomorphic states
    are merged.
{[
[
  { ... },
  { ... }
]
]}

{[
[
  {
    "state": { ... },
    "prob": 1
  }
]
]}

{[
[
  {
    "state": { ... },
    "rate": 8.031000000000001
  },
  {
    "state": { ... },
    "rate": 4.941004000600001
  }
]
]}

{[
{
  "error": "Not a probability"
}
]}
*)
val step : ?encoding:Jsonm.encoding -> ?minify:bool -> String.t -> String.t
(**/**)
