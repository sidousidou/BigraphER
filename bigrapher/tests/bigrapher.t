Test BigraphER output.
  $ for model in ../examples/rts_cts.big \
  >              ../examples/actors.big \
  >              ../examples/wireless.big \
  >              ../examples/savannah.big \
  >              ../examples/rrim.big \
  >              ../examples/rrim2.big \
  >              ../examples/hospital.big \
  >              ../examples/closures.big \
  >              ../examples/merge.big \
  >              ../examples/prob.big \
  >              ../examples/link_inst_map.big \
  >              ../examples/string_params.big \
  >              ../examples/iter-merge.big \
  >              ../examples/norules.big \
  >              ../examples/unique_entities.big \
  >              ../examples/conditional_turn_taking.big \
  >              ../examples/multiset-matching.big \
  >              ../examples/virus-simpl.big \
  >              ../examples/virus-multifirewall.big \
  >              ../examples/abrs-mobilesink.big \
  >              ../examples/prob_wsn.big \
  >              ../examples/mdp_wsn.big \
  >              ../examples/stoch_occ.big; do
  >   for solver in MSAT \
  >                 MCARD \
  >                 KSAT \
  >                 MAPLE; do
  >     bigrapher full -l out.csl -p out.tra -s -t out --solver=$solver --no-colors --debug -M 140 "$model";
  >   done
  > done
  Type:          Stochastic BRS
  Bindings:      66      
  # of rules:    487     
  Max # states:  140     
  States:        128     
  Transitions:   154     
  Occurrences:   430     
  Type:          Stochastic BRS
  Bindings:      66      
  # of rules:    487     
  Max # states:  140     
  States:        128     
  Transitions:   154     
  Occurrences:   430     
  Type:          Stochastic BRS
  Bindings:      66      
  # of rules:    487     
  Max # states:  140     
  States:        128     
  Transitions:   154     
  Occurrences:   430     
  Type:          Stochastic BRS
  Bindings:      66      
  # of rules:    487     
  Max # states:  140     
  States:        128     
  Transitions:   154     
  Occurrences:   430     
  Type:          BRS
  Bindings:      16      
  # of rules:    4       
  Max # states:  140     
  States:        10      
  Transitions:   12      
  Occurrences:   12      
  Type:          BRS
  Bindings:      16      
  # of rules:    4       
  Max # states:  140     
  States:        10      
  Transitions:   12      
  Occurrences:   12      
  Type:          BRS
  Bindings:      16      
  # of rules:    4       
  Max # states:  140     
  States:        10      
  Transitions:   12      
  Occurrences:   12      
  Type:          BRS
  Bindings:      16      
  # of rules:    4       
  Max # states:  140     
  States:        10      
  Transitions:   12      
  Occurrences:   12      
  Type:          Stochastic BRS
  Bindings:      4       
  # of rules:    1       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          Stochastic BRS
  Bindings:      4       
  # of rules:    1       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          Stochastic BRS
  Bindings:      4       
  # of rules:    1       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          Stochastic BRS
  Bindings:      4       
  # of rules:    1       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          BRS
  Bindings:      34      
  # of rules:    11      
  Max # states:  140     
  States:        107     
  Transitions:   252     
  Occurrences:   252     
  Type:          BRS
  Bindings:      34      
  # of rules:    11      
  Max # states:  140     
  States:        107     
  Transitions:   252     
  Occurrences:   252     
  Type:          BRS
  Bindings:      34      
  # of rules:    11      
  Max # states:  140     
  States:        107     
  Transitions:   252     
  Occurrences:   252     
  Type:          BRS
  Bindings:      34      
  # of rules:    11      
  Max # states:  140     
  States:        107     
  Transitions:   252     
  Occurrences:   252     
  Type:          BRS
  Bindings:      9       
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   1       
  Occurrences:   1       
  Type:          BRS
  Bindings:      9       
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   1       
  Occurrences:   1       
  Type:          BRS
  Bindings:      9       
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   1       
  Occurrences:   1       
  Type:          BRS
  Bindings:      9       
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   1       
  Occurrences:   1       
  Type:          BRS
  Bindings:      10      
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   1       
  Occurrences:   1       
  Type:          BRS
  Bindings:      10      
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   1       
  Occurrences:   1       
  Type:          BRS
  Bindings:      10      
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   1       
  Occurrences:   1       
  Type:          BRS
  Bindings:      10      
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   1       
  Occurrences:   1       
  Type:          BRS
  Bindings:      33      
  # of rules:    10      
  Max # states:  140     
  States:        144     
  Transitions:   255     
  Occurrences:   345     
  Type:          BRS
  Bindings:      33      
  # of rules:    10      
  Max # states:  140     
  States:        144     
  Transitions:   255     
  Occurrences:   345     
  Type:          BRS
  Bindings:      33      
  # of rules:    10      
  Max # states:  140     
  States:        144     
  Transitions:   255     
  Occurrences:   345     
  Type:          BRS
  Bindings:      33      
  # of rules:    10      
  Max # states:  140     
  States:        144     
  Transitions:   255     
  Occurrences:   345     
  Type:          BRS
  Bindings:      5       
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   2       
  Occurrences:   3       
  Type:          BRS
  Bindings:      5       
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   2       
  Occurrences:   3       
  Type:          BRS
  Bindings:      5       
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   2       
  Occurrences:   3       
  Type:          BRS
  Bindings:      5       
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   2       
  Occurrences:   3       
  Type:          BRS
  Bindings:      16      
  # of rules:    1       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          BRS
  Bindings:      16      
  # of rules:    1       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          BRS
  Bindings:      16      
  # of rules:    1       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          BRS
  Bindings:      16      
  # of rules:    1       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          Probabilistic BRS
  Bindings:      9       
  # of rules:    3       
  Max # states:  140     
  States:        14      
  Transitions:   29      
  Occurrences:   64      
  Type:          Probabilistic BRS
  Bindings:      9       
  # of rules:    3       
  Max # states:  140     
  States:        14      
  Transitions:   29      
  Occurrences:   64      
  Type:          Probabilistic BRS
  Bindings:      9       
  # of rules:    3       
  Max # states:  140     
  States:        14      
  Transitions:   29      
  Occurrences:   64      
  Type:          Probabilistic BRS
  Bindings:      9       
  # of rules:    3       
  Max # states:  140     
  States:        14      
  Transitions:   29      
  Occurrences:   64      
  Type:          BRS
  Bindings:      10      
  # of rules:    1       
  Max # states:  140     
  States:        142     
  Transitions:   141     
  Occurrences:   141     
  Type:          BRS
  Bindings:      10      
  # of rules:    1       
  Max # states:  140     
  States:        142     
  Transitions:   141     
  Occurrences:   141     
  Type:          BRS
  Bindings:      10      
  # of rules:    1       
  Max # states:  140     
  States:        142     
  Transitions:   141     
  Occurrences:   141     
  Type:          BRS
  Bindings:      10      
  # of rules:    1       
  Max # states:  140     
  States:        142     
  Transitions:   141     
  Occurrences:   141     
  Type:          BRS
  Bindings:      8       
  # of rules:    2       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          BRS
  Bindings:      8       
  # of rules:    2       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          BRS
  Bindings:      8       
  # of rules:    2       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          BRS
  Bindings:      8       
  # of rules:    2       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          BRS
  Bindings:      8       
  # of rules:    2       
  Max # states:  140     
  States:        1       
  Transitions:   1       
  Occurrences:   21      
  Type:          BRS
  Bindings:      8       
  # of rules:    2       
  Max # states:  140     
  States:        1       
  Transitions:   1       
  Occurrences:   21      
  Type:          BRS
  Bindings:      8       
  # of rules:    2       
  Max # states:  140     
  States:        1       
  Transitions:   1       
  Occurrences:   21      
  Type:          BRS
  Bindings:      8       
  # of rules:    2       
  Max # states:  140     
  States:        1       
  Transitions:   1       
  Occurrences:   21      
  Type:          BRS
  Bindings:      2       
  # of rules:    0       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          BRS
  Bindings:      2       
  # of rules:    0       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          BRS
  Bindings:      2       
  # of rules:    0       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          BRS
  Bindings:      2       
  # of rules:    0       
  Max # states:  140     
  States:        1       
  Transitions:   0       
  Occurrences:   0       
  Type:          BRS
  Bindings:      4       
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   1       
  Occurrences:   1       
  Type:          BRS
  Bindings:      4       
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   1       
  Occurrences:   1       
  Type:          BRS
  Bindings:      4       
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   1       
  Occurrences:   1       
  Type:          BRS
  Bindings:      4       
  # of rules:    1       
  Max # states:  140     
  States:        2       
  Transitions:   1       
  Occurrences:   1       
  Type:          BRS
  Bindings:      12      
  # of rules:    4       
  Max # states:  140     
  States:        12      
  Transitions:   16      
  Occurrences:   20      
  Type:          BRS
  Bindings:      12      
  # of rules:    4       
  Max # states:  140     
  States:        12      
  Transitions:   16      
  Occurrences:   20      
  Type:          BRS
  Bindings:      12      
  # of rules:    4       
  Max # states:  140     
  States:        12      
  Transitions:   16      
  Occurrences:   20      
  Type:          BRS
  Bindings:      12      
  # of rules:    4       
  Max # states:  140     
  States:        12      
  Transitions:   16      
  Occurrences:   20      
  Type:          BRS
  Bindings:      8       
  # of rules:    3       
  Max # states:  140     
  States:        2       
  Transitions:   2       
  Occurrences:   3       
  Type:          BRS
  Bindings:      8       
  # of rules:    3       
  Max # states:  140     
  States:        2       
  Transitions:   2       
  Occurrences:   3       
  Type:          BRS
  Bindings:      8       
  # of rules:    3       
  Max # states:  140     
  States:        2       
  Transitions:   2       
  Occurrences:   3       
  Type:          BRS
  Bindings:      8       
  # of rules:    3       
  Max # states:  140     
  States:        2       
  Transitions:   2       
  Occurrences:   3       
  Type:          Probabilistic BRS
  Bindings:      12      
  # of rules:    3       
  Max # states:  140     
  States:        142     
  Transitions:   376     
  Occurrences:   411     
  Type:          Probabilistic BRS
  Bindings:      12      
  # of rules:    3       
  Max # states:  140     
  States:        142     
  Transitions:   376     
  Occurrences:   411     
  Type:          Probabilistic BRS
  Bindings:      12      
  # of rules:    3       
  Max # states:  140     
  States:        142     
  Transitions:   376     
  Occurrences:   411     
  Type:          Probabilistic BRS
  Bindings:      12      
  # of rules:    3       
  Max # states:  140     
  States:        142     
  Transitions:   376     
  Occurrences:   411     
  Type:          Probabilistic BRS
  Bindings:      16      
  # of rules:    4       
  Max # states:  140     
  States:        142     
  Transitions:   376     
  Occurrences:   411     
  Type:          Probabilistic BRS
  Bindings:      16      
  # of rules:    4       
  Max # states:  140     
  States:        142     
  Transitions:   376     
  Occurrences:   411     
  Type:          Probabilistic BRS
  Bindings:      16      
  # of rules:    4       
  Max # states:  140     
  States:        142     
  Transitions:   376     
  Occurrences:   411     
  Type:          Probabilistic BRS
  Bindings:      16      
  # of rules:    4       
  Max # states:  140     
  States:        142     
  Transitions:   376     
  Occurrences:   411     
  Type:          Nondeterministic BRS
  Bindings:      52      
  # of rules:    85      
  Max # states:  140     
  States:        142     
  Transitions:   222     
  Occurrences:   444     
  Type:          Nondeterministic BRS
  Bindings:      52      
  # of rules:    85      
  Max # states:  140     
  States:        142     
  Transitions:   222     
  Occurrences:   444     
  Type:          Nondeterministic BRS
  Bindings:      52      
  # of rules:    85      
  Max # states:  140     
  States:        142     
  Transitions:   222     
  Occurrences:   444     
  Type:          Nondeterministic BRS
  Bindings:      52      
  # of rules:    85      
  Max # states:  140     
  States:        142     
  Transitions:   222     
  Occurrences:   444     
  Type:          Probabilistic BRS
  Bindings:      8       
  # of rules:    2       
  Max # states:  140     
  States:        4       
  Transitions:   6       
  Occurrences:   12      
  Type:          Probabilistic BRS
  Bindings:      8       
  # of rules:    2       
  Max # states:  140     
  States:        4       
  Transitions:   6       
  Occurrences:   12      
  Type:          Probabilistic BRS
  Bindings:      8       
  # of rules:    2       
  Max # states:  140     
  States:        4       
  Transitions:   6       
  Occurrences:   12      
  Type:          Probabilistic BRS
  Bindings:      8       
  # of rules:    2       
  Max # states:  140     
  States:        4       
  Transitions:   6       
  Occurrences:   12      
  Type:          Nondeterministic BRS
  Bindings:      13      
  # of rules:    4       
  Max # states:  140     
  States:        3       
  Transitions:   4       
  Occurrences:   4       
  Type:          Nondeterministic BRS
  Bindings:      13      
  # of rules:    4       
  Max # states:  140     
  States:        3       
  Transitions:   4       
  Occurrences:   4       
  Type:          Nondeterministic BRS
  Bindings:      13      
  # of rules:    4       
  Max # states:  140     
  States:        3       
  Transitions:   4       
  Occurrences:   4       
  Type:          Nondeterministic BRS
  Bindings:      13      
  # of rules:    4       
  Max # states:  140     
  States:        3       
  Transitions:   4       
  Occurrences:   4       
  Type:          Stochastic BRS
  Bindings:      7       
  # of rules:    2       
  Max # states:  140     
  States:        5       
  Transitions:   4       
  Occurrences:   20      
  Type:          Stochastic BRS
  Bindings:      7       
  # of rules:    2       
  Max # states:  140     
  States:        5       
  Transitions:   4       
  Occurrences:   20      
  Type:          Stochastic BRS
  Bindings:      7       
  # of rules:    2       
  Max # states:  140     
  States:        5       
  Transitions:   4       
  Occurrences:   20      
  Type:          Stochastic BRS
  Bindings:      7       
  # of rules:    2       
  Max # states:  140     
  States:        5       
  Transitions:   4       
  Occurrences:   20      






