Test PRISM output.
  $ bigrapher full -q -p prob_wsn.tra ../examples/prob_wsn.big
  $ bigrapher full -q -p mdp_wsn.tra ../examples/mdp_wsn.big
  $ bigrapher full -q -p stoch_occ.tra ../examples/stoch_occ.big

  $ cat prob_wsn.tra
  4 6
  0 1 1
  1 0 0.2727
  1 2 0.7273
  2 1 0.6
  2 3 0.4
  3 2 1

  $ cat mdp_wsn.tra
  3 3 4
  0 0 1 0.2 send
  0 0 2 0.8 send
  0 1 0 1 wait
  1 0 0 1 reset

  $ cat stoch_occ.tra
  5 4
  0 1 28
  1 2 15
  2 3 6
  3 4 1

