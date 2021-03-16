Test bigraph library
  $ ./test_match.exe ./files
  $ ./test_inst.exe
  $ ./test_decomp.exe ./files
  $ ./test_brs.exe
  brs
  States: 30
  Transitions: 29
  Occurrences: 4495
  sim_brs
  States: 30
  Transitions: 29
  Occurrences: 4495
  sbrs
  States: 30
  Transitions: 29
  Occurrences: 4495
  sim_sbrs
  States: 30
  Transitions: 29
  Occurrences: 4495
  $ ./test_bug_savannah.exe
  Occurrences:
  {(0, 4), (1, 20), (2, 24), (3, 22), (4, 23)}, {(0, 8)}, {(1, 0), (2, 3), (3, 7)}
  $ ./test_bug_spec.exe
  s1:
  {(0, R:0),(1, T:0),(2, T:0),(3, R:0),(4, M:2),(5, M:2),(6, M:2),(7, M:2),(8, ND:5)}
  2 9 0
  110000000
  000100000
  000011000
  001000000
  000000110
  000000001
  000000000
  000000000
  000000000
  000000000
  000000000
  ({}, {a}, {(5, 1), (6, 1), (8, 1)})
  ({}, {b}, {(4, 1), (5, 1), (8, 1)})
  ({}, {c}, {(8, 1)})
  ({}, {d}, {(6, 1), (7, 1), (8, 1)})
  ({}, {x}, {(4, 1), (7, 1), (8, 1)})
  r:
  {(0, M:2),(1, M:2),(2, M:2),(3, M:2),(4, ND:5)}
  3 5 0
  11000
  00110
  00001
  00000
  00000
  00000
  00000
  00000
  ({}, {a}, {(0, 1), (3, 1), (4, 1)})
  ({}, {b}, {(0, 1), (1, 1), (4, 1)})
  ({}, {c}, {(1, 1), (2, 1), (4, 1)})
  ({}, {d}, {(2, 1), (3, 1), (4, 1)})
  ({}, {x}, {(4, 1)})
  true
  true
  $ ./debug_ksat.exe
  Parsing ./files/T19.big
  Parsing ./files/P28.big
  Occurrence 0: {{(0, 0)},
                 {},
                 {(0, 0)}}
  Occurrence 1: {{(0, 4)},
                 {},
                 {(0, 2)}}
  Occurrence 2: {{(0, 5)},
                 {},
                 {(0, 3)}}
