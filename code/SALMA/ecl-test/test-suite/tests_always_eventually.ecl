create_eventually_formula_two_robots_with_var(MaxT, F) :-
        F = forall(r:robot,
                   forall(j:item,                        
                          implies(occur(grab(r,j)),
                                  let(maxX : xpos(r) + 15,
                                      eventually(MaxT, xpos(r) > maxX))))).
 
testcase(eventually_var_ok) :- 
        create_eventually_formula_two_robots_with_var(30, F),
        runTest(F, 30, 
                [ev(5, grab(rob1, item1)), 
                 ev(10, grab(rob2, item2))], 
                
                [expect(0, 4, ok),
                 expect(5, 5, nondet),
                 expect(6, 9, ok),
                 expect(10, 10, nondet),
                 expect(11, 30, ok)
                
                ],
                
                 [expect(0, 5, []),
                  expect(6, 10, [s(5,5) : nondet]),
                  expect(11, 20, [s(5,5) : nondet, s(10,10) : nondet]),
                  expect(21, 21, [s(5,5) : ok, s(10,10) : nondet]),
                  expect(22, 25, [s(10,10) : nondet]),
                  expect(26, 26, [s(10,10) : ok]),
                  expect(27, 30, [])]).


create_eventually_formula_two_robots(MaxT, TargetX, F) :-
        F = forall(r:robot,
                   forall(j:item,
                          implies(carrying(r,j),
                                  eventually(MaxT, xpos(r) > TargetX)))).
 
testcase(eventually_ok_1) :- 
        create_eventually_formula_two_robots(30, 25, F),
        runTest(F, 30, 
                [ev(5, grab(rob1, item1)), 
                 ev(10, grab(rob2, item2))], 
                
                [expect(0, 4, ok),
                 expect(5, 15, nondet),
                 expect(16, 30, ok)],
                
                 [expect(0, 5, []),
                  expect(6, 6, [s(5,5) : nondet]),
                  expect(7, 7, [s(5,6) : nondet]),
                  expect(8, 8, [s(5,7) : nondet]),
                  expect(9, 9, [s(5,8) : nondet]),
                  expect(10, 10, [s(5,9) : nondet]),
                  expect(11, 11, [s(5,9) : nondet, s(10,10) : nondet]),
                  expect(12, 12, [s(5,9) : nondet, s(10,11) : nondet]),
                  expect(13, 13, [s(5,9) : nondet, s(10,12) : nondet]),
                  expect(14, 14, [s(5,9) : nondet, s(10,13) : nondet]),
                  expect(15, 15, [s(5,9) : nondet, s(10,14) : nondet]),
                  expect(16, 16, [s(5,9) : ok, s(10,15) : ok]),
                  expect(17, 30, [])]).


create_nested_always_eventually_formula_two_robots(MaxT, TargetX, F) :-
        F = forall(r:robot,
                   forall(j:item,
                          implies(occur(grab(r,j)),
                                  always(MaxT,
                                         and(   
                                                carrying(r, j),
                                                eventually(MaxT, xpos(r) > TargetX)))))).
 
testcase(nested_always_eventually_ok_1) :- 
        create_nested_always_eventually_formula_two_robots(20, 25, F),
        runTest(F, 30, 
                [ev(5, grab(rob1, item1)), 
                 ev(10, grab(rob2, item2))], 
                
                 [expect(0, 4, ok),
                 expect(5, 5, nondet),
                 expect(6, 9, ok),
                 expect(10, 10, nondet),
                 expect(11, 30, ok)],
                
                [expect(0, 5, []),
                 expect(6, 10, [s(5,5) : nondet]),
                 expect(11, 24, [s(5,5) : nondet, s(10,10) : nondet]),
                 expect(25, 25, [s(5,5) : ok, s(10,10) : nondet]),
                 expect(26, 29, [s(10,10) : nondet]),
                 expect(30, 30, [s(10,10) : ok]) ]).


testcase(eventually_mean_dist) :-
        F = eventually(5, mean_dist_from_origin([rob1, rob2]) > 21.5),
         runTest(F, 10, [], [expect(0, 4, nondet), expect(5, 10, ok)],
                 [expect(0, 0, []),
                  expect(1, 1, [s(0,0) : nondet]),
                  expect(2, 2, [s(0,1) : nondet]), 
                  expect(3, 3, [s(0,2) : nondet]),
                  expect(4, 4, [s(0,3) : nondet]),
                  expect(5, 5, [s(0,4) : ok]),
                  expect(6, 10, [])]).