create_until_formula_two_robots(MaxT, TargetX, F) :-
        F = forall(r:robot,
                   forall(j:item,
                          implies(
                                     occur(grab(r,j)),
                                     until(MaxT,
                                           carrying(r, j),
                                           xpos(r) > TargetX
                                          )
                                 )
                         )
                  ).


testcase(until_ok_two_robots) :-
        create_until_formula_two_robots(15, 20, F),
        runTest(F, 20, 
                [ev(0, grab(rob1, item1)), 
                 ev(3, grab(rob2, item2))], 
                [expect(0, 0, nondet),
                 expect(1, 2, ok),
                 expect(3, 3, nondet),
                 expect(4, 20, ok)],
                [expect(1, 3, [s(0,0) : nondet]),
                 expect(4, 10, [s(0,0) : nondet, s(3,3) : nondet]),
                 expect(11, 11, [s(0,0) : ok, s(3,3) : ok]),
                 expect(12, 20, [])]).

testcase(until_not_ok_one_robot_drop) :-
        create_until_formula_two_robots(15, 20, F),
        runTest(F, 20, 
                [ev(0, grab(rob1, item1)), 
                 ev(3, grab(rob2, item2)),
                 ev(5, drop(rob1, item1))], 
                
                [expect(0, 0, nondet),
                 expect(1, 2, ok),
                 expect(3, 3, nondet),
                 expect(4, 10, ok)],
                
                [expect(1, 3, [s(0,0) : nondet]),
                 expect(4, 4, [s(0,0) : nondet, s(3,3) : nondet]),
                 expect(5, 5, [s(0,0) : not_ok, s(3,3) : nondet]),
                 expect(6, 10, [s(3,3) : nondet]),
                 expect(11, 11, [s(3,3) : ok]),
                 expect(12, 20, [])
                ]).

testcase(until_not_ok_timeout) :-
        create_until_formula_two_robots(15, 100, F),
        runTest(F, 20, 
                [ev(0, grab(rob1, item1)), 
                 ev(3, grab(rob2, item2))], 
                
                [expect(0, 0, nondet),
                 expect(1, 2, ok),
                 expect(3, 3, nondet),
                 expect(4, 20, ok)],
                
                [expect(1, 3, [s(0,0) : nondet]),
                 expect(4, 14, [s(0,0) : nondet, s(3,3) : nondet]),
                 expect(15, 15, [s(0,0) : not_ok, s(3,3) : nondet]),
                 expect(16, 17, [s(3,3) : nondet]),
                 expect(18, 18, [s(3,3) : not_ok]),
                 expect(19, 20, [])
                ]).


