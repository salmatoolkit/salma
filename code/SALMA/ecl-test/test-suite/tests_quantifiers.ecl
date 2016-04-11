run_exists_test_sequence(F) :-
        runTest(F, 20, 
                [ev(5, grab(rob1, item1)), 
                 ev(7, grab(rob2, item2)),
                 ev(10, drop(rob1, item1)),
                 ev(15, drop(rob2, item2))], 
                
                [expect(0, 4, not_ok),
                 expect(5, 14, ok),
                 expect(15, 20, not_ok)],
                
                [expect(0, 20, [])]).
        

testcase(exists_ok_1) :-
        F = exists(r : robot,
                   exists(i : item,
                          carrying(r, i))),
        run_exists_test_sequence(F).

testcase(exists_ok_1_oldformat) :-
        F = exists([r, robot],
                   exists([i, item],
                          carrying(r, i))),
        run_exists_test_sequence(F).


run_forall_test_sequence(F) :-
        runTest(F, 20, 
                [ev(5, grab(rob1, item1)), 
                 ev(7, grab(rob2, item2)),
                 ev(10, drop(rob1, item1)),
                 ev(15, drop(rob2, item2))], 
                
                [expect(0, 6, not_ok),
                 expect(7, 9, ok),
                 expect(10, 20, not_ok)],
                
                [expect(0, 20, [])]).

testcase(forall_ok_1) :-
        F = forall(r : robot, exists(i : item, carrying(r, i))),
        run_forall_test_sequence(F).

testcase(forall_ok_1_oldformat) :-
        F = forall([r, robot], exists([i, item], carrying(r, i))),
        run_forall_test_sequence(F).

        
                                     