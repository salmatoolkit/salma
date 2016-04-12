testcase(distance_1) :-
        F = exists(r : robot,
                   or(
                         and(
                                % matches at step 2 where rob1 has position (12, 10)
                                dist_from_origin(r) > 15.6,
                                dist_from_origin(r) < 15.7
                            ),
                         and(
                                % matches at step 6 where rob2 has position (16, 20)
                                dist_from_origin(r) > 25.6,
                                dist_from_origin(r) < 25.7
                            )                     
                     )),
       runTest(F, 20, 
               [], 
               
               [expect(0, 1, not_ok),
                expect(2, 2, ok),
                expect(3, 5, not_ok),
                expect(6, 6, ok),
                expect(7, 20, not_ok)],
               
               [expect(0, 20, [])]). 

