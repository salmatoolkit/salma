testcase(marking_comparison_wildcard) :-
        F = (marking(item1) = t_(m(a, ?))),
        runTest(F, 10, 
                [ev(3, mark(rob1, item1, m(a, 42))),
                 ev(5, mark(rob2, item1, m(b, 42)))
                ],
                [expect(0, 2, not_ok),
                 expect(3, 4, ok),
                 expect(5, 10, not_ok)],
                [expect(0,10, [])]).

testcase(hasMarking_wildcard) :-
        F = hasMarking(item1, a, ?),
        runTest(F, 10, 
                [ev(3, mark(rob1, item1, m(a, 42))),
                 ev(5, mark(rob2, item1, m(b, 42)))
                ],
                [expect(0, 2, not_ok),
                 expect(3, 4, ok),
                 expect(5, 10, not_ok)],
                [expect(0,10, [])]).

testcase(gravity_check) :-
        F = (get_earth_gravity =:= gravity),
        runTest(F, 1, [], [expect(0, 1, ok)], []).

testcase(use_standard_predicate) :-
        F =  number(xpos(rob1)),
        runTest(F, 1, [ev(0, join(rob1, rob2))], [expect(0, 1, ok)], []).