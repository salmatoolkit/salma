testcase(tests_complement_op) :-
        F = complement(xpos(rob1) > xpos(rob2)),
        compile_formula(F, F2),
        F2 = c_([xpos(rob1, X1, s0), xpos(rob2, X2, s0), X1 =< X2]).

testcase(tests_complement_and) :-
        F = complement(and(xpos(rob1) > 0, xpos(rob2) > 0)),
        compile_formula(F, F2),
        F2 = one([c_([xpos(rob1, X1, s0), X1 =< 0]), 
                  c_([xpos(rob2, X2, s0), X2 =< 0])]).

testcase(tests_complement_or) :-
        F = complement(or(xpos(rob1) > 0, xpos(rob2) > 0)),
        compile_formula(F, F2),
        F2 = all([c_([xpos(rob1, X1, s0), X1 =< 0]), c_([xpos(rob2, X2, s0), X2 =< 0])]).
        
testcase(tests_complement_not) :-
        F = complement(not(xpos(rob1) > 0)),
        compile_formula(F, F2),
        F2 = c_([xpos(rob1, X, s0), X > 0]).


testcase(tests_complement_default) :-
        F = complement(carrying(rob1, item1)),
        compile_formula(F, F2),
        F2 = not2(c_([carrying(rob1, item1, s0)])).
