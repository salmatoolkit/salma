testcase(unambiguous_result) :-
        get_unanimous_result([s(0,0) : ok, s(1,1) : not_ok],
                             ambiguous).

testcase(intersection_inf) :-
        get_interval_intersection(s(0,100), inf, inf, s(0,100), []),
        get_interval_intersection(s(0,100), inf, 200, s(0,100), []),
        get_interval_intersection(s(0,100), inf, 50, s(0,50), [s(51,100)]),
        get_interval_intersection(s(50,100), inf, 20, none, [s(50,
                                                               100)]),
        get_interval_intersection(s(50,100), 20, inf, s(50,100), []),
        get_interval_intersection(s(0,100), 20, inf, s(20,100), [s(0,
                                                                   19)]),
        get_interval_intersection(s(0,50), 80, inf, none, [s(0,50)]),
        get_interval_intersection(s(0,100), 20, 110, s(20,100),
                                  [s(0,19)]),
        get_interval_intersection(s(0,100), 20, 50, s(20,50), 
                                  [s(0,19), s(51, 100)]),
        get_interval_intersection(s(50,100), 20, 80, s(50,80), 
                                  [s(81,100)]).

testcase(first_interval_within) :-
        L = [s(10,20), s(30,40)],
        
        get_first_interval_within(L, 50,100, none, L),
        get_first_interval_within(L, 5,25, s(10,20), [s(30,40)]),
        get_first_interval_within(L, 25,45, s(30,40), [s(10,20)]),
        get_first_interval_within(L, 5,45, s(10, 20), [s(30,40)]),
        get_first_interval_within(L, 5, 15, s(10,15), [s(16,20), s(30,
                                                                   40)]).

testcase(right_bound_continuous_intersection) :-
        L = [s(10,20), s(50,100)],
        get_right_bound_continuous_intersection(L, 110, none),
        get_right_bound_continuous_intersection(L, 100, s(50,100)),
        get_right_bound_continuous_intersection(L, 90, s(50,90)).
        

testcase(max_interval_point_within) :-
        L = [s(10,20), s(50,100)],
        get_max_interval_point_within(L, 0, 110, 100),
        get_max_interval_point_within(L, 0, 90, 90),
        get_max_interval_point_within(L, 110, 200, none),
        get_max_interval_point_within(L, 0, 40, 20), 
        get_max_interval_point_within(L, 15, 90, 90).