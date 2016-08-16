testcase(cache_formula_toplevel_null) :-
        F = (xpos(rob1) > 0),
        compile_formula(F, F2),
        cache_formula(null, 0, F2, _).

testcase(clean_all) :-
        reset_smc,
        stored_keys_and_values(scheduled_goals, L),
        L =[].