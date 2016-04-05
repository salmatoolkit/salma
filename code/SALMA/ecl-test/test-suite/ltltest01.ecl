:- ['../../ecl-src/agasmc'].
:- ['../domaindesc'].

init :- 
        init_agasmc,
        setDomain(robot, [rob1, rob2]),
        setDomain(item, [item1, item2]),
        init_sort_hierarchy(_),
        set_current(xpos, [rob1], 10),
        set_current(ypos, [rob1], 10),
        set_current(xpos, [rob2], 10),
        set_current(ypos, [rob2], 20),
        set_current(marking, [item1], 0),
        set_current(marking, [item2], 0),
        set_current(vx, [rob1], 0),
        set_current(vy, [rob1], 0),
        set_current(vx, [rob2], 0),
        set_current(vy, [rob2], 0),
        domain(robot, D1),
        domain(item, D2),
        (foreach(R, D1), param(D2) do
            setConstant(robot_radius, [R, 10]),
            (foreach(I, D2), param(R) do
                set_current(carrying, [R, I], false)
            ),
            set_current(partner, [R], none)
        ),
        set_current(time, [], 0).


grabAll :-
        L = [grab(rob1, item1), grab(rob2, item2)],
        progress(L).

dropAll :-
        L = [drop(rob1, item1), drop(rob2, item2)],
        progress(L).

moveAll :-
        L = [move_right(rob1), move_right(rob2), finish_step(rob1), finish_step(rob2)],
        progress(L).

assertEquals(Expected, Actual) :-
        Expected = Actual, ! 
        ;
        throw(assert_equal_failed(Expected, Actual)).

runTest(F, Steps, Events, ExpectationsVerdict, ExpectationsSchedule) :-
        init,
        register_property(f, F, _),
        (count(I, 0, Steps), param(Events, ExpectationsVerdict, ExpectationsSchedule) do
            findall(E, member(ev(I, E), Events), ToProgress),
            (ToProgress \= [] -> progress(ToProgress) ; true),
            evstep(1, false, [VerdictNow : f], Sched),
            printf("%    d =  %w - %w\n", [I, VerdictNow, Sched]),
            (foreach(Exp, ExpectationsVerdict), param(I, VerdictNow) do 
                Exp = expect(Start, End, ExpectedVerdict),
                (I >= Start, I =< End ->
                 (ExpectedVerdict = VerdictNow -> true
                 ;
                 throw(unexpected_verdict(I, VerdictNow, ExpectedVerdict))
                 )
                ;
                 true
                )
            ),
            (foreach(Exp, ExpectationsSchedule), param(I, Sched) do 
                Exp = expect(Start, End, ExpectedGoals),
                
                (I >= Start, I =< End ->
                 (length(Sched, Len1), length(ExpectedGoals, Len2),
                  Len1 =:= Len2 ->
                     true ; length(Sched, Len1), length(ExpectedGoals,
                                                        Len2), throw(wrong_number_of_goals_in_schedule(I,
                                                                                                       Len2, Len1))),
                 (foreach(EG, ExpectedGoals), param(Sched, I) do
                     (existsInSchedule(Sched, EG) -> true
                     ;
                      throw(expected_goal_not_found(I, EG, Sched))
                     )
                 )
                ; 
                 true
                )
                 
            )
        
        ).

existsInSchedule(Sched, GoalSpec) :-
        member(r(_, _, [GoalSpec], _), Sched).

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
       

until_ok_two_robots :-
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

until_not_ok_one_robot_drop :-
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
      
pstep(Num) :-
        Period = 8,
        Delta = 3,
        Start = 0,
        (count(_, 1, Num), param(Period, Delta, Start) do
            current_time(T),
            printf("T = %d\n", [T]),
            M is mod(T - Start, Period), 
            (M =:= 0,
             grabAll, print("  Grab\n"), !
            ; M =:= Delta,
              dropAll, print("  Drop\n"), !
            ; true
            ),
            evstep
        ).




evstep :-
        evstep(1, false, _, _).

evstep(TimeDelta) :-
        evstep(TimeDelta, false, _, _).

evstep(TimeDelta, Verbose, ToplevelResults, ScheduledResults) :-
        current_time(T),
        T2 is T + TimeDelta -1,
        evaluation_step(T2, ToplevelResults, ScheduledResults,
                        PendingGoals, FailureStack),
        
        printf("%d : %w %w %w %w\n",[T,ToplevelResults,
                                     ScheduledResults, PendingGoals,
                                     FailureStack]),
        (Verbose = true ->  
            print_scheduled_goals(stdout,4),
            print("\n-------\n"),
            print_formula_cache(stdout),
            print("\n-------\n"),
            print_cache_candidates(stdout)
        ;
            true
        ),
        moveAll,
        progress([tick(TimeDelta)]).

maxxpos(XMax) :-
        domain(robot, Robots, s0),
        member(R, Robots),
        xpos(R, XMax, s0),
        not (
                member(R2, Robots),
                R \= R2,
                xpos(R2, XR2, s0),
                XR2 > XMax
            ), !.

