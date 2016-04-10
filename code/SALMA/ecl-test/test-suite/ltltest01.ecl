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

until_not_ok_timeout :-
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



               
create_eventually_formula_two_robots_with_var(MaxT, F) :-
        F = forall(r:robot,
                   forall(j:item,                        
                          implies(occur(grab(r,j)),
                                  let(maxX : xpos(r) + 15,
                                      eventually(MaxT, xpos(r) > maxX))))).
 
eventually_var_ok :- 
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
 
eventually_ok_1 :- 
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
 
nested_always_eventually_ok_1 :- 
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




simulate(F, Steps, Events, LogfilePath) :-
        init,
        register_property(f, F, _),
        open(LogfilePath, write, Stream),
        (count(I, 0, Steps), param(Events, Stream) do
            findall(E, member(ev(I, E), Events), ToProgress),
            (ToProgress \= [] -> progress(ToProgress) ; true),
            loggedEvstep(Stream, 1, [VerdictNow : f], _, DTime),
            printf("%    d (%.4f s) =  %w\n", [I, DTime, VerdictNow])
        ),
        close(Stream).


evstep :-
        evstep(1, false, _, _).

evstep(TimeDelta) :-
        evstep(TimeDelta, false, _, _).


loggedEvstep(FileHandle, TimeDelta, ToplevelResults,
             ScheduledResults, DTime) :-
        current_time(T),
        T2 is T + TimeDelta - 1,
        statistics(hr_time, STime1),
        evaluation_step(T2, ToplevelResults, ScheduledResults,
                        PendingGoals, _),
        statistics(hr_time, STime2),
        DTime is STime2 - STime1,
        length(ScheduledResults, L1),
        length(PendingGoals, L2),
        ToplevelResults = [VerdictNow : f],
        printf(FileHandle, "%d;%.4f;%w;%d;%d\n",[T, DTime, VerdictNow, L1, L2]),
        
        moveAll,
        progress([tick(TimeDelta)]).


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




run_all_tests :-
        until_ok_two_robots,
        until_not_ok_one_robot_drop,
        until_not_ok_timeout,
        eventually_var_ok,
        eventually_ok_1,
        nested_always_eventually_ok_1.

simulate_1 :-
        F = forall(r:robot,
                   forall(j:item,                        
                          implies(carrying(r, j),
                                  let(maxX : xpos(r) + 2000,
                                      eventually(2000, xpos(r) >
                                                             maxX))))),
        simulate(F, 1000, [ev(0, grab(rob1, item1)), 
                 ev(0, grab(rob2, item2))], 'sim1.csv').
        

simulate_2 :-
        F = forall(r:robot,
                   forall(j:item,                        
                          implies(carrying(r, j),
                                  eventually(2000, xpos(r) >  3000)))),
        simulate(F, 1000, [ev(0, grab(rob1, item1)), 
                 ev(0, grab(rob2, item2))], 'sim2.csv').