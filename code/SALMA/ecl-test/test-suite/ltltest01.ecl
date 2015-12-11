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

test_1_ok :-
        init,
        F = forall([r,robot],
                   forall([j,item],
                          implies(
                                     occur(grab(r,j)),
                                     until(15,
                                           carrying(r, j),
                                           xpos(r) > 20
                                          )
                                 )
                         )
                  ),
        register_property(f, F, _),
        grabAll,
        (count(I,0,20) do
            evstep(1, false, [VerdictNow : f], Sched),
            printf("%    d =  %w - %w\n", [I, VerdictNow, Sched]),
            (I = 0 -> assertEquals(nondet, VerdictNow)
            ;
                      assertEquals(ok, VerdictNow)
            ),
            (
                I = 0, assertEquals([], Sched), !
            ;
                I < 11, assertEquals([r(f, 5, [s(0, 0) : nondet], nondet)], Sched), !
            ;
                I = 11, assertEquals([r(f, 5, [s(0, 0) : ok], ok)], Sched), !
            ;
                assertEquals([], Sched)
            )
        ).

test_2_not_ok_one_robot_drop :-
        init,
        F = forall([r,robot],
                   forall([j,item],
                          implies(
                                     occur(grab(r,j)),
                                     until(15,
                                           carrying(r, j),
                                           xpos(r) > 20
                                          )
                                 )
                         )
                  ),
        register_property(f, F, _),
        grabAll,
        (count(I,0,20) do
            (I =:= 5 -> progress( [drop(rob1, item1)] ) ; true),
            
            evstep(1, false, [VerdictNow : f], Sched),
            printf("%d =  %w - %w\n", [I, VerdictNow, Sched]),
            (I =:= 0 -> assertEquals(nondet, VerdictNow)
            ;
                      assertEquals(ok, VerdictNow)
            ),
            (
                I =:= 0, assertEquals([], Sched), !
            ;
                I < 5, assertEquals([r(f, 5, [s(0, 0) : nondet],
                                       nondet)], Sched), !
            ;
                I =:= 5, assertEquals([r(f, 5, [s(0, 0) : not_ok], not_ok)], Sched), !
            ;
                assertEquals([], Sched)
            )
        ).


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

