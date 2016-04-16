:- ['../../ecl-src-instrumented/agasmc'].
:- ['domaindesc'].
:- ['test-suite-tools'].
:- ['tests_until'].
:- ['tests_always_eventually'].
:- ['tests_quantifiers'].
:- ['tests_calculations'].


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




simulate(F, Steps, Events, LogfilePath, Verbose) :-
        init,
        register_property(f, F, _),
        open(LogfilePath, write, Stream),
        print(Stream, "Step;DeltaT;Verdict;LenSched;LenPending\n"),
        (count(I, 0, Steps), param(Events, Stream, Verbose) do
            findall(E, member(ev(I, E), Events), ToProgress),
            (ToProgress \= [] -> progress(ToProgress) ; true),
            loggedEvstep(Stream, 1, [VerdictNow : f], _, DTime),
            printf("\n\n%    d (%.4f s) =  %w\n", [I, DTime,
                                                   VerdictNow]),
            (Verbose = true ->
                print_scheduled_goals(stdout,4),
                print("\n-------\n"),
                print_formula_cache(stdout),
                print("\n-------\n"),
                print_cache_candidates(stdout)
            ;
                true
            ),
            flush(stdout),
            flush(Stream)
        
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
        printf(FileHandle, "%d;%f;%w;%d;%d\n",[T, DTime, VerdictNow, L1, L2]),
        
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

simulate_3 :-
        F = forall(r:robot,
                   forall(j:item,                        
                          implies(occur(grab(r, j)),
                                  eventually(50, xpos(r) >  3000)))),
        (fromto([], EvIn, EvOut, Events), count(Step, 0, 10) do 
            (Step mod 2 =:= 0 ->
                E2 = [ev(Step, grab(rob1, item1)), 
                      ev(Step, grab(rob2, item2))]
            ;
                E2 = [ev(Step, drop(rob1, item1)), 
                      ev(Step, drop(rob2, item2))]
            ),
            append(EvIn, E2, EvOut)
        ),
        simulate(F, 1000, Events, 'sim3.csv', false).