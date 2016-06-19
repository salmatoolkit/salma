:- dynamic testcase/1.
:- local store(coverage_map).

reset_coverage :-
        store_erase(coverage_map).

logdec(DecisionId) :-
        store_inc(coverage_map, d(DecisionId)).


get_coverage_count(DecisionId, Count) :-
        (store_get(coverage_map, d(DecisionId), Count) ->
            true
        ;
            Count = -1
        ).

report_coverage(PositionsFile) :-
        open(PositionsFile, read, Stream),
        get_position_entries(Stream, PositionEntries),
        close(Stream),
        length(PositionEntries, NumDecTotal),
        (foreach(PEntry, PositionEntries), fromto([], MissedIn, MissedOut, Missed) do
            PEntry = d(DecisionId, _, _),
            get_coverage_count(DecisionId, Count),
            (Count =< 0 ->
                append(MissedIn, [PEntry], MissedOut)
            ;
                MissedOut = MissedIn
            )
        ),
        length(Missed, NumDecMissed),
        print("Missed:\n---------\n\n"),
        (foreach(MissedEntry, Missed) do
            MissedEntry = d(DecisionId, FilePath, LineNo),
            printf("%d: line %d in %s\n", [DecisionId, LineNo, FilePath])
        ),
        printf("\nMissed %d of %d decisions.\n", [NumDecMissed, NumDecTotal]),
        Coverage is (NumDecTotal - NumDecMissed) / NumDecTotal,
        printf("Coverage = %.2f\n", [Coverage]).

get_position_entries(Stream, Entries) :-
        (read_string(Stream, end_of_line, _, Line) ->
            split_string(Line, ";", "", Columns),
            Columns = [DecIdStr, FilePath, LineNoStr],
            number_string(DecId, DecIdStr),
            number_string(LineNo, LineNoStr),
            Entries = [d(DecId, FilePath, LineNo) | NextEntries],
            get_position_entries(Stream, NextEntries)
        ;
            Entries = []
        ).

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
            extract_schedule_entries(Sched, ExtractedScheduleEntries),
            (foreach(Exp, ExpectationsSchedule), param(I, ExtractedScheduleEntries) do 
                Exp = expect(Start, End, ExpectedGoals),
                
                (I >= Start, I =< End ->
                 (length(ExtractedScheduleEntries, Len1), length(ExpectedGoals, Len2),
                  Len1 =:= Len2 ->
                     true ; length(ExtractedScheduleEntries, Len1), length(ExpectedGoals,
                                                        Len2), throw(wrong_number_of_goals_in_schedule(I,
                                                                                                       Len2, Len1))),
                 (foreach(EG, ExpectedGoals), param(ExtractedScheduleEntries, I) do
                     (existsInSchedule(ExtractedScheduleEntries, EG) -> true
                     ;
                       throw(expected_goal_not_found(I, EG, ExtractedScheduleEntries))
                     )
                 )
                ; 
                 true
                )
                 
            )
        
        ).

extract_schedule_entries(Sched, Extracted) :-
        (foreach(SEntry, Sched), fromto([], E1, E2, Extracted) do
            SEntry = r(_, _, Entries, _),
            append(E1, Entries, E2)
            ).   

existsInSchedule(Sched, GoalSpec) :-
         % member(r(_, _, [GoalSpec], _), Sched).
        member(GoalSpec, Sched).



run_all_tests :-
        reset_coverage,
        findall(TestCase, clause(testcase(TestCase), _), TestCases),
        print(TestCases), nl,
        (foreach(TestCase, TestCases), foreach(Res, Results), 
         fromto(0, FailCounterIn, FailCounterOut, FailCounter)  do
            run_testcase(TestCase, Outcome : Exception),
            Res = r(TestCase, Outcome, Exception),
            (Outcome = success ->
                FailCounterOut = FailCounterIn
            ;
                FailCounterOut is FailCounterIn + 1
            )
        ),
        print(Results), nl,
        report_coverage('../../ecl-src-instrumented/positions.csv'),
        TotalCounter is length(Results),
        SuccessCounter is length(Results) - FailCounter,
        printf("Total test cases: %d\nSucess: %d\nFailure: %d\n", 
               [TotalCounter, SuccessCounter, FailCounter]),
        FailCounter =:= 0.


run_testcase(TestCase, Result) :-
        shelf_create(res/1, success : none, Shelf),
        (
            catch(testcase(TestCase), 
                  Exception,
                  shelf_set(Shelf, 1, failure : Exception)), !
        ;
            shelf_set(Shelf, 1, failure : none)
        ),
        shelf_get(Shelf, 1, Result).