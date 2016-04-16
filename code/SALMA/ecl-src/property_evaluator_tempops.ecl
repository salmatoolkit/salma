% evaluates until(MaxTime, P, Q) 
% -the inspected interval is determined as min(StartTime + MaxTime, EndTime)
evaluate_until(ToplevelFormula, FormulaPath, 
               CurrentStep, Level, StartTimes, EndTime, MaxTime, P, Q, 
               Results, OverallResult, NewP, NewQ, ScheduleParams, HasChanged) :-
        /* $D$ */ 
        NextLevel is Level + 1,
        last_element(StartTimes, s(_, MaxStartTime)),
        Deadline is MaxStartTime + MaxTime,
        % the end of the interval that is to be inspected
        
        time(CurrentTime, do2(tick(CurrentStep), s0)),
        
        getval(current_failure_stack, CFS),
        record_create(MyFailures),
        setval(current_failure_stack, MyFailures),
        % pqres(NewP, NewQ, Result, HasChanged, ScheduleParams)
        shelf_create(pqres/5, null, Shelf),
        
        shelf_set(Shelf, 1, P),
        shelf_set(Shelf, 2, Q),
        % changed = false
        shelf_set(Shelf, 4, false),
        %schedule params
        shelf_set(Shelf, 5, []),
        
        append(FormulaPath, [2], SubPathP), % start with subterm 2 since first is max time
        append(FormulaPath, [3], SubPathQ),
        %SubPathP = [0, 2],
        %SubPathQ = [0, 3],
        (Q = sched(_, QSchedIdIn, QRefTerm) -> 
            /* $D$ */ 
            (QRefTerm = cf(QCacheId) ->
                /* $D$ */ 
                get_cached_formula(QCacheId, SubQ)
            ;
                /* $D$ */ 
                SubQ = QRefTerm,
                QCacheId = -1
            )
        ;
            /* $D$ */ 
            SubQ = Q,
            QSchedIdIn = new,
            QCacheId = new
        ),
        (P = sched(_, PSchedIdIn, PRefTerm) -> 
            /* $D$ */ 
            (PRefTerm = cf(PCacheId) ->
                /* $D$ */ 
                get_cached_formula(PCacheId, SubP)
            ;
                /* $D$ */ 
                SubP = PRefTerm,
                PCacheId = -1
            )
        ;
            /* $D$ */ 
            SubP = P,
            PSchedIdIn = new,
            PCacheId = new
        ),	
        
        
        % check from current time to end of interval
        % ignore result for now since it will be examined later together with previously
        % scheduled results
        
        IntervalEnd is min(Deadline, EndTime),
        
        evaluate_for_all_timesteps(ToplevelFormula, [0], 
                                   eventually, CurrentStep, CurrentTime, IntervalEnd, SubQ, NextLevel, _, 
                                   QSchedIdIn, QCacheId,
                                   QSchedId, ToScheduleQ,
                                   QEarliestDefinite, _, _, _, _),	
        
        % if we found an ok, we only have to check P up to that point
        PIntervalEnd is getMin(QEarliestDefinite, IntervalEnd),
        
        evaluate_for_all_timesteps(ToplevelFormula, [0], 
                                   always, CurrentStep, CurrentTime, PIntervalEnd, SubP, NextLevel, _, 
                                   PSchedIdIn, PCacheId,
                                   PSchedId, ToScheduleP,
                                   _, _, _, _, _),	
        
        % now all referent results have been gathered in the schedule
        check_schedule_for_interval_until(PSchedId, QSchedId, NextLevel, StartTimes,
                                          MaxTime, Results1, UnhandledStartTimes),
        
        apply_unique_result(UnhandledStartTimes, nondet, Results2),
        append(Results1, Results2, UnsortedResults),
        sort([1, 1], =<, UnsortedResults, Results),
        
        get_unanimous_result(Results, OvRes),
        
        shelf_set(Shelf, 3, OvRes),		
        
        (PSchedId \= -1 ->
            /* $D$ */ 
            KeyP =.. [p, SubPathP],
            var(VarPSchedId),
            SParams1 = [KeyP : PSchedId],
            shelf_set(Shelf, 1, sched(KeyP, VarPSchedId, ToScheduleP)) 
        ; 
            /* $D$ */ 
            SParams1 = []
        ),
        (QSchedId \= -1 ->
            /* $D$ */ 
            KeyQ =.. [p, SubPathQ],
            var(VarQSchedId),
            append(SParams1, [KeyQ : QSchedId], SParams2),
            shelf_set(Shelf, 2, sched(KeyQ, VarQSchedId, ToScheduleQ)) 
        ; 
            /* $D$ */ 
            SParams2 = SParams1
        ),
        ( (QSchedIdIn \= QSchedId, /* $D$ */ !
          ; PSchedIdIn \= PSchedId /* $DS$ */ ) -> 
            shelf_set(Shelf, 4, true) ; true), % has changed
        shelf_set(Shelf, 5, SParams2),
        % retrieve all output values from shelf
        shelf_get(Shelf, 0, pqres(NewP, NewQ, OverallResult, HasChanged, ScheduleParams)),
        shelf_abolish(Shelf),
        getval(negated, Negated),
        ((Negated = 0, OverallResult = not_ok ; Negated = 1, OverallResult = ok) ->
            recorded_list(MyFailures, MFs),
            (foreach(MF, MFs), param(CFS) do
                record(CFS, MF)
            )
        ;
            true
        ),
        setval(current_failure_stack, CFS),
        erase_all(MyFailures).


		
		
% evaluates always(MaxTime, P) or eventually(MaxTime, P) depending on Mode
% - the inspected interval is determined as min(StartTime + MaxTime, EndTime)
evaluate_always_or_eventually(ToplevelFormula, FormulaPath, Mode,
                              CurrentStep, Level, StartTimes, EndTime, MaxTime, P, 
                              Results, OverallResult, ToSchedule, ScheduleParams, HasChanged) :-
        /* $D$ */ 
        NextLevel is Level + 1,
        last_element(StartTimes, s(_, MaxStartTime)),
        Deadline is MaxStartTime + MaxTime,
        % the end of the interval that is to be inspected
        IntervalEnd is min(Deadline, EndTime),
        
        time(CurrentTime, do2(tick(CurrentStep), s0)),
        
        shelf_create(orig/1, null, Shelf),
        shelf_set(Shelf,1,P),
        append(FormulaPath, [2], SubPathP),
        (P = sched(_, PSchedIdIn, PRefTerm) ->  
            /* $D$ */ 
            (PRefTerm = cf(PCacheId) ->
                /* $D$ */ 
                get_cached_formula(PCacheId, SubP)
            ;
                /* $D$ */ 
                SubP = PRefTerm,
                PCacheId = -1
            )
        ;
            /* $D$ */ 
            SubP = P,
            PSchedIdIn = -1,
            PCacheId = -1		
        ),
        evaluate_for_all_timesteps(ToplevelFormula, SubPathP, 
                                   Mode, CurrentStep, CurrentTime, IntervalEnd, SubP, NextLevel, Result1, 
                                   PSchedIdIn, PCacheId,
                                   PSchedId, ToScheduleP,
                                   EarliestDefinite, _, _, LastPossible, _),
        % apply current result
        
        (Result1 = ok, Mode = eventually, !, /* $D$ */ 
         % we now can confirm every interval that is not longer than 
         % MaxTime before the ok we found
         LeftBoundary is EarliestDefinite - MaxTime,
         apply_result_within_interval(StartTimes, ok,
                                      LeftBoundary, inf, 
                                      UnhandledStartTimes, ResultsWithoutSchedule)			
        ; Result1 = not_ok, Mode = always, !, /* $D$ */ 
          LeftBoundary is LastPossible - MaxTime,
          apply_result_within_interval(StartTimes, not_ok,
                                       LeftBoundary, inf, 
                                       UnhandledStartTimes, ResultsWithoutSchedule)
        ; 
         /* $D$ */ 
         UnhandledStartTimes = StartTimes,
         ResultsWithoutSchedule = []
        ),
        (length(UnhandledStartTimes) > 0 ->
            /* $D$ */ 
            (PSchedId >= 0 ->
                /* $D$ */ 
                check_schedule_for_interval(PSchedId, NextLevel, UnhandledStartTimes, Mode,
                                            MaxTime, ResultsFromSchedule, _, _,
                                            UnhandledStartTimes2),
                apply_unique_result(UnhandledStartTimes2, nondet, ResultsUnhandled),
                append(ResultsWithoutSchedule, ResultsFromSchedule, RTemp),
                append(RTemp, ResultsUnhandled, ResultsUnsorted),
                sort([1,1], =<, ResultsUnsorted, Results)				
            ; % not scheduled before
              % this means we have no temporal operator in SubP and we can assume
              % that for all previous steps: (always -> ok) /\ (eventually -> not_ok) 
                /* $D$ */ 
                ((Result1 = ok, Mode = always, /* $D$ */ ! ; Result1 = not_ok, Mode = eventually /* $DS$ */ ) ->
                    /* $D$ */ 
                    RightBoundary is IntervalEnd - MaxTime,
                    apply_result_within_interval(UnhandledStartTimes, Result1,
                                                 inf, RightBoundary, 
                                                 UnhandledStartTimes2, ResultsByTimeout)
                ;
                    /* $D$ */ 
                    UnhandledStartTimes2 = UnhandledStartTimes,
                    ResultsByTimeout = []				
                ),
                apply_unique_result(UnhandledStartTimes2, nondet, ResultsUnhandled),
                append(ResultsWithoutSchedule, ResultsUnhandled, ResultsUnsorted1),
                append(ResultsUnsorted1, ResultsByTimeout, ResultsUnsorted),
                sort([1,1], =<, ResultsUnsorted, Results)
            )
        ;
            /* $D$ */ 
            Results = ResultsWithoutSchedule
        ),
        get_unanimous_result(Results, OverallResult),		
        (PSchedIdIn =\= PSchedId ->
            /* $D$ */ 
            HasChanged = true
        ;
            /* $D$ */ 
            HasChanged = false
        ),		
        (OverallResult = nondet -> 
            /* $D$ */ 
            (PSchedId > -1 ->
                /* $D$ */ 
                KeyP =.. [p, SubPathP],
                var(VarPSchedId),
                ScheduleParams = [KeyP : PSchedId],
                ToSchedule = sched(KeyP, VarPSchedId, ToScheduleP)
            ; % is this actually possible?
                /* $D$ */ 
                shelf_get(Shelf, 1, OrigP),
                ScheduleParams = [],
                ToSchedule = OrigP			
            )		
        ;
            /* $D$ */ 
            ToSchedule = OverallResult,
            ScheduleParams= []		
        ),
        shelf_abolish(Shelf).	