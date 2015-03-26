% evaluates until(MaxTime, P, Q) 
% -the inspected interval is determined as min(StartTime + MaxTime, EndTime)
evaluate_until(ToplevelFormula, FormulaPath, 
	CurrentStep, Level, StartTime, EndTime, MaxTime, P, Q, 
				Result, NewP, NewQ, ScheduleParams, HasChanged) :-
		NextLevel is Level + 1,
		Deadline is StartTime + MaxTime,
		% the end of the interval that is to be inspected
		IntervalEnd is min(Deadline, EndTime),
		time(CurrentTime, do2(tick(CurrentStep), s0)),
		printlog("U0: CurrentTime=%d CurrentStep=%d, Level=%d, StartTime=%d, EndTime=%d, MaxTime=%d\n",
			[CurrentTime, CurrentStep, Level, StartTime, EndTime, MaxTime]),
		
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
		% also evaluate history: schedule for each time step and store in list. 
		
		append(FormulaPath, [2], SubPathP), % start with subterm 2 since first is max time
		append(FormulaPath, [3], SubPathQ),
		
		(Q = sched(_, QSchedIdIn, QRefTerm) -> 
			(QRefTerm = cf(QCacheId) ->
				get_cached_formula(QCacheId, SubQ)
				;
				SubQ = QRefTerm,
				QCacheId = -1
			)
			;
			SubQ = Q,
			QSchedIdIn = -1,
			QCacheId = -1
		),				
		% check from current time to end of interval
		% ignore result for now since it will be examined later together with previously
		% scheduled results

		evaluate_for_all_timesteps(ToplevelFormula, SubPathQ, 
			eventually, CurrentStep, CurrentTime, StartTime, IntervalEnd, SubQ, NextLevel, _, 
			QSchedIdIn, QCacheId,
			QSchedId, ToScheduleQ,
			QEarliestDefinite1, _, QEarliestPossible1, _),	
		printlog("   Q1: %w - %w\n", [QEarliestDefinite1, QEarliestPossible1]),
		(QSchedId >= 0 ->		
			check_schedule_for_interval(QSchedId, StartTime, IntervalEnd, eventually, _, 
				QEarliestDefinite2, _, QEarliestPossible2, _),
			printlog("   Q2: %d - %w - %w\n", [QSchedId, QEarliestDefinite2, QEarliestPossible2]),
			getMin(QEarliestDefinite1, QEarliestDefinite2, QEarliestDefinite),
			getMin(QEarliestPossible1, QEarliestPossible2, QEarliestPossible)
			;
			QEarliestDefinite = QEarliestDefinite1,
			QEarliestPossible = QEarliestPossible1
		),
		printlog("   Q3: %w - %w\n", [QEarliestDefinite, QEarliestPossible]),
		(			
			
			
		% ------- BEGIN EVALUATE P ----------------------
			( % determine if it's necessary to evaluate P
				(QEarliestPossible = nondet, Deadline > EndTime), !
				;
				(QEarliestPossible \= nondet, 
					QEarliestPossible =< Deadline)
			),
			(QEarliestDefinite = nondet ->
				PEndTime = IntervalEnd
				;
				PEndTime = QEarliestDefinite,
				QEarliestDefinite > StartTime % also don't evaluate P if Q at the beginning
			),
			% Q is at least possible in time
			(P = sched(_, PSchedIdIn, PRefTerm) ->  
				(PRefTerm = cf(PCacheId) ->
					get_cached_formula(PCacheId, SubP)
					;
					SubP = PRefTerm,
					PCacheId = -1
				)
				;
				SubP = P,
				PSchedIdIn = -1,
				PCacheId = -1
			),
			(PSchedIdIn >= 0 ->
				check_schedule_for_interval(PSchedIdIn, StartTime, PEndTime, 
					always, _, 
					_, PLatestDefinite1, _, PLatestPossible1),
				printlog("   P1-A: %d - %w - %w\n", [PSchedIdIn, PLatestDefinite1, PLatestPossible1])
				;
				% P has not been scheduled. If we're ahead StartTime, we wouldn't be here if
				% at any point since StartTime P \= ok
				
				(CurrentTime > StartTime ->
					PLatestDefinite1 is CurrentTime - 1,
					PLatestPossible1 is CurrentTime - 1
					;
					PLatestDefinite1 = nondet,
					PLatestPossible1 = nondet
				),
				printlog("   P1-B: %w - %w\n", [PLatestDefinite1, PLatestPossible1])
			),			
			% now we can safely evaluate again. if current evaluation fails then schedule could be
			% rewritten	to not_ok
			evaluate_for_all_timesteps(ToplevelFormula, SubPathP, 
				always, CurrentStep, CurrentTime, StartTime, PEndTime, 
				SubP, NextLevel, _, 
				PSchedIdIn, PCacheId,
				PSchedId, ToScheduleP,
				_, PLatestDefinite2, _, PLatestPossible2),
			printlog("   P2: %w - %w\n", [PLatestDefinite2, PLatestPossible2]),
			(PLatestDefinite1 = nondet,
				(PSchedIdIn >= 0 ->
					% this means that there has been a schedule that didn't return any ok
					% since we also require that previous instances hold, we can't take 
					% the current result
					PLatestDefinite = nondet
					;
					% it's a simple P so go with the flow...
					PLatestDefinite = PLatestDefinite2
				), !
			; PLatestDefinite1 < CurrentTime - 1, % there's a gap of definite result points
				PLatestDefinite = PLatestDefinite1, !
			; % PLatestDefinite1 >= CurrentTime - 1
				getMax(PLatestDefinite1, PLatestDefinite2, PLatestDefinite)
			),
			(PLatestPossible1 = nondet,
				(PSchedIdIn >= 0 ->
					% this means that there has been a schedule that didn't return anything -> fail
					PLatestPossible = nondet
					;
					PLatestPossible = PLatestPossible2
				), !
			; PLatestPossible1 < CurrentTime - 1, % this is actually very bad and shouldn't really happen
				PLatestPossible = PLatestPossible1, !
			; % PLatestPossible1 >= CurrentTime - 1
				getMax(PLatestPossible1, PLatestPossible2, PLatestPossible)
			),
			printlog("   P3: %w - %w\n", [PLatestDefinite, PLatestPossible]),
			!
		% ------- END EVALUATE P ----------------------
			;
			% either no Q can and will be found or Q is true at StartTime -> P was skipped
			PLatestDefinite = nondet,
			PLatestPossible = nondet,
			PSchedIdIn = -1, PSchedId = -1, ToScheduleP = nondet
		), 
		% now we have all 4 time markers so the overall result can be determined.
		%printf("PLatestDefinite=%w \nQEarliestDefinite=%w \nDeadline=%w \nIntervalEnd=%w\n",
		%	[PLatestDefinite, QEarliestDefinite, Deadline, IntervalEnd]),
		(QEarliestDefinite \= nondet, 
			QEarliestDefinite =< StartTime,
			Res = ok, 
			FailureTerm = none, !
			;
			calculate_until_result(PLatestDefinite, PLatestPossible,	
				QEarliestDefinite, QEarliestPossible, EndTime, Deadline, Res, FailureTerm)
		),
		shelf_set(Shelf, 3, Res),
		printlog("   Res: %w\n", [Res]),
		(FailureTerm \= none ->
			record(MyFailures, until_failed(FailureTerm, 
				ToplevelFormula, FormulaPath, CurrentTime, StartTime, EndTime))
			;
			true
		),		
		(not PSchedId =:= -1 ->
			KeyP =.. [p, SubPathP],
			var(VarPSchedId),
			SParams1 = [KeyP : PSchedId],
			shelf_set(Shelf, 1, sched(KeyP, VarPSchedId, ToScheduleP)) 
			; 
			SParams1 = []
		),
		(not QSchedId =:= -1 ->
			KeyQ =.. [p, SubPathQ],
			var(VarQSchedId),
			append(SParams1, [KeyQ : QSchedId], SParams2),
			shelf_set(Shelf, 2, sched(KeyQ, VarQSchedId, ToScheduleQ)) 
			; 
			SParams2 = SParams1
		),
		(QSchedIdIn =\= QSchedId, !
			; PSchedIdIn =\= PSchedId -> 
				shelf_set(Shelf, 4, true) ; true),
		shelf_set(Shelf, 5, SParams2),
		% retrieve all output values from shelf
		shelf_get(Shelf, 0, pqres(NewP, NewQ, Result, HasChanged, ScheduleParams)),
		shelf_abolish(Shelf),
		getval(negated, Negated),
		((Negated = 0, Result = not_ok ; Negated = 1, Result = ok) ->
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
				
		NextLevel is Level + 1,
		last_element(StartTimes, s(_, MaxStartTime)),
		Deadline is MaxStartTime + MaxTime,
		% the end of the interval that is to be inspected
		IntervalEnd is min(Deadline, EndTime),
		
		time(CurrentTime, do2(tick(CurrentStep), s0)),
		
		shelf_create(orig/1, null, Shelf),
		shelf_set(Shelf,1,P),
		append(FormulaPath, [1], SubPathP),
		(P = sched(_, PSchedIdIn, PRefTerm) ->  
			(PRefTerm = cf(PCacheId) ->
				get_cached_formula(PCacheId, SubP)
				;
				SubP = PRefTerm,
				PCacheId = -1
			)
			;
			SubP = P,
			PSchedIdIn = -1,
			PCacheId = -1		
		),
		evaluate_for_all_timesteps(ToplevelFormula, SubPathP, 
			Mode, CurrentStep, CurrentTime, IntervalEnd, SubP, NextLevel, Result1, 
			PSchedIdIn, PCacheId,
			PSchedId, ToScheduleP,
			EarliestDefinite, _, EarliestPossible, _),
		
		(Result1 = ok, Mode = eventually, !,
			Result3 = ok
		; Result1 = not_ok, Mode = always, !,
			Result3 = not_ok
		; % not decided yet  
			(PSchedId >= 0 ->
				check_schedule_for_interval(PSchedId, StartTime, IntervalEnd, 
					Mode, Result2, _, _, _, _),
				(Result2 = not_ok, 
					(Mode = always ->
						Result3 = not_ok
						;
						Result3 = Result1
					), !
				; Result2 = ok,
					(Mode = eventually ->
						Result3 = ok
						;
						Result3 = Result1
					), !
				; % Result2 = nondet
					Result3 = nondet
				)
				; % not scheduled before
				Result3 = Result1
			)
		),
		
		(Result3 = nondet ->
			Result = nondet
			;
			(Mode = eventually ->
				(Result3 = ok ->
					Result = ok
					; % not_ok,
					(Deadline > IntervalEnd ->
						Result = nondet
						;
						Result = not_ok
					)
				)
				; % always
				(Result3 = not_ok ->
					Result = not_ok
					; % ok
					(Deadline > IntervalEnd ->
						Result = nondet
						;
						Result = ok
					)
				)
			)
		),		
		(PSchedIdIn =\= PSchedId ->
			HasChanged = true
			;
			HasChanged = false
		),		
		(Result = nondet -> 
			(PSchedId > -1 ->
				KeyP =.. [p, SubPathP],
				var(VarPSchedId),
				ScheduleParams = [KeyP : PSchedId],
				ToSchedule = sched(KeyP, VarPSchedId, ToScheduleP)
				;
				shelf_get(Shelf, 1, OrigP),
				ScheduleParams = [],
				ToSchedule = OrigP			
			)		
			;
			ToSchedule = Result,
			ScheduleParams= []		
		),
		shelf_abolish(Shelf).	