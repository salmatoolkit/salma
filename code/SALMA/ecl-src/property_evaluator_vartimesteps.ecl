% iterates over the given interval and evaluates P for all timesteps
% Result:
% - for eventually: 
%		- Result is nondet if one of the timesteps was nondet
%		- Result is not_ok if only not_oks were found
% 		- Result is ok if one ok was found
% - for always:
%		- Result is not_ok if one timestep was not_ok
%		- Result is ok if all timesteps were ok
%		- Result is nondet if at least one timestep was nondet and all others were ok
evaluate_for_all_timesteps(ToplevelFormula, FormulaPath, 
	Mode, StartStep, CurrentTime, StartTime, EndTime, P, Level, Result, 
	ScheduleIdIn, CacheIdIn,
	ScheduleIdOut, ToSchedule,
	EarliestDefinite, LatestDefinite, EarliestPossible, LatestPossible) :-
	% make sure that interval [T-T] includes the current step
	EndTime2 is EndTime + 1,
	shelf_create(orig/1, null, Shelf),
	shelf_set(Shelf,1,P),
	(Mode = eventually ->
		ResStart = not_ok
		;
		ResStart = ok
	),
	(CurrentTime < EndTime2 ->
	
		(fromto(CurrentTime, T, T2, EndTime2), fromto(StartStep, Step, NextStep, _), 
			fromto(ResStart, ResIn, ResOut, Result), % start with ok, might be switched to nondet and stay that way
			fromto(ScheduleIdIn, SId1, SId2, ScheduleIdOut), 
			fromto(nondet, EDIn, EDOut, EarliestDefinite), 
			fromto(nondet, EPIn, EPOut, EarliestPossible),
			fromto(nondet, LDIn, LDOut, LatestDefinite), 
			fromto(nondet, LPIn, LPOut, LatestPossible),
			param(Shelf, ToplevelFormula, FormulaPath, Mode, StartTime, EndTime, EndTime2, Level) do
				shelf_get(Shelf, 1, OrigP),
				Sit = do2(tick(Step), s0),
				%print(Sit), nl,
				% idea: only substitute until reaching until block
				
				subst_in_term(s0, Sit, OrigP, P2, [until]), 	
				%print(P2), nl,
				% TODO: is 0 as level ok here?
				time(StepTime, Sit),
				evaluate_and_schedule(ToplevelFormula, FormulaPath, Step, StartTime, EndTime, 
					P2, -1, Level, SId1, Res, _, SId2, _),		
				%print(Res), nl,
				% TODO: how to deal with ToSchedule2?
				NextStep is Step + 1,
				(Res = nondet,
					ResOut = nondet, 
					T2 is T + 1, 
					EDOut = EDIn,
					EPOut = getMin(EPIn, T),
					LDOut = LDIn,			
					LPOut = T,
					!
					;
				Res = ok,
					EDOut is getMin(EDIn, T),
					EPOut is getMin(EPIn, T),
					LDOut = T,			
					LPOut = T,
					(Mode = always ->
						ResOut = ResIn,
						T2 is T + 1
						; % eventually
						ResOut = ok,
						T2 = EndTime2 % this stops the iteration
					), !
				; 
				Res = not_ok,
					EDOut = EDIn,
					EPOut = EPIn,
					LDOut = LDIn,			
					LPOut = LPIn,
					(Mode = eventually ->
						ResOut = ResIn,
						T2 is T + 1
						; % always
						ResOut = not_ok,
						T2 = EndTime2 % this stops the iteration
					), !
				; throw(wrong_mode(Mode))		
				)			
				
		)
	; % CurrentTime > EndTime2
		Result = not_ok,
		ScheduleIdOut = ScheduleIdIn,
		EarliestDefinite = nondet,
		LatestDefinite = nondet, 
		EarliestPossible = nondet, 
		LatestPossible = nondet
	),
	((Result = nondet, ! ; ScheduleIdIn > -1) -> % schedule original if nondet or O had been scheduled before
		% cache original formula if necessary
		(CacheIdIn = -1 ->
			shelf_get(Shelf, 1, OrigP),
			cache_formula(ToplevelFormula, FormulaPath, OrigP, CacheId)
			;
			CacheId = CacheIdIn
		),
		ToSchedule = cf(CacheId)
		;
		ToSchedule = Result
	),
	shelf_abolish(Shelf).
	

% checks the current states of the scheduled goals for the
% given PSchedId to determine the definite and possible time markers. 
% Note: only considers schedule, the actual time markers could differ
% due to current results (evaluate_for_all_timesteps).
%
% Result:
% - for eventually: 
%		- Result is nondet if one of the timesteps was nondet
%		- Result is not_ok if only not_oks were found
% 		- Result is ok if one ok was found
% - for always:
%		- Result is not_ok if one timestep was not_ok
%		- Result is ok if all timesteps were ok
%		- Result is nondet if at least one timestep was nondet and all others were ok
check_schedule_for_interval(PSchedId, Level, Start, End, Mode, OverallResult, 
	EarliestDefinite, LatestDefinite, EarliestPossible, LatestPossible) :-
	get_scheduled_intervals_within(PSchedId, Level,
		Start, End, 
		NondetIntervals, OkIntervals, NotOkIntervals),
	
	(Mode = eventually ->
		(length(OkIntervals) > 0 ->
			OkIntervals = [s(EarliestDefinite,_) | _],
			% for eventually, last definite doesn't need to
			% be connected continuously	
			last_element(OkIntervals, s(_, LatestDefinite)),
			Res1 = ok
			; % ok-intervals empty
			EarliestDefinite = nondet,
			LatestDefinite = nondet,
			Res1 = not_ok
		),
		(length(NondetIntervals) > 0 ->
			NondetIntervals = [s(EP1, _)],
			getMin(EP1, EarliestDefinite, EarliestPossible),
			last_element(NondetIntervals, s(_, LP1)),
			getMax(LP1, LatestDefinite, LatestPossible),
			Res2 = nondet
			; % nondet-intervals empty
			EarliestPossible = EarliestDefinite,
			LatestPossible = LatestDefinite,
			(Ok1 = true -> OverallResult = ok ; OverallResult = nondet),
			Res2 = not_ok
		),
		(Res1 = ok -> OverallResult = ok
			; (Res2 = nondet -> OverallResult = nondet 
				; OverallResult = not_ok))
		; % always
		% idea: merge goals and see if their continuous
		(length(OkIntervals) > 0 ->
			merge_goals(OkIntervals, MergedOkIntervals),
			
	)
			
			
				
		
	
	
% Evaluates F as part of eventually/always block for each step up to EndTime 
%
% StartTime: marks the start of the interval that is checked
% CurrentStep: refers to the currently evaluated step relative to StartTime
evaluate_formula_for_interval(ToplevelFormula, FormulaPath, 
	Mode, StartStep, StartTime, EndTime, P, Level, 
	Result, ToSchedule, ScheduleParams, HasChanged) :-
	% StartTime could be before the current time
	% TODO: should we really distinguish StartTime from CurrentTime?
	append(FormulaPath, [1], SubPathP),
	%SubPathP = FormulaPath,
	time(CurrentTime, do2(tick(StartStep), s0)),
	TimeDiff is EndTime - CurrentTime,
	(TimeDiff < 0 -> throw(end_time_before_current) ; true),
	shelf_create(orig/1, null, Shelf),
	shelf_set(Shelf,1,P),
	
	NextLevel is Level + 1,
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
	% use formula path 
	evaluate_for_all_timesteps(ToplevelFormula, FormulaPath, 
		Mode, StartStep, CurrentTime, StartTime, EndTime, SubP, NextLevel, Result1, 
		PSchedIdIn, PCacheId,
		PSchedId, ToScheduleP,
		_, _, _, _),
	
	(Result1 = ok, Mode = eventually, !,
		Result = ok
	; Result1 = not_ok, Mode = always, !,
		Result = not_ok
	; % not decided yet  
		(PSchedId >= 0 ->
			check_schedule_for_interval(PSchedId, StartTime, EndTime, Mode, Result2, 
				_, _, _, _),
			(
				Result1 = nondet,
				(
					Mode = eventually,
					Result2 = ok,
					Result = ok, !
					;
					Mode = always,
					Result2 = not_ok,
					Result = not_ok, !
					;
					Result = nondet
				), !
				;
				Result = Result2				
			)
			; % PSchedId = -1
			Result = Result1
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
	

calculate_until_result(PLatestDefinite, PLatestPossible,
	QEarliestDefinite, QEarliestPossible,
	EndTime, Deadline, Result, FailureTerm) :-
	IntervalEnd is min(Deadline, EndTime),
	(QEarliestPossible = nondet ->
		(Deadline =< EndTime ->
			Result = not_ok,
			FailureTerm = until_q_timeout
			;
			% Q could still happen in next interval
			% make sure that P is possible for the
			% whole interval
			(PLatestPossible = nondet,
				Result = not_ok, 
				FailureTerm = until_p_failed, !
			; PLatestPossible < IntervalEnd - 1,
				Result = not_ok, 
				FailureTerm = until_p_timeout, !
			; % PLatestPossible > IntervalEnd
				Result = nondet,
				FailureTerm = none			
			)
		)
		; % QEarliestPossible not nondet	
		(QEarliestPossible > Deadline ->
			Result = not_ok,
			FailureTerm = until_q_timeout
			; %QEarliestPossible <= Deadline
			(PLatestPossible = nondet,
				Result = not_ok, 
				FailureTerm = until_p_failed, !
			; PLatestPossible < QEarliestPossible - 1,
				Result = not_ok,
				FailureTerm = until_p_failed, !
			; % PLatestPossible >= QEarliestPossible - 1
			  % -> at least nondet
				( % check for OK
					PLatestDefinite \= nondet, 
					QEarliestDefinite \= nondet,
					PLatestDefinite >= QEarliestDefinite - 1,
					Result = ok, !
					;
					Result = nondet
				),
				FailureTerm = none
			)
		)
	).				
				
					
					
				
				
				
				
				
				
			
			
			

				