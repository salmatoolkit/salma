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
	Mode, StartStep, StartTime, EndTime, P, Level, Result, 
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
	(StartTime < EndTime2 ->
	
		(fromto(StartTime, T, T2, EndTime2), fromto(StartStep, Step, NextStep, _), 
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
				evaluate_and_schedule(ToplevelFormula, FormulaPath, 
				Step, StepTime, EndTime, 
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
					(Mode = eventually ->
						EDOut = EDIn,
						EPOut is getMin(EPIn, T + 1),
						LDOut = LDIn,			
						LPOut = LPIn,
						ResOut = ResIn,
						T2 is T + 1
						; % always
						ResOut = not_ok,
						T2 = EndTime2, % this stops the iteration
						EDOut = EDIn,
						EPOut = EPIn,
						LDOut = LDIn,
						% we know that the previous step was the last
						% where a positive result is event possible
						LPOut is getMin(LPIn, T - 1) 						
					), !
				; throw(wrong_result(Res))		
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
			NondetIntervals = [s(EP1, _) | _],
			getMin(EP1, EarliestDefinite, EarliestPossible),
			last_element(NondetIntervals, s(_, LP1)),
			getMax(LP1, LatestDefinite, LatestPossible),
			Res2 = nondet
			; % nondet-intervals empty
			EarliestPossible = EarliestDefinite,
			LatestPossible = LatestDefinite,
			Res2 = not_ok
		),
		(Res1 = ok, !, OverallResult = ok
			; Res2 = nondet, !, OverallResult = nondet 
			; OverallResult = not_ok)
		; % MODE = always
		(length(NotOkIntervals) > 0 ->
			OverallResult = not_ok,
			EarliestDefinite = nondet,
			EarliestPossible = nondet,
			LatestDefinite = nondet,
			LatestPossible = nondet
			;
			% integrity check: there must not be a gap 
			% without any result!
			append(OkIntervals, NondetIntervals, NonNegativeIntervals),
			(merge_goals(NonNegativeIntervals, s(Start, End)) ->
				true ; throw(evaluation_gap(NonNegativeIntervals))),
			EarliestPossible = Start,
			LatestPossible = End,
			(length(OkIntervals) > 0 ->
				OkIntervals = [s(OkStartFirst, OkEndFirst) | _],
				(OkStartFirst > Start ->
					EarliestDefinite = nondet,
					LatestDefinite = nondet,
					% There's a gap between Start and the first ok. We know that
					% there are no not_ok intervals and there's no unspecified gap ->
					% there have to be nondet intervals at the start
					OverallResult = nondet		
					; 
					EarliestDefinite = Start,
					LatestDefinite = OkEndFirst,
					(LatestDefinite < End ->
						OverallResult = nondet ; OverallResult = ok)
				)
				; % all nondet
				EarliestDefinite = nondet,
				LatestDefinite = nondet,
				OverallResult = nondet	
			)
		)
	).		
			
				
		
	
	
% Evaluates F as part of an invariant/goal block 
%
evaluate_formula_for_interval(ToplevelFormula, FormulaPath, 
	Mode, StartStep, StartTimes, EndTime, P, Level, 
	Results, OverallResult, ToSchedule, ScheduleParams, HasChanged) :-
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
		Mode, StartStep, CurrentTime, EndTime, SubP, NextLevel, Result1, 
		PSchedIdIn, PCacheId,
		PSchedId, ToScheduleP,
		_, _, _, _),
	% for interval/goal, we have unique results since no max time
	% is given
	(Result1 = ok, Mode = eventually, !,
		OverallResult = ok
	; Result1 = not_ok, Mode = always, !,
		OverallResult = not_ok
	; % not decided yet  
		(PSchedId >= 0 ->
			check_schedule_for_interval(PSchedId, NextLevel, 
				StartTime, EndTime, Mode, Result2, 
				_, _, _, _),
			(
				Result1 = nondet,
				(
					Mode = eventually,
					Result2 = ok,
					OverallResult = ok, !
					;
					Mode = always,
					Result2 = not_ok,
					OverallResult = not_ok, !
					;
					OverallResult = nondet
				), !
				;
				OverallResult = Result2				
			)
			; % PSchedId = -1
			OverallResult = Result1
		)

	),	
	(PSchedIdIn =\= PSchedId ->
		HasChanged = true
		;
		HasChanged = false
	),
	(OverallResult = nondet -> 
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
		ToSchedule = OverallResult,
		ScheduleParams= []
		
	),
	shelf_abolish(Shelf),
	apply_unique_result(StartTimes, OverallResult, Results).
	
	

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
				
					
					
				
				
				
				
				
				
			
			
			

				