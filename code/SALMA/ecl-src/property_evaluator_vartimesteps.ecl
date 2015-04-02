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
	EarliestDefinite, LatestDefinite, EarliestPossible, LatestPossible,
	EarliestNondet) :-
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
			fromto(nondet, ENondetIn, ENondetOut, EarliestNondet),
			param(Shelf, ToplevelFormula, FormulaPath, Mode, EndTime, EndTime2, Level) do
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
						P2, Sit, -1, Level, SId1, Res, _, SId2, _),		
				%print(Res), nl,
				% TODO: how to deal with ToSchedule2?
				NextStep is Step + 1,
				(Res = nondet,
					ResOut = nondet, 
					T2 is T + 1, 
					EDOut = EDIn,
					EPOut is getMin(EPIn, T),
					ENondetOut is getMin(ENondetIn, T),
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
	

% attempts to find decisions for StartTimes based
% on scheduled ok/not_ok entries for PSchedId
% MaxTime could be inf
check_schedule_for_interval(PSchedId, Level, StartTimes, Mode,
	MaxTime, Results, OkDecisionPoints, NotOkDecisionPoints,
	UnhandledStartTimes) :-
	store_get(scheduled_goals, g(Level, PSchedId), 
		i(_, OkIntervals, NotOkIntervals, _)),	
	% for eventually:
	% 1) for each ok-interval:
	% - extend ok-interval by MaxTime, or to 0 for inf
	% - apply ok to each intersection in StartTimes
	% 2) for each not_ok-interval that is as least as 
	%	 long as MaxTime
	% - take as is, don't extend 
    % - apply not_ok to each intersection in StartTimes
    % -------------------------------------
	% for always: the other way around
	%
	(Mode = eventually ->
		(foreach(Int, OkIntervals), 
			fromto(StartTimes, STIn, STOut, Unhandled1),
			fromto([], Res1In, Res1Out, Res1),
			fromto([], OkDecIn, OkDecOut, OkDecisionPoints),
			param(MaxTime) do
				Int = s(Start, End),
				(MaxTime = inf -> LeftBoundary = 0 ;
					LeftBoundary is Start - MaxTime),
				apply_result_within_interval(STIn, ok,
					LeftBoundary, End, STOut, R),
				append(Res1In, R, Res1Out),
				append(OkDecIn, [Start : R], OkDecOut)
		),
		% handle time-out
		(foreach(Int, NotOkIntervals), 
			fromto(Unhandled1, STIn, STOut, UnhandledStartTimes),
			fromto(Res1, Res2In, Res2Out, Results),
			fromto([], NotOkDec1In, NotOkDec1Out, NotOkDecisionPoints),
			param(MaxTime) do
				Int = s(Start, End),
				(End - Start >= MaxTime ->
					LastTimeout is Start + End - Start - MaxTime,
					apply_result_within_interval(STIn, not_ok,
						Start, LastTimeout, STOut, R),
					append(Res2In, R, Res2Out),
					append(NotOkDec1In, [Start : R], NotOkDec1Out)
					;
					STOut = STIn,
					Res2Out = Res2In,
					NotOkDec1Out = NotOkDec1In
				)
		)
		; % always
		(foreach(Int, NotOkIntervals), 
			fromto(StartTimes, STIn, STOut, Unhandled1),
			fromto([], Res1In, Res1Out, Res1),
			fromto([], NotOkDec1In, NotOkDec1Out, NotOkDecisionPoints),
			param(MaxTime) do
				Int = s(Start, End),
				(MaxTime = inf -> LeftBoundary = 0 ;
					LeftBoundary is Start - MaxTime),
				apply_result_within_interval(STIn, not_ok,
					LeftBoundary, End, STOut, R),
				append(Res1In, R, Res1Out),
				append(NotOkDec1In, [Start : R], NotOkDec1Out)
		),
		% confirm
		(foreach(Int, OkIntervals), 
			fromto(Unhandled1, STIn, STOut, UnhandledStartTimes),
			fromto(Res1, Res2In, Res2Out, Results),
			fromto([], OkDec1In, OkDec1Out, OkDecisionPoints),
			param(MaxTime) do
				Int = s(Start, End),
				(End - Start >= MaxTime ->
					LastConfirmable is Start + End - Start - MaxTime,
					apply_result_within_interval(STIn, ok,
						Start, LastConfirmable, STOut, R),
					append(Res2In, R, Res2Out),
					append(OkDec1In, [Start : R], OkDec1Out)
					;
					STOut = STIn,
					Res2Out = Res2In,
					OkDec1Out = OkDec1In
				)
		)
	).	
	
	
check_schedule_for_interval_until(PSchedId, QSchedId, Level, StartTimes,
	MaxTime, Results, UnhandledStartTimes) :-
	store_get(scheduled_goals, g(Level, PSchedId), 
		i(_, POkIntervals, PNotOkIntervals, _)),	
	store_get(scheduled_goals, g(Level, QSchedId), 
		i(_, QOkIntervals, QNotOkIntervals, _)),
	% assumptions:
	% - interval lists sorted in ascending order
	
	% precedence of confirm: points that are both P and not(Q) will 
	% be confirmed
	confirm_scheduled_until_goals(StartTimes, POkIntervals, QOkIntervals, MaxTime, 
		Results1, Unhandled1),
		
	reject_until_goals_within_not_ok_interval(Unhandled1, 
		PNotOkIntervals, Results2, Unhandled2),		

	reject_until_goals_within_terminated_interval(Unhandled2, 
		PNotOkIntervals, QNotOkIntervals, Results3, Unhandled3),
	
	apply_until_timeout(Unhandled3, QNotOkIntervals, MaxTime, 
		Results4, UnhandledStartTimes),
		
	flatten([Results1, Results2, Results3, Results4], UnsortedResults),
	sort([1,1], =<, UnsortedResults, Results).
	

% reject all start points that lie within a Not-P-Interval
reject_until_goals_within_not_ok_interval(StartTimes, 
	PNotOkIntervals, Results, UnhandledStartTimes) :-	
		(foreach(Int, PNotOkIntervals), 
			fromto([], R1, R2, Results),
			fromto(StartTimes, UnhandledIn, UnhandledOut, UnhandledStartTimes) do
				Int = s(T1, T2),
				apply_result_within_interval(UnhandledIn, not_ok,
					T1, T2, UnhandledOut, R),
				append(R1, R, R2)
		).


% -	look for Not-Q-Intervals that contain a Not-P-Point.
% - reject all start points that lie within the left segment
%   formed by the start of the interval and the highest such Not-P points.		
reject_until_goals_within_terminated_interval(StartTimes, 
	PNotOkIntervals, QNotOkIntervals, Results, UnhandledStartTimes) :-

		(fromto(QNotOkIntervals, ListIn, ListOut, []),
			fromto([], ResIn, ResOut, Results),
			fromto(StartTimes, UnhandledIn, UnhandledOut, UnhandledStartTimes), 
			param(PNotOkIntervals) do
				(length(UnhandledIn) > 0 ->
					ListIn = [s(T1, T2) | ListOut],
					get_max_interval_point_within(PNotOkIntervals, T1, T2, Max),
					(Max \= none -> 
						apply_result_within_interval(UnhandledIn, not_ok,
							T1, Max, UnhandledOut, R),
						append(ResIn, R, ResOut)
						;
						UnhandledOut = UnhandledIn,
						ResOut = ResIn
					)
					; % no more unhandled start intervals -> stop
					ListOut = [],
					ResOut = ResIn,
					UnhandledOut = []
				)
		).

apply_until_timeout(StartTimes, QNotOkIntervals, MaxTime, 
	Results, UnhandledStartTimes) :-
		% handle time-out
		(foreach(Int, QNotOkIntervals), 
			fromto(StartTimes, STIn, STOut, UnhandledStartTimes),
			fromto([], ResIn, ResOut, Results),
			param(MaxTime) do
				Int = s(Start, End),
				(End - Start >= MaxTime ->
					LastTimeout is Start + End - Start - MaxTime,
					apply_result_within_interval(STIn, not_ok,
						Start, LastTimeout, STOut, R),
					append(ResIn, R, ResOut)
					;
					ResOut = ResIn,
					STOut = STIn
				)
		).

	
confirm_scheduled_until_goals(StartTimes, POkIntervals, QOkIntervals, MaxTime, 
	Results, UnhandledStartTimes) :-
		(foreach(QOkInterval, QOkIntervals),
			fromto(StartTimes, UnhandledIn, UnhandledOut, UnhandledStartTimes),
			fromto([], ResIn, ResOut, Results),
			param(POkIntervals, MaxTime) do
		
				QOkInterval = s(QStart, QEnd),
				
				% first confirm all starting points where Q is true at once
				apply_result_within_interval(UnhandledIn, ok, QStart, QEnd,
					U1, Confirmed1),
					
				(MaxTime = inf -> LeftBoundary = 0 ;
							LeftBoundary is QStart - MaxTime),
				get_intervals_within(U1, LeftBoundary, QEnd, Candidates, 
					Remaining1),
				% Selection now contains candidates that could
				% be ok if P is ok until QStart
				(length(Candidates) > 0 ->
					get_right_bound_continuous_intersection(POkIntervals, 
						QStart, POkSpan),
					(POkSpan \= none ->
						POkSpan = s(POkStart, POkEnd),
						get_intervals_within(Candidates, POkStart, POkEnd, 
							ToConfirm, Remaining2)
						;
						ToConfirm = [],
						Remaining2 = Candidates
					)
					
					; % no candidates
					ToConfirm = [],
					Remaining2 = []
				),
				apply_unique_result(ToConfirm, ok, Confirmed2),
				append(ResIn, Confirmed1, ResOutPre),
				append(ResOutPre, Confirmed2, ResOut), 
				
				append(Remaining1, Remaining2, RemainingUnsorted),
				sort(0, =<, RemainingUnsorted, RemainingUnmerged),
				merge_goals(RemainingUnmerged, UnhandledOut)
		).
	
	
% Evaluates F as part of an invariant/goal block 
%
evaluate_formula_for_interval(ToplevelFormula, FormulaPath, 
	Mode, StartStep, StartTimes, EndTime, P, Level, 
	Results, OverallResult, ToSchedule, ScheduleParams, HasChanged) :-
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
		_, _, _, _, _),
	% for interval/goal, we have unique results since no max time
	% is given
	(Result1 = ok, Mode = eventually, !,
		OverallResult = ok,
		apply_unique_result(StartTimes, ok, Results)
	; Result1 = not_ok, Mode = always, !,
		OverallResult = not_ok,
		apply_unique_result(StartTimes, not_ok, Results)
	; % not decided yet  
		(PSchedId >= 0 ->
			check_schedule_for_interval(PSchedId, NextLevel, StartTimes, Mode,
				inf, ResultsPre, _, _,
				UnhandledStartTimes),
			% unhandled means nondet here
			(length(UnhandledStartTimes) > 0 ->
				apply_unique_result(UnhandledStartTimes, nondet, 
					ResultsUnhandled),
				append(ResultsPre, ResultsUnhandled, ResultsUnsorted),
				sort([1,1], =<, ResultsUnsorted, Results)
				;
				Results = ResultsPre
			),
			get_unanimous_result(Results, OverallResult)
			; % PSchedId = -1
			OverallResult = Result1,
			apply_unique_result(StartTimes, OverallResult, Results)			
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
	shelf_abolish(Shelf).
	

% calculate_until_result(PLatestDefinite, PLatestPossible,
	% QEarliestDefinite, QEarliestPossible,
	% EndTime, Deadline, Result, FailureTerm) :-
	% IntervalEnd is min(Deadline, EndTime),
	% (QEarliestPossible = nondet ->
		% (Deadline =< EndTime ->
			% Result = not_ok,
			% FailureTerm = until_q_timeout
			% ;
			% % Q could still happen in next interval
			% % make sure that P is possible for the
			% % whole interval
			% (PLatestPossible = nondet,
				% Result = not_ok, 
				% FailureTerm = until_p_failed, !
			% ; PLatestPossible < IntervalEnd - 1,
				% Result = not_ok, 
				% FailureTerm = until_p_timeout, !
			% ; % PLatestPossible > IntervalEnd
				% Result = nondet,
				% FailureTerm = none			
			% )
		% )
		% ; % QEarliestPossible not nondet	
		% (QEarliestPossible > Deadline ->
			% Result = not_ok,
			% FailureTerm = until_q_timeout
			% ; %QEarliestPossible <= Deadline
			% (PLatestPossible = nondet,
				% Result = not_ok, 
				% FailureTerm = until_p_failed, !
			% ; PLatestPossible < QEarliestPossible - 1,
				% Result = not_ok,
				% FailureTerm = until_p_failed, !
			% ; % PLatestPossible >= QEarliestPossible - 1
			  % % -> at least nondet
				% ( % check for OK
					% PLatestDefinite \= nondet, 
					% QEarliestDefinite \= nondet,
					% PLatestDefinite >= QEarliestDefinite - 1,
					% Result = ok, !
					% ;
					% Result = nondet
				% ),
				% FailureTerm = none
			% )
		% )
	% ).				
				
					
					
				
				
				
				
				
				
			
			
			

				