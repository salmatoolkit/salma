evaluate_for_all_timesteps(ToplevelFormula, FormulaPath, 
	Mode, StartStep, CurrentTime, StartTime, EndTime, P, Level, Result, 
	ScheduleIdIn, ScheduleIdOut, 
	EarliestDefinite, LatestDefinite, EarliestPossible, LatestPossible) :-
	% make sure that interval [T-T] includes the current step
	EndTime2 is EndTime + 1,
	shelf_create(orig/1, null, Shelf),
	shelf_set(Shelf,1,P),
	(CurrentTime < EndTime2 ->
	
		(fromto(CurrentTime, T, T2, EndTime2), fromto(StartStep, Step, NextStep, _), 
			fromto(ok, ResIn, ResOut, Result), % start with ok, might be switched to nondet and stay that way
			fromto(ScheduleIdIn, SId1, SId2, ScheduleIdOut), 
			fromto(nondet, EDIn, EDOut, EarliestDefinite), 
			fromto(nondet, EPIn, EPOut, EarliestPossible),
			fromto(nondet, LDIn, LDOut, LatestDefinite), 
			fromto(nondet, LPIn, LPOut, LatestPossible),
			param(Shelf, ToplevelFormula, FormulaPath, Mode, StartTime, Level, EndTime, EndTime2) do
			shelf_get(Shelf, 1, OrigP),
			Sit = do2(tick(Step), s0),
			print(Sit), nl,
			% idea: only substitute until reaching until block
			
			subst_in_term(s0, Sit, OrigP, P2, [until]), 	
			print(P2), nl,
			CacheId = -1,
			evaluate_and_schedule(ToplevelFormula, FormulaPath, Step, StartTime, EndTime, 
				P2, CacheId, Level, SId1, Res, _, SId2, _),		
			print(Res), nl,
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
					ResOut = nondet,
					T2 is T + 1
					; % always
					ResOut = not_ok,
					T2 = EndTime2 % this stops the iteration
				), !
			; throw(wrong_mode(Mode))		
			)			
			% TODO: handle last parameters: ToSchedule, ScheduleParams, HasChanged		
		)
	; % CurrentTime > EndTime2
		Result = not_ok,
		ScheduleIdOut = ScheduleIdIn,
		EarliestDefinite = nondet,
		LatestDefinite = nondet, 
		EarliestPossible = nondet, 
		LatestPossible = nondet
	).
	

% Returns a sorted list of T : Result entries for the 
% given schedule id. The list is sorted by time.
get_schedule_sequence(PSchedId, StartTime, EndTime, L) :-
	stored_keys_and_values(scheduled_goals, List1),
		
	% select all for given PSchedId and T >= StartTime
	(foreach(Entry, List1), fromto([], In, Out, List2), 
		param(PSchedId, StartTime, EndTime) do
		% level can be ignored since we assume that each id is unique
		(Entry = sg(_,_, PSchedId, T, _, _) - Goal , T >= StartTime, T =< EndTime) 
			-> append(In, [T:Goal], Out)
			; Out = In
		),
	sort(List2, L).

% checks the current states of the scheduled goals for the
% given PSchedId to determine the definite and possible time markers. 
% Note: only considers schedule, the actual time markers could differ
% due to current results (evaluate_for_all_timesteps).
check_schedule_for_interval(PSchedId, StartTime, EndTime, Mode, Result, 
	EarliestDefinite, LatestDefinite, EarliestPossible, LatestPossible) :-
	EndTime2 is EndTime + 1,
	(StartTime < EndTime2 ->
		(Mode = eventually ->
			R0 = nondet
			;
			R0 = ok
		),
		get_schedule_sequence(PSchedId, StartTime, EndTime, 
			ScheduleSequence),		
		(	fromto(ScheduleSequence, SeqIn, SeqOut, []),
			fromto(R0, R1, R2, Result),
			fromto(nondet, EDIn, EDOut, EarliestDefinite), 
			fromto(nondet, EPIn, EPOut, EarliestPossible),
			fromto(nondet, LDIn, LDOut, LatestDefinite), 
			fromto(nondet, LPIn, LPOut, LatestPossible),
			param(Mode) do
			SeqIn = [SeqEntry | SeqTail],
			SeqEntry = T : F,
			F = app(_, Res),
			(Mode = eventually ->
				(Res = ok,
					R2 = ok,
					SeqOut = [],
					EDOut is getMin(EDIn, T),
					EPOut is getMin(EDIn, T),
					LDOut = T,			
					LPOut = T, !
				; Res = not_ok,
					R2 = R1,
					SeqOut = SeqTail,
					EDOut = EDIn,
					EPOut = EPIn,
					LDOut = LDIn,			
					LPOut = LPIn, !
				; % nondet
					R2 = R1,
					SeqOut = SeqTail,			
					EDOut = EDIn,
					EPOut is getMin(EPIn, T),
					LDOut = LDIn,			
					LPOut = T
				)
			; % always
				(Res = not_ok,
					R2 = not_ok,
					SeqOut = [], 
					EDOut = EDIn,
					EPOut = EPIn,
					LDOut = LDIn,			
					LPOut = LPIn, !					
					;
				Res = ok,
					R2 = R1,
					SeqOut = SeqTail, 
					EDOut is getMin(EDIn, T),
					EPOut is getMin(EPIn, T),
					LDOut = T,			
					LPOut = T, !
				; % nondet
					R2 = nondet,
					SeqOut = SeqTail,
					EDOut = EDIn,
					EPOut is getMin(EPIn, T),
					LDOut = LDIn,			
					LPOut = T
				)
			)		
		)
	; % StartTime > EndTime2
		Result = not_ok,
		EarliestDefinite = nondet,
		LatestDefinite = nondet, 
		EarliestPossible = nondet, 
		LatestPossible = nondet
	).
		
		
	
	
% Evaluates F as part of eventually/always block for each step up to EndTime 
%
% StartTime: marks the start of the interval that is checked
% CurrentStep: refers to the currently ealuated step relative to StartTime
evaluate_formula_for_interval(ToplevelFormula, FormulaPath, 
	Mode, StartStep, StartTime, EndTime, P, Level, Result, ToSchedule, ScheduleParams, HasChanged, 
	EarliestDefinite, LatestDefinite, EarliestPossible, LatestPossible) :-
	% StartTime could be before the current time
	% TODO: should we really distinguish StartTime from CurrentTime?
	append(FormulaPath, [1], SubPathP),
	time(CurrentTime, do2(tick(StartStep), s0)),
	TimeDiff is EndTime - CurrentTime,
	(TimeDiff < 0 -> throw(end_time_before_current) ; true),
	% increase level so that steps will be evaluated before 
	% the always/eventually parent formula in 
	% evaluate_all_scheduled
	NextLevel is Level + 1,
	(P = sched(_, PSchedId, PRefTerm) ->  
		(PRefTerm = cf(PCacheId) ->
			get_cached_formula(PCacheId, SubP)
			;
			SubP = PRefTerm
		),
		% If we have sched here, then all time steps have been evaluated 
		% already before. Therefore we don't have to evaluate again but 
		% check the results of the scheduled evaluations.
		check_schedule_for_interval(PSchedId, StartTime, EndTime, Mode, Result, 
			EarliestDefinite, LatestDefinite, EarliestPossible, LatestPossible)
		;
		SubP = P,
		evaluate_for_all_timesteps(ToplevelFormula, FormulaPath, 
			Mode, StartStep, CurrentTime, StartTime, EndTime, P, NextLevel, Result, 
			-1, PSchedId, EarliestDefinite, LatestDefinite, EarliestPossible, LatestPossible)
	),	
	(Result = nondet ->
		KeyP =.. [p, SubPathP],
		var(VarPSchedId),
		ScheduleParams = [KeyP : PSchedId],
		ToSchedule = sched(KeyP, VarPSchedId, SubP),
		HasChanged = true		
		;
		% otherwise we're good...
		ToSchedule = Result,
		ScheduleParams = [],
		HasChanged = true		
	).
	

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
			; PLatestPossible =< IntervalEnd,
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
			; PLatestPossible < QEarliestPossible,
				Result = not_ok,
				FailureTerm = until_p_failed, !
			; % PLatestPossible >= QEarliestPossible
			  % -> at least nondet
				( % check for OK
					PLatestDefinite \= nondet, 
					QEarliestDefinite \= nondet,
					PLatestDefinite >= QEarliestDefinite,
					Result = ok, !
					;
					Result = nondet
				),
				FailureTerm = none
			)
		)
	).				
				
					
					
				
				
				
				
				
				
			
			
			

				