evaluate_for_all_timesteps(ToplevelFormula, FormulaPath, 
	Mode, StartStep, CurrentTime, StartTime, EndTime, P, Level, Result, 
	ScheduleIdIn, ScheduleIdOut, 
	EarliestDefinite, LatestDefinite, EarliestPossible, LatestPossible) :-
	% make sure that interval [T-T] includes the current step
	EndTime2 is EndTime + 1,
	shelf_create(orig/1, null, Shelf),
	shelf_set(Shelf,1,P),
	(fromto(CurrentTime, T, T2, EndTime2), fromto(StartStep, Step, NextStep, _), 
		fromto(ok, ResIn, ResOut, Result), % start with ok, might be switched to nondet and stay that way
		fromto(ScheduleIdIn, SId1, SId2, ScheduleIdOut), 
		fromto(EndTime2, EDIn, EDOut, EarliestDefinite), 
		fromto(EndTime2, EPIn, EPOut, EarliestPossible),
		fromto(0, LDIn, LDOut, LatestDefinite), 
		fromto(0, LPIn, LPOut, LatestPossible),
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
			EPOut is min(EPIn, T),
			LDOut = LDIn,			
			LPOut = T,
			!
			% ...
			;
		Res = ok,
			EDOut is min(EDIn, T),
			EPOut is min(EPIn, T),
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
	).


check_schedule_for_interval(PSchedId, StartTime, EndTime, Mode, Result, 
	EarliestDefinite, LatestDefinite, EarliestPossible, LatestPossible) :-
	EndTime2 is EndTime + 1,
	(Mode = eventually ->
		R0 = nondet
		;
		R0 = ok
	),
	(fromto(StartTime, T, T2, EndTime2), fromto(R0, R1, R2, Result),
		fromto(EndTime2, EDIn, EDOut, EarliestDefinite), 
		fromto(EndTime2, EPIn, EPOut, EarliestPossible),
		fromto(0, LDIn, LDOut, LatestDefinite), 
		fromto(0, LPIn, LPOut, LatestPossible),
		param(PSchedId, Mode, EndTime2) do
		store_get(scheduled_goals, sg(_, _, PSchedId, T, _), F),
		F = app(_, Res),
		(Mode = eventually ->
			(Res = ok ->
				R2 = ok,
				T2 = EndTime2
			; % not_ok or nondet
				R2 = R1,
				T2 is T + 1
			)
		;
			((Res = not_ok, ! ; Res = nondet),
				R2 = not_ok,
				T2 = EndTime2, !
				;
			Res = ok,
				R2 = R1,
				T2 is T + 1
			)
		),
		(Res = ok,
			EDOut is min(EDIn, T),
			EPOut is min(EDIn, T),
			LDOut = T,			
			LPOut = T, !
			;
		Res = nondet,
			EDOut = EDIn,
			EPOut is min(EPIn, T),
			LDOut = LDIn,			
			LPOut = T, !
			;
			EDOut = EDIn,
			EPOut = EPIn,
			LDOut = LDIn,			
			LPOut = LPIn
		)		
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
	

