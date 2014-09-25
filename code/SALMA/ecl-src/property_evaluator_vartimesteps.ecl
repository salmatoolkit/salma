evaluate_for_all_timesteps(ToplevelFormula, FormulaPath, 
	Mode, StartStep, CurrentTime, StartTime, EndTime, P, Level, Result, 
	ScheduleIdOut) :-
	% make sure that interval [T-T] includes the current step
	EndTime2 is EndTime + 1,
	(fromto(CurrentTime, T, T2, EndTime2), fromto(StartStep, Step, NextStep, _), fromto(nondet, ResIn, ResOut, Result),
		fromto(-1, SId1, SId2, ScheduleIdOut), 
		param(ToplevelFormula, FormulaPath, Mode, StartTime, P, Level) do
		Sit = do2(step(Step), s0),
		% idea: only substitute until reaching until block
		% TODO: handle change predicate 
		subst_in_outer_term_level(P, s0, Sit, P2), 	
		% TODO: handle F = sched(...)
		CacheId = -1,
		evaluate_and_schedule(ToplevelFormula, FormulaPath, Step, StartTime, EndTime, 
			P2, CacheId, Level, SchedIdIn, Res, ToSchedule2, SIdOut, HasChanged2),		
		% TODO: how to deal with ToSchedule2?
		NextStep is Step + 1,
		(Res = nondet,
			ResOut = nondet, 
			T2 is T + 1, !
			% ...
			;
		Res = ok,
			(Mode = always ->
				ResOut = nondet,
				T2 is T + 1
				; % eventually
				ResOut = ok,
				T2 is EndTime2 % this stops the iteration
			), !
		; 
		Res = not_ok,
			(Mode = eventually ->
				ResOut = nondet,
				T2 is T + 1
				; % always
				ResOut = not_ok,
				T2 is EndTime2 % this stops the iteration
			), !
		; throw(wrong_mode(Mode))		
		)			
		% TODO: handle last parameters: ToSchedule, ScheduleParams, HasChanged		
	).


check_schedule_for_interval(PSchedId, StartTime, EndTime, Mode, Result) :-
	EndTime2 is EndTime + 1,
	(Mode = eventually ->
		R0 = nondet
		;
		R0 = ok
	),
	(fromto(StartTime, T, T2, EndTime2), fromto(R0, R1, R2, Result),
		param(PSchedId, Mode) do
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
		)		
	).
		
		
	
	
% Evaluates F for each step up to EndTime
%
% StartTime: marks the start of the interval that is checked
% CurrentStep: refers to the currently ealuated step relative to StartTime
evaluate_formula_for_interval(ToplevelFormula, FormulaPath, 
	Mode, StartStep, StartTime, EndTime, P, Level, Result, ToSchedule, ScheduleParams, HasChanged) :-
	% StartTime could be before the current time
	% TODO: should we really distinguish StartTime from CurrentTime?
	append(FormulaPath, [1], SubPathP),
	time(CurrentTime, do2(step(StartStep), s0)),
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
			SubP = PRefTerm,
			PCacheId is -1
		),
		% If we have sched here, then all time steps have been evaluated 
		% already before. Therefore we don't have to evaluate again but 
		% check the results of the scheduled evaluations.
		check_schedule_for_interval(PSchedId, StartTime, EndTime, Mode, Result)
		;
		SubP = P,
		evaluate_for_all_timesteps(ToplevelFormula, FormulaPath, 
			Mode, StartStep, CurrentTime, StartTime, EndTime, P, NextLevel, Result, 
			PSchedId)
	),	
	(Result = nondet ->
		KeyP =.. [p, SubPathP],
		var(VarPSchedId),
		SParams1 = [KeyP : PSchedId],
		ToSchedule = sched(KeyP, VarPSchedId, SubP),
		HasChanged = true		
		;
		% otherwise we're good...
		ToSchedule = Result,
		HasChanged = true		
	).
	

