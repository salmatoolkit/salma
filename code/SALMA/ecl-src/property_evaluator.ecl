% signature changes:
% -- evaluate_until
% -- evaluate_and_schedule
:- local store(formula_cache).
:- local variable(next_formula_cache_id).
% stores results of evaluate_toplevel 
% structure: toplevelformula(Path) - List<Cache-Ids>
:- local store(formula_cache_candidates).
:- local store(toplevel_goals).
% stores uncompiled properties
:- local store(original_properties).

:- local store(scheduled_goals).
:- local variable(next_scheduled_goal_id).
:- local store(persistent_fluents).
% structure: name - (current_value : last_changed)

:- local store(persistent_fluent_states).

:- local variable(negated, 0).
:- local variable(current_failure_stack, failurestack).
:- lib(hash).
:- lib(lists).
:- dynamic properties_unsynced/0.
:- [property_evaluator_vartimesteps].

init_smc :-
	store_erase(formula_cache),
	store_erase(toplevel_goals),
	store_erase(original_properties),
	store_erase(scheduled_goals),
	store_erase(persistent_fluents),
	store_erase(persistent_fluent_states),
	store_erase(formula_cache_candidates),
	setval(next_scheduled_goal_id, 0),
	setval(next_formula_cache_id, 0),
	setval(negated, 0),
	erase_failure_stack,	
	setval(current_failure_stack, failurestack),
	compile_persistent_fluents,
	set_properties_unsynced(false).
	

erase_failure_stack :-
	getval(current_failure_stack, FStack),
	erase_referenced_failure_stack(FStack).
	
	
erase_referenced_failure_stack(Ref) :-
	(is_record(Ref) ->
		recorded_list(Ref, L),
		(foreach(Entry, L) do
			(Entry = ref(SubRef) ->
				erase_referenced_failure_stack(SubRef)
				;
				true
			)
		),
		erase_all(Ref)
		;
		true
	).
	
get_merged_failures(Failures) :-
	getval(current_failure_stack, FStack),
	get_referenced_failures(FStack, Failures).
	
get_referenced_failures(Ref, Failures) :-
	(is_record(Ref) ->
		recorded_list(Ref, L),
		(foreach(Entry, L), fromto([], F1, F2, Failures) do
			(Entry = ref(SubRef) ->
				get_referenced_failures(SubRef, SubFailures),
				append(F1, SubFailures, F2)
				;
				append(F1, [Entry], F2)
			)
		)
		;
		Failures = []
	).
				

reset_smc :-
	store_erase(scheduled_goals),
	store_erase(persistent_fluent_states),
	setval(negated, 0),
	erase_failure_stack,	
	setval(current_failure_stack, failurestack),
	clean_formula_cache,
	set_properties_unsynced(true).
	
	
set_properties_unsynced(IsUnsynced) :-
	(IsUnsynced = true ->
		(not(properties_unsynced) -> assert(properties_unsynced) ; true)
		;
		(properties_unsynced -> retract(properties_unsynced) ; true)
	).	

% erases all formula cache entries except the ones that are referenced by toplevel entries
% or scheduled goals.
clean_formula_cache :-
	stored_keys_and_values(toplevel_goals, L),
	% first build list of entries to keep
	(foreach(Entry, L), foreach(E2, EntriesToKeep1) do
		Entry = _ - cf(E2)
	),
	
	stored_keys_and_values(scheduled_goals, L2),
	(foreach(Entry, L2), fromto([], In, Out, EntriesToKeep2) do
		Entry = _ - FRef,
		FRef = app(_, F2),
		(F2 = cf(CacheId) ->
			append(In, [CacheId], Out)
			;
			Out = In
		)
	),	
	append(EntriesToKeep1, EntriesToKeep2, EntriesToKeep),
	stored_keys_and_values(formula_cache, L3),
	(foreach(CacheEntry, L3), param(EntriesToKeep) do
		CacheEntry = Id - _,
		(not member(Id, EntriesToKeep) ->
			store_delete(formula_cache, Id)
			;
			true
		)
	),
	stored_keys_and_values(formula_cache_candidates, L4),
	(foreach(CacheCandidateEntry, L4), param(EntriesToKeep) do
		CacheCandidateEntry = FormulaPath - Candidates,
		% keep only top level 
		(FormulaPath =.. [_, 0] ->
			intersection(Candidates, EntriesToKeep, NewCandidates),
			store_set(formula_cache_candidates, FormulaPath, NewCandidates)			
			; % not top level
			store_delete(formula_cache_candidates, FormulaPath)
		)
	).
	
				
flip_negated :-
	getval(negated, N),
	N2 is 1 - N,
	setval(negated, N2).

% Compiles and registers a property to check
register_property(Name, P, P2) :-
	store_set(original_properties, Name, P),
	compile_formula(P, P2),
	add_toplevel_goal(Name, P2).

	
register_property_str(Name, PStr, P2) :-
	term_string(P, PStr),
	register_property(Name, P, P2).

% Clears and rebuilds persistent fluents and toplevel formulas.
% This is mainly done in response to domain changes.
recompile_all :-
	store_erase(persistent_fluents),
	store_erase(formula_cache_candidates),
	store_erase(toplevel_goals),
	compile_persistent_fluents,
	stored_keys_and_values(original_properties, Props),
	(foreach(P, Props) do
		P = Name - F,
		compile_formula(F, F2),
		add_toplevel_goal(Name, F2)
	),
	set_properties_unsynced(false).
		

		
	
		
		

% Evaluates the given (sub-)formula.		
% in ToplevelFormula: name of the top-level formula that F belongs to.
% in FormulaPath: the path of F within the toplevel formula, i.e. a list of
%    numeric positions. 0 stands for the whole formula.
% in F: (sub-)formula to evaluate.
% in StartTime: the reference time point this evaluation refers to.
% in Level: the nesting level of the current evaluation with regard to 
%	nested until formulas.
% out Result: the result (ok, not_ok, nondet)
% out ToSchedule:
% out ScheduleParams:
% out HasChanged:
evaluate_formula(ToplevelFormula, FormulaPath, 
	CurrentStep, StartTime, EndTime, 
	F, Level, Result, 
	ToSchedule, ScheduleParams, HasChanged) :-	
		getval(current_failure_stack, CFS),
		record_create(MyFailures),
		setval(current_failure_stack, MyFailures),
		(
			member(F, [ok, not_ok]), Result = F, 
			ToSchedule = F, ScheduleParams = [], HasChanged = false, !
			;
			F = not2(F2),
			% switch mode for recording failures
			flip_negated,
			append(FormulaPath, [1], SubPath),
			evaluate_formula(ToplevelFormula, SubPath, CurrentStep,
				StartTime, EndTime, F2, Level, Res2, ToSchedule2, ScheduleParams2, HasChanged2),
			flip_negated,
			(
				Res2 = nondet, Result = nondet ;
				Res2 = ok, Result = not_ok ;
				Res2 = not_ok, Result = ok
			),
			(Result = nondet -> 	
				ToSchedule = not2(ToSchedule2),
				ScheduleParams = ScheduleParams2,
				HasChanged = HasChanged2
				;
				ToSchedule = Result, HasChanged = true, ScheduleParams = []
			), !
			;
			F = all(Sf2), !,
			evaluate_checkall(ToplevelFormula, FormulaPath, 
				CurrentStep, StartTime, EndTime, Sf2, Level, Result, 
				OutVar2, ScheduleParams2, HasChanged2), 
			((Result = nondet) -> 
				ToSchedule = all(OutVar2), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = Result, HasChanged = true, ScheduleParams = []
			)
			;
			F = one(Sf2),
			evaluate_checkone(ToplevelFormula, FormulaPath, 
				CurrentStep, StartTime, EndTime, Sf2, Level, Result, OutVar2, ScheduleParams2, HasChanged2), 
			((Result = nondet) -> 
				ToSchedule = one(OutVar2), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = Result, HasChanged = true, ScheduleParams = []
			), !
			;
			F = c_(Sf2), !, % constraint
			evaluate_checkall(ToplevelFormula, FormulaPath, 
				CurrentStep, StartTime, EndTime, Sf2, Level, Result, 
				_, _, _),
			(Result = nondet -> 
				throw(nondet_result_in_constraint(F))
				;
				ToSchedule = Result, HasChanged = true, ScheduleParams = []
			)
			;			
			F = until(MaxTime, P, Q),
			evaluate_until(ToplevelFormula, FormulaPath, 
				CurrentStep, Level, StartTime, EndTime, MaxTime, 
				P, Q, Result, NewP, NewQ, ScheduleParams2, HasChanged2),
			((Result = nondet) ->
				ToSchedule = until(MaxTime, NewP, NewQ), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = Result, HasChanged = true, ScheduleParams = []
			), !		
			;
			F = always(P),
			evaluate_formula_for_interval(ToplevelFormula, FormulaPath, 
				always, CurrentStep, StartTime, EndTime, P, Level, Result, ToSchedule2, ScheduleParams2, HasChanged2),
			((Result = nondet) ->
				ToSchedule = always(ToSchedule2), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = Result, HasChanged = true, ScheduleParams = []
			), !
			;
			F = eventually(P),
			evaluate_formula_for_interval(ToplevelFormula, FormulaPath, 
				eventually, CurrentStep, StartTime, EndTime, P, Level, Result, ToSchedule2, ScheduleParams2, HasChanged2),
			((Result = nondet) ->
				ToSchedule = eventually(ToSchedule2), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = Result, HasChanged = true, ScheduleParams = []
			), !
			;			
			F = let(Var : FreshVar, Def, Body),		
			evaluate_formula(ToplevelFormula, FormulaPath, 
				CurrentStep, StartTime, EndTime, Def, Level, _, _, _, _),
			% only proceed if FreshVar has been bound in the previous evaluation
			(ground(FreshVar) ->
				subst_in_term(Var, FreshVar, Body, Body2),
				evaluate_formula(ToplevelFormula, FormulaPath, 
					CurrentStep, StartTime, EndTime, Body2, 
					Level, Result, ToSchedule2, ScheduleParams2, _)
				;
				Result = not_ok
			),			
			(Result = nondet ->
				ToSchedule = ToSchedule2, HasChanged = true, ScheduleParams = ScheduleParams2
				;
				ToSchedule = Result, HasChanged = true, ScheduleParams = []
			), !
			;
			F = match(VarSpecs, Def, Body),
			evaluate_formula(ToplevelFormula, FormulaPath, 
				CurrentStep, StartTime, EndTime, Def, Level, _, _, _, _),
			(foreach(VarSpec, VarSpecs), fromto(Body, BodyIn, BodyOut, Body2), 
				fromto(true, GIn, GOut, GroundStat) do
					VarSpec = Var : FreshVar,
					% only proceed if FreshVar has been bound in the previous evaluation
					(ground(FreshVar) ->
						subst_in_term(Var, FreshVar, BodyIn, BodyOut),
						GOut = GIn						
						;
						BodyOut = BodyIn,
						GOut = false
					)
			),
			(GroundStat = true ->
				evaluate_formula(ToplevelFormula, FormulaPath, 
					CurrentStep, StartTime, EndTime,
					Body2, Level, Result, ToSchedule2, ScheduleParams2, _)
				;
				Result = not_ok
			),
			(Result = nondet ->
				ToSchedule = ToSchedule2, HasChanged = true, ScheduleParams = ScheduleParams2
				;
				ToSchedule = Result, HasChanged = true, ScheduleParams = []
			), !
			;
						
			% for all remaining cases: restore original formula for Level > 0 or nondet.
			% store original formula as value for ToSchedule later
			shelf_create(orig/1, null, Shelf),
			shelf_set(Shelf,1,F),
			ScheduleParams = [],
			( 
				F = changed(P, Q, ExpectedNow),
				% for now ignore ToSchedule output of changed
				evaluate_changed(ToplevelFormula, FormulaPath, 
					 StartTime, P, Q, ExpectedNow, Level, Result, _),	
				!
				;
				F = pfswitch(PFName, ExpectedNow),
				evaluate_persistent_fluent_switched(PFName, StartTime, ExpectedNow, Result),
				!
				;
				F = occur(ActionTerm),
				% for occur, only the start time is relevant since only tick can occur
				% during a time advance phase
				evaluate_action_occured(ActionTerm, StartTime, Result),
				!
				;
				% is golog program possible?
				F = possible(CurrentStep, GologProg, InitSit),
				(do2(GologProg, InitSit, _) -> 
					Result = ok ; 
					Result = not_ok), !
				;
				% comparison or boolean fluent
				functor(F, Functor, _),
				(
					reifiable_op(Functor), !,
					test_reifiable(F, Result)
					;				
					(constraint_op(Functor, _), ! ; fluent(Functor, _, boolean), ! ;
						derived_fluent(Functor, _, boolean)),
					(call(F) -> Result = ok ; Result = not_ok), !
				), !
				;		
				% otherwise it's a function so call it to bind variables but don't use in schedule term
				call(F), Result = ok, !
			),
			
			((Result = nondet) ->
					shelf_get(Shelf, 1, ToSchedule), HasChanged = false
					;
					ToSchedule = Result, HasChanged = true
			), !
		),
		getval(negated, Negated),
		((Negated = 0, Result = not_ok ; Negated = 1, Result = ok) ->
			record(CFS, ref(MyFailures)),
			record(CFS, failure(Result, ToplevelFormula, FormulaPath, StartTime, F, Level))
			;
			erase_all(MyFailures)
		).

% Executes a reified version of the goal in F. This ensures that the domains of variables are not 
% reduced.
test_reifiable(F, Result) :-
	F =.. [Op | Params],
	append(Params, [T], Params2),
	F2 =.. [Op | Params2],
	call(F2),
	(is_in_domain(1, T) -> Result = ok ; Result = not_ok).
	
% compiles and evaluates a formula ad hoc, i.e. without caching it		
evaluate_ad_hoc(F, Result) :-
		evaluate_ad_hoc(F, Result, s0).
		
evaluate_ad_hoc(F, Result, Situation) :-
		current_time(T),
		compile_formula(F, F2, Situation),
		erase_failure_stack,
		setval(negated, 0),
		evaluate_formula(null, [0], 0, T, T, F2, 0, Result, _, _, _).

evaluate_ad_hoc_str(FStr, Result, Situation) :-
	term_string(F, FStr),
	evaluate_ad_hoc(F, Result, Situation).
	
		
test_ad_hoc(F, Situation) :-
		evaluate_ad_hoc(F, Result, Situation),
		Result = ok.

test_ad_hoc(F) :-
		test_ad_hoc(F, s0).

evaluate_checkall(ToplevelFormula, FormulaPath, 
	CurrentStep, StartTime, EndTime, Subformulas, Level, 
	Result, ToSchedule, ScheduleParams, HasChanged) :- 
		%  check all subformulas. if result can be determined (ok, not_ok) only that result is stored 
		
		(fromto(Subformulas, SFIn, SFOut, []), 
			foreach(F2, Handled), fromto(ok, ResIn, ResOut, Result), fromto(false, In2, Out2, HasChanged), 
			fromto([], In3, Out3, ScheduleParams), count(I,1,_),
			param(CurrentStep, StartTime, EndTime, Level, ToplevelFormula, FormulaPath) do
				% handle cases
				SFIn = [F | SFRest],
				append(FormulaPath, [1,I], SubPath),
				evaluate_formula(ToplevelFormula, SubPath, 
					CurrentStep, StartTime, EndTime, F, Level, Res2, Ts1, ScheduleParams2, HC1),
				(Res2 = nondet, 
					ResOut = nondet,
					SFOut = SFRest,
					F2 = Ts1, 
					HC2 = false,
					append(In3, ScheduleParams2, Out3), !
				; Res2 = ok,
					ResOut = ResIn,
					SFOut = SFRest,		
					F2 = ok, 
					Out3 = In3,
					HC2 = true, !
				; % not_ok
					ResOut = not_ok,
					SFOut = [], % break iteration
					F2 = not_ok,
					Out3 = In3,
					HC2 = true
				),
		
				% update changed flag
				((In2 = true, ! ; HC1 = true, ! ; HC2 = true, !) -> Out2 = true ; Out2 = false)
		), 
		flatten(Handled, ToSchedule).
			

evaluate_checkone(ToplevelFormula, FormulaPath, 
	CurrentStep, StartTime, EndTime, Subformulas, Level, Result, ToSchedule, ScheduleParams, HasChanged) :- 
		%  check all subformulas. if result can be determined (ok, not_ok) only that result is stored 

		(fromto(Subformulas, SFIn, SFOut, []), foreach(F2, Handled), fromto(not_ok, ResIn, ResOut, Result), 
			fromto(false, In2, Out2, HasChanged), 
			fromto([], In3, Out3, ScheduleParams), count(I,1,_),
			param(CurrentStep, StartTime, EndTime, Level, ToplevelFormula, FormulaPath) do
				% handle cases
				SFIn = [F | SFRest],
				append(FormulaPath, [1,I], SubPath),
				evaluate_formula(ToplevelFormula, SubPath, 
					CurrentStep, StartTime, EndTime, F, Level, Res2, Ts1, ScheduleParams2, HC1),
				(Res2 = nondet, 
					ResOut = nondet,
					SFOut = SFRest,
					F2 = Ts1, 
					HC2 = false ,
					append(In3, ScheduleParams2, Out3), !
				; Res2 = not_ok,
					ResOut = ResIn,
					SFOut = SFRest,
					F2 = not_ok, 
					HC2 = true,
					Out3 = In3, !
				; % ok
					ResOut = ok,
					SFOut = [], % break iteration
					F2 = ok,
					HC2 = true,
					Out3 = In3
				),
				((In2 = true, ! ; HC1 = true, ! ; HC2 = true, !) -> Out2 = true ; Out2 = false)
		), 
		flatten(Handled, ToSchedule).


		
% checks whether the result of Now differs from the result of Last and Now = ExpectedNow. 
% - No until is allowed for Last and Now
% - The result can't be nondet
evaluate_changed(ToplevelFormula, FormulaPath, StartTime, Last, Now, ExpectedNow, Level, Result, ToSchedule) :-	
		append(FormulaPath, [2], SubPathNow),
		evaluate_formula(ToplevelFormula, SubPathNow, 0, 
			StartTime, StartTime, Now, Level, ResNow, _, _, _),
		(not ResNow = ExpectedNow ->
			Result = not_ok
			;
			append(FormulaPath, [1], SubPathLast),
			evaluate_formula(ToplevelFormula, SubPathLast, 0, 
				StartTime, StartTime, Last, Level, ResLast, _, _, _),
			(ResNow \= ResLast -> Result = ok ; Result = not_ok)
		),
		ToSchedule = Result.
		

% checks whether a persistent fluent's value has changed at Time		
evaluate_persistent_fluent_switched(Name, Time, Expected, Result) :-
		query_persistent_fluent(Name, CurrentState, LastChanged),
		((CurrentState = Expected, LastChanged =:= Time) ->
			Result = ok
			;
			Result = not_ok
		).
	
evaluate_action_occured(ActionTerm, Time, Result) :-
		ActionTerm =.. [Action | Params],
		get_action_clock(Action, Params, T2),
		(T2 is Time -> Result = ok ; Result = not_ok).

		

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
			KeyQ =.. [q, SubPathQ],
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


% Calculates the minimum among V1 and V2. Handles nondet.
getMin(V1, V2, Result) :-
	V1 = nondet, Result = V2, ! ;
	V2 = nondet, Result = V1, ! ;
	(V2 < V1 -> Result = V2 ; Result = V1).

getMax(V1, V2, Result) :-
	V1 = nondet, Result = V2, ! ;
	V2 = nondet, Result = V1, ! ;
	(V2 > V1 -> Result = V2 ; Result = V1).
			


% Evaluates F at the current time. If an Id is given, the result is stored in the list associated with the id
% and schedule params gathered during evaluation of F. 
% No schedule parameters are returned from here as the gathered params are "consumed".

evaluate_and_schedule(ToplevelFormula, FormulaPath, CurrentStep, StartTime, EndTime,
	F, CacheId, Level, 
	ScheduleIdIn, Result, ToScheduleOut, ScheduleIdOut, HasChanged) :-
		time(CurrentTime, do2(tick(CurrentStep), s0)),
		StartTime = StartTime, % TODO: for now StartTime isn't used, this could change...
		evaluate_formula(ToplevelFormula, FormulaPath, 
			CurrentStep, CurrentTime, EndTime, F, Level, Result, ToSchedule1, ScheduleParams, HasChanged1),
		% we cache in two cases: 1.) always if the result was nondet 2.) if result is not undet then only if changed
		((CacheId is -1, Result = nondet, ! ; not(CacheId is -1), HasChanged1 = true) ->		
			cache_formula(ToplevelFormula, FormulaPath, ToSchedule1, CacheId2), HasChanged2 = true
			; 
			CacheId2 is CacheId, HasChanged2 = false
		),	
		(
			(Result = nondet, 
				(ScheduleIdIn is -1 ->	
					incval(next_scheduled_goal_id),
					getval(next_scheduled_goal_id, ScheduleIdOut)
				;
					ScheduleIdOut is ScheduleIdIn
				),						
				ToSchedule2 = cf(CacheId2), 
				ToScheduleOut = cf(CacheId2),
				HasChanged3 = false,
				!
				; 
			not(Result = nondet), not(ScheduleIdIn is -1), 
				ToSchedule2 = Result, 
				ScheduleIdOut is ScheduleIdIn,
				ToScheduleOut = ToSchedule2,
				HasChanged3 = true,				
				!
			), 
			store_set(scheduled_goals, 
						sg(ToplevelFormula, Level, ScheduleIdOut, CurrentTime, EndTime), 
						app(ScheduleParams, ToSchedule2)
					), !
			
			;		
			ScheduleIdOut = -1,
			ToScheduleOut = ToSchedule1,
			HasChanged3 = false		
		),
		((HasChanged1 = true, !; HasChanged2 = true, ! ; HasChanged3 = true) -> 
				HasChanged = true, !
				;
			HasChanged = false
		).

% Adds the given formula to formula_cache. Before actually adding a new entry to formula_cache,
% we check if we've already have the same formula cached. To speed up search, we keep an extra 
% storage formula_cache_candidates that stores a list of cache ids for each formula position (path).
cache_formula(ToplevelFormula, FormulaPath, F, Id) :-
		(not ToplevelFormula = null ->
			Key =.. [ToplevelFormula | FormulaPath],
			(store_get(formula_cache_candidates, Key, Candidates), ! ; Candidates = []),
			(fromto(Candidates, In, Out1, []), fromto(-1, _, Out2, MatchingKey), param(F) do
				In = [CId | Rest],
				get_cached_formula(CId, CachedF),
				(CachedF = F -> 
					Out1 = [], 
					Out2 is CId
					;
					Out1 = Rest,
					Out2 is -1
				)
			)
			;
			MatchingKey is -1
		),
		(MatchingKey is -1 ->				
			incval(next_formula_cache_id),
			getval(next_formula_cache_id, Id),
			store_set(formula_cache, Id, F),
			(not ToplevelFormula = null ->
				Candidates2 = [Id | Candidates],
				store_set(formula_cache_candidates, Key, Candidates2)
				; true
			)
			;
			Id is MatchingKey
		).

get_cached_formula(Id, F) :-
		store_get(formula_cache, Id, F).

	
add_toplevel_goal(Name, F) :-
		cache_formula(Name, [0], F, Id),
		store_set(toplevel_goals, Name, cf(Id)).

		

get_toplevel_goals(Goals) :-
		stored_keys_and_values(toplevel_goals, L),
		(foreach(Entry, L), foreach(Goal, Goals) do
			Entry = Name - cf(CacheId),
			get_cached_formula(CacheId, F),
			Goal = Name : F
		).
		
print_toplevel_goals(Stream) :-
		stored_keys_and_values(toplevel_goals, L),
		nl,
		(foreach(Entry, L), param(Stream) do
			Entry = Name - cf(CacheId),
			get_cached_formula(CacheId, F),
			printf(Stream, "%10s %10d %w\n",[Name, CacheId, F])
		).


		
print_formula_cache(Stream) :-
		stored_keys_and_values(formula_cache, L),
		(foreach(Entry, L), param(Stream) do
			Entry = Id - F,
			printf(Stream, "%10d %w\n",[Id, F])
		).

print_cache_candidates(Stream):-
		stored_keys_and_values(formula_cache_candidates, L),
		(foreach(Entry, L), param(Stream) do
			Entry = Key - IdList,
			printf(Stream, "%w %w\n",[Key, IdList])
		).
		
% Evaluates all registered toplevel goals. 
% Results: list of Term with schema Name - Result
evaluate_toplevel(EndTime, Results) :-
		get_current(time, [], CurrentTime),
		stored_keys_and_values(toplevel_goals, L),
		setval(negated, 0),
		getval(current_failure_stack, CFS),
		(fromto(L, In, Out, []), fromto([], In2, Out2, Results), 
			param(CFS, CurrentTime, EndTime) do
			In = [Entry | Rest],
			Entry = Name - cf(CacheId),
			get_cached_formula(CacheId, F),
			record_create(MyFailureStack),
			setval(current_failure_stack, MyFailureStack),
			setval(negated, 0),
			% CurrentStep = 0
			% StartTime = CurrentTime
			evaluate_and_schedule(Name, [0], 0, CurrentTime, EndTime,			
				F, CacheId, 0, -1, R, _, _, _),
			(
				R = ok, append(In2, [ok : Name], Out2), !
				;
				R = not_ok, append(In2, [not_ok : Name], Out2), !
				;
				% nondet
				append(In2, [nondet : Name], Out2)
			),
			recorded_list(MyFailureStack, MyFSList),
			(foreach(FSL, MyFSList), param(CFS) do 
				record(CFS, FSL)
			),
			erase_all(MyFailureStack),
			Out = Rest
		),
		setval(current_failure_stack, CFS).
		

% Instantiates variables in sched - parts of F with schedule ids given in 
% Params. The ids in Params are identified by the corresponding position (path) in F.
apply_params(Params, F) :-
	(foreach(P, Params), param(F) do
		P = p(Path) : SchedId,
		get_subterm(F, Path, SubTerm),
		SubTerm = sched(_, Var, _),
		Var is SchedId		
	).

evaluate_scheduled(Key, EndTime, Result) :-
	Key = sg(ToplevelFormula, Level, Id, StartTime, OrigEndTime),
	store_get(scheduled_goals, Key, FRef),
	FRef = app(Params, F2),
	(F2 = cf(CacheId) -> get_cached_formula(CacheId, F) ; F = F2),
	apply_params(Params, F),
	setval(negated, 0),
	% CurrentStep = 0
	evaluate_formula(ToplevelFormula, [0], 0,
		StartTime, EndTime, F, Level, Result, ToSchedule, ScheduleParams2, HasChanged),
	(Result = nondet ->
		(HasChanged = true ->
			cache_formula(ToplevelFormula, [0], ToSchedule, CacheId2)
			;
			CacheId2 is CacheId
		),
		ToSchedule2 = cf(CacheId2)
		;
		ToSchedule2 = Result
	),
	(OrigEndTime =:= EndTime ->
		Key2 = Key
		;
		Key2 = sg(ToplevelFormula, Level, Id, StartTime, EndTime),
		store_delete(scheduled_goals, Key)
	),
	store_set(scheduled_goals, Key2, app(ScheduleParams2,ToSchedule2)).
	

check_state_filter(Content, StateFilter) :-
	StateFilter = all, !
	;
	StateFilter = nondet, !,
	Content \= ok, 
	Content \= not_ok
	; % ok / not_ok				
	StateFilter = Content.
	
get_scheduled_goals(ScheduledGoals, StateFilter, LevelFilter) :-
	stored_keys_and_values(scheduled_goals, L),
	(foreach(Entry, L), fromto([], In, Out, ScheduledGoals), param(StateFilter, LevelFilter) do
		Entry = Key - app(_, Content),
		Key = sg(_, Level, _, _, _),
		(  
			((LevelFilter = all, ! ; Level == LevelFilter),
				check_state_filter(Content, StateFilter) ) ->			 
				append(In, [Key], Out)
				;
				Out = In
		)		
	).	
	
get_pending_goals(PendingGoals, LevelFilter) :-
	get_scheduled_goals(PendingGoals, nondet, LevelFilter).
		
get_pending_toplevel_goals(PendingGoals) :-
	get_pending_goals(PendingGoals, 0).
	
% claim schedule ids when entering evaluation --> from outside to inside. in evaluation first sort and then evaluate in descending order.
% This makes sure that dependencies are resolved.
evaluate_all_scheduled(EndTime, Results) :-
	getval(current_failure_stack, CFS),
	get_pending_goals(PendingGoals, all),
	% sort by  1st argument = level
	sort(2, >=, PendingGoals, SortedKeys),
	(fromto(SortedKeys, In, Out, []), fromto([], In2, Out2, Results), 
		param(EndTime, CFS) do
		In = [Key | Rest],
		Key = sg(_, Level, _, _, _),
		record_create(MyFailureStack),
		setval(current_failure_stack, MyFailureStack),
		evaluate_scheduled(Key, EndTime, R),
		% only report properties with level 0
		(Level == 0 ->
			append(In2, [R : Key], Out2)
			;
			% don't report
			Out2 = In2
		),
		recorded_list(MyFailureStack, MyFSList),
		(foreach(FSL, MyFSList), param(CFS) do 
			record(CFS, FSL)
		),
		erase_all(MyFailureStack),
		Out = Rest	
	),
	setval(current_failure_stack, CFS).
	

% TODO: proper cleanup-procedure
print_scheduled_goals(Stream, SortPositions) :-
	stored_keys(scheduled_goals, Keys),
	sort(SortPositions, =<, Keys, SortedKeys),
	nl,
	printf(Stream, "%10s %10s %10s %5s %5s %10s %s\n",["Name", "Time","End Time", "Level","Id","Params","Term"]),
	printf(Stream, "-------------------------------------------------------\n",[]),
	(foreach(Key, SortedKeys), param(Stream) do
		Key = sg(Name, Level, Id, T, EndTime),
		store_get(scheduled_goals, Key, app(Params, F)),
		
		printf(Stream, "%10s %10d %10d %5d %5d %w %w\n",[Name, T, EndTime, Level, Id, Params, F])
	).


compile_persistent_fluents :-
	findall((N - F),persistent_fluent(N,F), L),
	(foreach(Entry, L) do
		Entry = Name - Formula,
		compile_formula(Formula, CompiledFormula),
		store_set(persistent_fluents, Name, CompiledFormula)
	).
	
% TODO: rename to persistent_property
query_persistent_fluent(Name, CurrentState, LastChanged) :-
	(state_dirty -> update_persistent_fluents ; true),
	internal_query_persistent_fluent(Name, CurrentState, LastChanged).
	
internal_query_persistent_fluent(Name, CurrentState, LastChanged) :-
	(store_get(persistent_fluent_states, Name, S) ->
		S = CurrentState : LastChanged 
		;
		CurrentState = nondet,
		LastChanged = -1
	).
update_persistent_fluents :-
	stored_keys_and_values(persistent_fluents, L),
	current_time(CurrentTime),		
	(foreach(Entry, L), param(CurrentTime) do
		Entry = Name - Formula,
		internal_query_persistent_fluent(Name, CurrentState, _),
		evaluate_formula(null, [0], 0,
			CurrentTime, CurrentTime, Formula, 0, Result, _, _, _),
		(Result \= CurrentState ->
			store_set(persistent_fluent_states, Name, Result : CurrentTime)
			;
			true
		)
	),
	set_state_dirty(false).

print_persistent_fluents :-
	findall((N - F),persistent_fluent(N,F), L),

	(foreach(Entry, L) do
		Entry = Name - Formula,
		query_persistent_fluent(Name, CurrentState, LastChanged),
		store_get(persistent_fluents, Name, CF),
		printf("%s = %8s (%10d)\nFormula: %w\n Compiled: %w\n\n",[Name, CurrentState, LastChanged, Formula, CF])
	).
	
		
		
evaluate_condition(GoalName, Params) :-
	(foreach(P, Params), foreach(P2, Params2) do
		P = zero -> P2 = 0 ; P2 = P
	),
	
	T =.. [GoalName | Params2],
	call(T).
		
% TODO: check if there's actually a difference
evaluate_function(FunctionName, Params) :-
	%(foreach(P, Params), foreach(P2, Params2) do
	%	P = zero -> P2 = 0 ; P2 = P
	%),
	Params2 = Params,
	
	T =.. [FunctionName | Params2],
	call(T).

% printlog(Format, Params) :-
	% printf(Format, Params).
	
printlog(_, _) :- true.	