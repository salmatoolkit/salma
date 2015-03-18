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
% stores a map of cache id / param terms to schedule ids
:- local store(goal_id_map).
% stored descriptors about schedule4d goals, including cache id reference
% and parameters
:- local store(scheduled_goal_descriptions).

:- local variable(next_scheduled_goal_id).
:- local store(persistent_fluents).
% structure: name - (current_value : last_changed)

:- local store(persistent_fluent_states).

:- local variable(negated, 0).
:- local variable(current_failure_stack, failurestack).
:- lib(hash).
:- lib(lists).
:- dynamic properties_unsynced/0.
:- [goal_schedule].
:- [property_evaluator_vartimesteps].
:- [property_evaluator_tempops].

init_smc :-
	store_erase(formula_cache),
	store_erase(toplevel_goals),
	store_erase(original_properties),
	store_erase(scheduled_goals),
	store_erase(goal_id_map),
	store_erase(scheduled_goal_descriptions),
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
	store_erase(goal_id_map),
	store_erase(scheduled_goal_descriptions),
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
		

		

% Returns the same result for all given start time intervals.
apply_unique_result(StartTimes, Result, Results) :-
	(foreach(Entry, StartTimes), foreach(R, Results),
		param(Result) do
		R = Entry : Result
	).	

negate_results(OrigResults, Results) :-
	(foreach(Entry, OrigResults), foreach(R, Results) do
		Entry = Interval : OldR,
		(OldR = nondet, !, NewR = nondet
		; OldR = ok, !, NewR = not_ok 
		; OldR = not_ok, !, NewR = ok
		),
		R = Interval : NewR
	).		
		
get_unanimous_result(OrigResults, Result) :-
	(fromto(OrigResults, In, Out, []), fromto(none, R1, R2, Result) do
		In = [First | Rest],
		First = _ : Res,
		(Res = nondet, !,
			Out = [], R2 = nondet
		; R1 = none, !,
			Out = Rest, R2 = Res
		; R1 = Res, !, 
			Out = Rest, R2 = R1
		; R1 \= none, R1 \= Res, !,
			Out = [], R2 = ambiguous
		)
	).
		

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
	CurrentStep, StartTimes, EndTime, 
	F, Level, Results, OverallResult,
	ToSchedule, ScheduleParams, HasChanged) :-	
		getval(current_failure_stack, CFS),
		record_create(MyFailures),
		setval(current_failure_stack, MyFailures),
		(
			member(F, [ok, not_ok]), 
			apply_unique_result(StartTimes, F, Results),
			OverallResult = F,				
			ToSchedule = F, ScheduleParams = [], HasChanged = false, !
			;
			F = not2(F2),
			% switch mode for recording failures
			flip_negated,
			append(FormulaPath, [1], SubPath),
			evaluate_formula(ToplevelFormula, SubPath, CurrentStep,
				StartTimes, EndTime, F2, Level, Res2, OvRes2,
				ToSchedule2, ScheduleParams2, HasChanged2),
			flip_negated,
			negate_results(Res2, Results),
			((OvRes2 = nondet, ! ; OvRes2 = ambiguous) -> 	
				OverallResult = OvRes2,
				ToSchedule = not2(ToSchedule2),
				ScheduleParams = ScheduleParams2,
				HasChanged = HasChanged2
				;
				(OvRes2 = ok -> OverallResult = not_ok ; OverallResult = ok),
				ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
			), !
			;
			F = all(Sf2), !,
			evaluate_checkall(ToplevelFormula, FormulaPath, 
				CurrentStep, StartTimes, EndTime, Sf2, Level, Results,
				OverallResult,
				OutVar2, ScheduleParams2, HasChanged2), 
			((OverallResult = nondet, ! ; OverallResult = ambiguous) -> 
				ToSchedule = all(OutVar2), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
			)
			;
			F = one(Sf2),
			evaluate_checkone(ToplevelFormula, FormulaPath, 
				CurrentStep, StartTimes, EndTime, Sf2, Level, 
				Results, OverallResult, OutVar2, ScheduleParams2, HasChanged2), 
			((OverallResult = nondet, ! ; OverallResult = ambiguous) -> 
				ToSchedule = one(OutVar2), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
			), !
			;
			F = c_(Sf2), !, % constraint
			evaluate_checkall(ToplevelFormula, FormulaPath, 
				CurrentStep, StartTimes, EndTime, Sf2, Level, 
				Results, OverallResult, _, _, _),
			((OverallResult = nondet, ! ; OverallResult = ambiguous) -> 
				throw(ambiguous_result_in_constraint(F))
				;
				ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
			)
			;			
			F = until(MaxTime, P, Q),
			evaluate_until(ToplevelFormula, FormulaPath, 
				CurrentStep, Level, StartTimes, EndTime, MaxTime, 
				P, Q, Results, OverallResult, 
				NewP, NewQ, ScheduleParams2, HasChanged2),
			((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
				ToSchedule = until(MaxTime, NewP, NewQ), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
			), !		
			;
			F = always(MaxTime, P),
			evaluate_always_or_eventually(ToplevelFormula, FormulaPath, always,
				CurrentStep, Level, StartTimes, EndTime, MaxTime, 
				P, Results, OverallResult, NewP, ScheduleParams2, HasChanged2),
			((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
				ToSchedule = always(MaxTime, NewP), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
			), !		
			;
			F = eventually(MaxTime, P),
			evaluate_always_or_eventually(ToplevelFormula, FormulaPath, eventually,
				CurrentStep, Level, StartTimes, EndTime, MaxTime, 
				P, Results, OverallResult, NewP, ScheduleParams2, HasChanged2),
			((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
				ToSchedule = eventually(MaxTime, NewP), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
			), !		
			;		
			F = invariant(P),
			evaluate_formula_for_interval(ToplevelFormula, FormulaPath, 
				always, CurrentStep, StartTimes, EndTime, P, Level, 
				Results, OverallResult, ToSchedule2, ScheduleParams2, HasChanged2),
			((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
				ToSchedule = invariant(ToSchedule2), 
				%ToSchedule = ToSchedule2, 
				HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
			), !
			;
			F = goal(P),
			evaluate_formula_for_interval(ToplevelFormula, FormulaPath, 
				eventually, CurrentStep, StartTimes, EndTime, P, Level, 
				Results, OverallResult, ToSchedule2, ScheduleParams2, HasChanged2),
			((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
				ToSchedule = goal(ToSchedule2), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
			), !
			;			
			F = let(Var : FreshVar, Def, Body),		
			evaluate_formula(ToplevelFormula, FormulaPath, 
				CurrentStep, StartTimes, EndTime, Def, Level, _, _, _, _, _),
			% only proceed if FreshVar has been bound in the previous evaluation
			(ground(FreshVar) ->
				subst_in_term(Var, FreshVar, Body, Body2),
				evaluate_formula(ToplevelFormula, FormulaPath, 
					CurrentStep, StartTimes, EndTime, Body2, 
					Level, Results, OverallResult, ToSchedule2, ScheduleParams2, _)
				; % TODO: should we throw an exception here?
				apply_unique_result(StartTimes, not_ok, Results),
				OverallResult = not_ok				
			),			
			((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
				ToSchedule = ToSchedule2, HasChanged = true, ScheduleParams = ScheduleParams2
				;
				ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
			), !
			;
			F = match(VarSpecs, Def, Body),
			evaluate_formula(ToplevelFormula, FormulaPath, 
				CurrentStep, StartTimes, EndTime, Def, Level, _, _, _, _, _),
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
					CurrentStep, StartTimes, EndTime,
					Body2, Level, Results, OverallResult, ToSchedule2, ScheduleParams2, _)
				;
				apply_unique_result(StartTimes, not_ok, Results),
				OverallResult = not_ok
			),
			((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
				ToSchedule = ToSchedule2, HasChanged = true, ScheduleParams = ScheduleParams2
				;
				ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
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
					 StartTimes, EndTime, P, Q, ExpectedNow, 
					 Level, Results),	
				!
				;
				F = pfswitch(PFName, ExpectedNow),
				evaluate_persistent_fluent_switched(PFName, StartTimes, 
					ExpectedNow, Results),
				!
				;
				F = occur(ActionTerm),
				% for occur, only the start time is relevant since only tick can occur
				% during a time advance phase
				evaluate_action_occurred(ActionTerm, StartTimes, Results),
				!
				;
				% is golog program possible?
				F = possible(CurrentStep, GologProg, InitSit),
				(do2(GologProg, InitSit, _) -> 
					apply_unique_result(StartTimes, ok, Results) ; 
					apply_unique_result(StartTimes, not_ok, Results)), !
				;
				% comparison or boolean fluent
				functor(F, Functor, _),
				(
					reifiable_op(Functor), !,
					test_reifiable(F, Res2)					
					;				
					(constraint_op(Functor, _), ! ; fluent(Functor, _, boolean), ! ;
						derived_fluent(Functor, _, boolean)),
					(call(F) -> Res2 = ok ; Res2 = not_ok), !
				), 
				apply_unique_result(StartTimes, Res2, Results), !
				;		
				% otherwise it's a function so call it to bind variables but don't use in schedule term
				call(F), apply_unique_result(StartTimes, ok, Results), !
			), !
		),
		get_unanimous_result(Results, OverallResult),
		ToSchedule = OverallResult,
		getval(negated, Negated),
		(foreach(Entry, Results), param(Negated, CFS, MyFailures,
			ToplevelFormula, FormulaPath, F, Level) do
			Entry = S(IStart, _) : R,
			((Negated = 0, R = not_ok ; Negated = 1, R = ok) ->
				record(CFS, ref(MyFailures)),
				record(CFS, failure(R, ToplevelFormula, FormulaPath, IStart, F, Level))
				;
				erase_all(MyFailures)
			)
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


and_resvector(R1, R2, RAnd, OverallResult) :-
	(foreach(E1, R1), foreach(E2, R2), foreach(EAnd, RAnd),
		fromto(none, OvResIn, OvResOut, OverallResult) do
		E1 = Intv : State1,
		(E2 = Intv : State2 -> 
			(State1 = ok, State2 = ok, !,
				EAnd = Intv : ok
			; (State1 = not_ok, ! ; State2 = not_ok), !,
				EAnd = Intv : not_ok
			; EAnd = Intv : nondet
			)
		; % interval doesn't match
			throw(different_intervals_and_resvector)
		),
		(OvResIn = none, !,
			OvResOut = EAnd
		; EAnd = nondet, !, 
			OvResOut = nondet
		; OvResIn = EAnd, !
			OvResOut = OvResIn
		; OvResOut = ambiguous
		)	
	).

or_resvector(R1, R2, ROr, OverallResult) :-
	(foreach(E1, R1), foreach(E2, R2), foreach(EOr, ROr),
		fromto(none, OvResIn, OvResOut, OverallResult) do
		E1 = Intv : State1,
		(E2 = Intv : State2 -> 
			(State1 = not_ok, State2 = not_ok, !,
				EAnd = Intv : not_ok
			; (State1 = ok, ! ; State2 = ok), !,
				EAnd = Intv : ok
			; EAnd = Intv : nondet
			)
		; % interval doesn't match
			throw(different_intervals_and_resvector)
		),
		(OvResIn = none, !,
			OvResOut = EAnd
		; EAnd = nondet, !, 
			OvResOut = nondet
		; OvResIn = EAnd, !
			OvResOut = OvResIn
		; OvResOut = ambiguous
		)		
	).
	
evaluate_checkall(ToplevelFormula, FormulaPath, 
	CurrentStep, StartTimes, EndTime, Subformulas, Level, 
	Results, OverallResult, ToSchedule, ScheduleParams, HasChanged) :- 
		%  check all subformulas. if result can be determined (ok, not_ok) only that result is stored 
		% TODO: "vector" result
		apply_unique_result(StartTimes, ok, ResInitial),
		(fromto(Subformulas, SFIn, SFOut, []), 
			foreach(F2, Handled), fromto(ResInitial, ResIn, ResOut, Results), 
			fromto(ok, OvResIn, OvResOut, OverallResult),
			fromto(false, In2, Out2, HasChanged), 
			fromto([], In3, Out3, ScheduleParams), count(I,1,_),
			param(CurrentStep, StartTimes, EndTime, Level, ToplevelFormula, FormulaPath) do
				% handle cases
				SFIn = [F | SFRest],
				append(FormulaPath, [1,I], SubPath),
				evaluate_formula(ToplevelFormula, SubPath, 
					CurrentStep, StartTimes, EndTime, F, Level, 
					Res2, _, Ts1, ScheduleParams2, HC1),
				and_resvector(ResIn, Res2, ResOut, OvRes2), 
				(OvRes2 = nondet, 
					OvResOut = nondet,
					SFOut = SFRest,
					F2 = Ts1, 
					HC2 = false,
					append(In3, ScheduleParams2, Out3), !
				; OvRes2 = ok,
					OvResOut = OvResIn,
					SFOut = SFRest,		
					F2 = ok, 
					Out3 = In3,
					HC2 = true, !
				; OvRes2 = not_ok, !,
					OvResOut = not_ok,
					SFOut = [], % break iteration
					F2 = not_ok,
					Out3 = In3,
					HC2 = true
				; % ambiguous
					OvResOut = ambiguous, !,
					SFOut = SFRest,
					F2 = Ts1, 
					HC2 = false,
					append(In3, ScheduleParams2, Out3)					
				),
		
				% update changed flag
				((In2 = true, ! ; HC1 = true, ! ; HC2 = true, !) -> Out2 = true ; Out2 = false)
		), 
		flatten(Handled, ToSchedule).
			

evaluate_checkone(ToplevelFormula, FormulaPath, 
	CurrentStep, StartTimes, EndTime, Subformulas, Level, 
	Results, OverallResult, ToSchedule, ScheduleParams, HasChanged) :- 
		%  check all subformulas. if result can be determined (ok, not_ok) only that result is stored 
		apply_unique_result(StartTimes, not_ok, ResInitial),
		(fromto(Subformulas, SFIn, SFOut, []), 
			foreach(F2, Handled), 
			fromto(ResInitial, ResIn, ResOut, Results), 
			fromto(not_ok, OvResIn, OvResOut, OverallResult),
			fromto(false, In2, Out2, HasChanged), 
			fromto([], In3, Out3, ScheduleParams), count(I,1,_),
			param(CurrentStep, StartTime, EndTime, Level, ToplevelFormula, FormulaPath) do
				% handle cases
				SFIn = [F | SFRest],
				append(FormulaPath, [1,I], SubPath),
				evaluate_formula(ToplevelFormula, SubPath, 
					CurrentStep, StartTimes, EndTime, F, Level, 
					Res2, _, Ts1, ScheduleParams2, HC1),
				or_resvector(ResIn, Res2, ResOut, OvRes2),
				(OvRes2 = nondet, 
					OvResOut = nondet,
					SFOut = SFRest,
					F2 = Ts1, 
					HC2 = false ,
					append(In3, ScheduleParams2, Out3), !
				; OvRes2 = not_ok,
					OvResOut = OvResIn,
					SFOut = SFRest,
					F2 = not_ok, 
					HC2 = true,
					Out3 = In3, !
				; OvRest2 = ok, !,
					OvResOut = ok,
					SFOut = [], % break iteration
					F2 = ok,
					HC2 = true,
					Out3 = In3
				; % ambiguous
					OvResOut = ambiguous, !,
					SFOut = SFRest,
					F2 = Ts1, 
					HC2 = false,
					append(In3, ScheduleParams2, Out3)			
				),
				((In2 = true, ! ; HC1 = true, ! ; HC2 = true, !) -> Out2 = true ; Out2 = false)
		), 
		flatten(Handled, ToSchedule).


		
% checks whether the result of Now differs from the result of Last and Now = ExpectedNow. 
% - No until is allowed for Last and Now
% - The result can't be nondet
evaluate_changed(ToplevelFormula, FormulaPath, 
	StartTimes, EndTime, Last, Now, 
	ExpectedNow, Level, Results) :-	
		% for now: only support one single step
		(
			StartTimes = [s(Start, Start)], !
			;
			throw(no_intervals_allowed_for_evaluate_changed)
		),
		append(FormulaPath, [2], SubPathNow),
		evaluate_formula(ToplevelFormula, SubPathNow, 0, 
			StartTimes, EndTime, Now, Level, 
			_, OvResNow, _, _, _),
		(not OvResNow = ExpectedNow ->
			apply_unique_result(StartTimes, not_ok, Results)
			;
			append(FormulaPath, [1], SubPathLast),
			evaluate_formula(ToplevelFormula, SubPathLast, 0, 
				StartTimes, EndTime, Last, Level, _, OvResLast, _ , _, _),
			(OvResNow \= OvResLast -> 
				Res2 = ok
				; 
				Res2 = not_ok
			),
			apply_unique_result(StartTimes, Res2, Results)			
		).
		

% checks whether a persistent fluent's value has changed at Time		
evaluate_persistent_fluent_switched(Name, StartTimes, Expected, Results) :-
	% for now: only support one single step
	(
		StartTimes = [s(Time, Time)], !
		;
		throw(no_intervals_allowed_for_evaluate_persistent_fluent_switched)
	),
	query_persistent_fluent(Name, CurrentState, LastChanged),
	((CurrentState = Expected, LastChanged =:= Time) ->
		Res = ok
		;
		Res = not_ok
	),
	apply_unique_result(StartTimes, Res, Results).
	
evaluate_action_occurred(ActionTerm, StartTimes, Results) :-
	% for now: only support one single step
	(
		StartTimes = [s(Time, Time)], !
		;
		throw(no_intervals_allowed_for_evaluate_action_occurred)
	),
	ActionTerm =.. [Action | Params],
	get_action_clock(Action, Params, T2),
	(T2 is Time -> Res = ok ; Res = not_ok),
	apply_unique_result(StartTimes, Res, Results).

		
action_occurred(ActionTerm, Sit) :-
	time(Time, Sit),
	evaluate_action_occurred(ActionTerm, Time, Result),
	Result = ok.


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
	ScheduleIdIn, OverallResult, ToScheduleOut, ScheduleIdOut, HasChanged) :-
		time(CurrentTime, do2(tick(CurrentStep), s0)),
		StartTimes = [s(StartTime, StartTime)], 
		evaluate_formula(ToplevelFormula, FormulaPath, 
			CurrentStep, StartTimes, EndTime, F, Level, _, 
			OverallResult, ToSchedule1, ScheduleParams, HasChanged1),
		% we cache in two cases: 1.) always if the result was nondet 2.) if result is not undet then only if changed
		((CacheId is -1, OverallResult = nondet, ! ; not(CacheId is -1), HasChanged1 = true) ->		
			cache_formula(ToplevelFormula, FormulaPath, ToSchedule1, CacheId2), HasChanged2 = true
			; 
			CacheId2 is CacheId, HasChanged2 = false
		),	
		(OverallResult = nondet, !,
			ToSchedule2 = cf(CacheId2), 
			ToScheduleOut = cf(CacheId2),
			HasChanged3 = false,
			(ScheduleIdIn is -1 ->	
				get_goal_schedule_id(ToplevelFormula, Level, ToSchedule2, ScheduleParams, 
					ScheduleIdOut)
			;
				ScheduleIdOut is ScheduleIdIn
			),
			add_nondet_schedule_interval(ScheduleIdOut, Level,
				StartTime, EndTime)			
		; not(OverallResult = nondet), not(ScheduleIdIn is -1), !,
			% already scheduled and determined now
			ToSchedule2 = OverallResult, 
			ScheduleIdOut is ScheduleIdIn,
			ToScheduleOut = ToSchedule2,
			HasChanged3 = true,
			apply_interval_decisions(ScheduleIdOut, Level, 
				[s(StartTime, StartTime) : OverallResult], 
				EndTime)	
		; % nothing to schedule here - go ahead
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
	%printf("AP: %w | %w\n", [Params, F]),
	(foreach(P, Params), param(F) do
		P = p(Path) : SchedId,
		get_subterm(F, Path, SubTerm),
		SubTerm = sched(_, Var, _),
		Var is SchedId		
	).

evaluate_scheduled(Goal, EndTime, Results, OverallResult) :-
	Goal = Key - StateVector,
	Key = g(Level, SchedId),
	StateVector = i(NondetIntervals, _, 
		_, _),
	get_scheduled_goal_description(SchedId, Description),
	Description = s(ToplevelFormula, _, Term, Params),
	
	(Term = cf(CacheId) -> get_cached_formula(CacheId, F) ; F = Term, CacheId = -1),
	apply_params(Params, F),
	setval(negated, 0),
	% CurrentStep = 0
	evaluate_formula(ToplevelFormula, [0], 0,
		NondetIntervals, EndTime, F, Level, Results, OverallResult,
		_, _, _),
	apply_interval_decisions(SchedId, Level, 
		Results, EndTime).
	
		

	
% claim schedule ids when entering evaluation --> from outside to inside. in evaluation first sort and then evaluate in descending order.
% This makes sure that dependencies are resolved.
evaluate_all_scheduled(EndTime, Results) :-
	getval(current_failure_stack, CFS),
	get_pending_goals(PendingGoals, all), % format: Entry = Key - StateVector, Key = g(Level, Id)
	% sort by level
	sort([1,1],, >=, PendingGoals, SortedGoals),
	(fromto(SortedGoals, In, Out, []), fromto([], In2, Out2, Results), 
		param(EndTime, CFS) do
		In = Key - _,
		Key = g(Level, SchedId),
		record_create(MyFailureStack),
		setval(current_failure_stack, MyFailureStack),
		evaluate_scheduled(In, EndTime, _, OvR),
		% only report properties with level 0
		(Level == 0 ->
			get_scheduled_goal_description(SchedId, s(ToplevelFormula, _, _, _)),
			append(In2, r(ToplevelFormula, OvR), Out2)
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