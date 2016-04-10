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

:- [interval_utils].
:- [goal_schedule].
:- [formula_cache].
:- [persistent_fluents].
:- [property_evaluator_vartimesteps].
:- [property_evaluator_tempops].

init_smc :-
        /* $D$ */ 
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
        /* $D(IGNORED)$ */ 
	getval(current_failure_stack, FStack),
	get_referenced_failures(FStack, Failures).
	
get_referenced_failures(Ref, Failures) :-
        /* $D(IGNORED)$ */ 
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
        /* $D$ */
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
        /* $D$ */
	(IsUnsynced = true ->
            /* $D$ */
		(not(properties_unsynced) -> /* $D$ */
                    assert(properties_unsynced) ; /* $D$ */ true)
		;
                /* $D$ */
		(properties_unsynced -> /* $D$ */
                    retract(properties_unsynced) ; /* $D$ */ true)
	).	

% erases all formula cache entries except the ones that are referenced by toplevel entries
% or scheduled goals.
clean_formula_cache :-
        /* $D$ */
	stored_keys_and_values(toplevel_goals, L),
	% first build list of entries to keep
	(foreach(Entry, L), foreach(E2, EntriesToKeep1) do
		/* $D$ */ Entry = _ - cf(E2)
	),
	
	stored_keys_and_values(scheduled_goals, L2),
	(foreach(Entry, L2), fromto([], In, Out, EntriesToKeep2) do
            /* $D$ */
		Entry = g(_, ScheduleId) - _,
		get_scheduled_goal_description(ScheduleId, Description),
		Description = s(_, _, Term, _),
		(Term = cf(CacheId) ->
                    /* $D$ */ append(In, [CacheId], Out)
                ;
                   /* $D$ */ Out = In
                )
	),	
	append(EntriesToKeep1, EntriesToKeep2, EntriesToKeep),
	stored_keys_and_values(formula_cache, L3),
	(foreach(CacheEntry, L3), param(EntriesToKeep) do
		/* $D$ */ CacheEntry = Id - _,
		(not member(Id, EntriesToKeep) ->
                    /* $D$ */ store_delete(formula_cache, Id)
                ;
                    /* $D$ */ true
                )
	),
	stored_keys_and_values(formula_cache_candidates, L4),
        (foreach(CacheCandidateEntry, L4), param(EntriesToKeep) do
            /* $D$ */ CacheCandidateEntry = FormulaPath - Candidates,
            % keep only top level 
            (FormulaPath =.. [_, 0] ->
                /* $D$ */ intersection(Candidates, EntriesToKeep, NewCandidates),
                store_set(formula_cache_candidates, FormulaPath, NewCandidates)			
            ; % not top level
                /* $D$ */ store_delete(formula_cache_candidates, FormulaPath)
            )
        ).
	
				
flip_negated :-
        /* $D$ */ 
	getval(negated, N),
	N2 is 1 - N,
	setval(negated, N2).

% Compiles and registers a property to check
register_property(Name, P, P2) :-
        /* $D$ */ 
	store_set(original_properties, Name, P),
	compile_formula(P, P2),
	add_toplevel_goal(Name, P2).

	
register_property_str(Name, PStr, P2) :-
        /* $D$ */ 
	term_string(P, PStr),
	register_property(Name, P, P2).

% Clears and rebuilds persistent fluents and toplevel formulas.
% This is mainly done in response to domain changes.
recompile_all :-
        /* $D$ */ 
	store_erase(persistent_fluents),
	store_erase(formula_cache_candidates),
	store_erase(toplevel_goals),
	compile_persistent_fluents,
	stored_keys_and_values(original_properties, Props),
	(foreach(P, Props) do
            /* $D$ */ 
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
	CurrentStep, StartTimes, EndTime, 
	F, Level, Results, OverallResult,
	ToSchedule, ScheduleParams, HasChanged) :-	
        /* $D$ */ 
        printlog("evaluate_formula: %w - %d - %w - %d - %w - %d\n",
                 [FormulaPath, CurrentStep, StartTimes, EndTime, F, Level]),
        getval(current_failure_stack, CFS),
        record_create(MyFailures),
        setval(current_failure_stack, MyFailures),
        (
            member(F, [ok, not_ok]), /* $D$ */ 
            apply_unique_result(StartTimes, F, Results),
            OverallResult = F,				
            ToSchedule = F, ScheduleParams = [], HasChanged = false, !
        ;
            F = not2(F2), /* $D$ */ 
            % switch mode for recording failures
            flip_negated,
            append(FormulaPath, [1], SubPath),
            evaluate_formula(ToplevelFormula, SubPath, CurrentStep,
                             StartTimes, EndTime, F2, Level, Res2, OvRes2,
                             ToSchedule2, ScheduleParams2, HasChanged2),
            flip_negated,
            negate_results(Res2, Results),
            ((OvRes2 = nondet, ! ; OvRes2 = ambiguous) -> 	
                /* $D$ */ 
                OverallResult = OvRes2,
                ToSchedule = not2(ToSchedule2),
                ScheduleParams = ScheduleParams2,
                HasChanged = HasChanged2
            ;
                /* $D$ */ 
                (OvRes2 = ok -> /* $D$ */ OverallResult = not_ok ; /* $D$ */ OverallResult = ok),
                ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
            ), !
        ;
            F = all(Sf2), !, /* $D$ */ 

            evaluate_checkall(ToplevelFormula, FormulaPath, 
                              CurrentStep, StartTimes, EndTime, Sf2, Level, Results,
                              OverallResult,
                              OutVar2, ScheduleParams2, HasChanged2), 
            ((OverallResult = nondet, ! ; OverallResult = ambiguous) -> 
                /* $D$ */ 
                ToSchedule = all(OutVar2), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
            ;
                /* $D$ */ 
                ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
            )
        ;
            F = one(Sf2), /* $D$ */ 

            evaluate_checkone(ToplevelFormula, FormulaPath, 
                              CurrentStep, StartTimes, EndTime, Sf2, Level, 
                              Results, OverallResult, OutVar2, ScheduleParams2, HasChanged2), 
            ((OverallResult = nondet, ! ; OverallResult = ambiguous) -> 
                /* $D$ */ 
                ToSchedule = one(OutVar2), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
            ;
                /* $D$ */ 
                ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
            ), !
        ;
            F = c_(Sf2), !, % constraint
            /* $D$ */ 

            evaluate_checkall(ToplevelFormula, FormulaPath, 
                              CurrentStep, StartTimes, EndTime, Sf2, Level, 
                              Results, OverallResult, _, _, _),
            ((OverallResult = nondet, ! ; OverallResult = ambiguous) -> 
                                /* ignored in covereage */
                throw(ambiguous_result_in_constraint(F))
            ;
                /* $D$ */ 
                ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
            )
        ;			
            F = until(MaxTime, P, Q), /* $D$ */
            evaluate_until(ToplevelFormula, FormulaPath, 
                           CurrentStep, Level, StartTimes, EndTime, MaxTime, 
                           P, Q, Results, OverallResult, 
                           NewP, NewQ, ScheduleParams2, HasChanged2),
            ((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
                /* $D$ */ 
                ToSchedule = until(MaxTime, NewP, NewQ), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
            ;
                /* $D$ */ 
                ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
            ), !		
        ;
            F = always(MaxTime, P), /* $D$ */ 
            evaluate_always_or_eventually(ToplevelFormula, FormulaPath, always,
                                          CurrentStep, Level, StartTimes, EndTime, MaxTime, 
                                          P, Results, OverallResult, NewP, ScheduleParams2, HasChanged2),
            ((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
                /* $D$ */ 
                ToSchedule = always(MaxTime, NewP), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
            ;
                /* $D$ */ 
                ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
            ), !		
        ;
            F = eventually(MaxTime, P), /* $D$ */ 
            evaluate_always_or_eventually(ToplevelFormula, FormulaPath, eventually,
                                          CurrentStep, Level, StartTimes, EndTime, MaxTime, 
                                          P, Results, OverallResult, NewP, ScheduleParams2, HasChanged2),
            ((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
                /* $D$ */ 
                ToSchedule = eventually(MaxTime, NewP), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
            ;
                /* $D$ */ 
                ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
            ), !		
        ;		
            F = invariant(P), /* $D$ */ 
            evaluate_formula_for_interval(ToplevelFormula, FormulaPath, 
                                          always, CurrentStep, StartTimes, EndTime, P, Level, 
                                          Results, OverallResult, ToSchedule2, ScheduleParams2, HasChanged2),
            ((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
                /* $D$ */ 
                ToSchedule = invariant(ToSchedule2), 
                HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
            ;
                /* $D$ */ 
                ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
            ), !
        ;
            F = goal(P), /* $D$ */ 
            evaluate_formula_for_interval(ToplevelFormula, FormulaPath, 
                                          eventually, CurrentStep, StartTimes, EndTime, P, Level, 
                                          Results, OverallResult, ToSchedule2, ScheduleParams2, HasChanged2),
            ((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
                /* $D$ */ 
                ToSchedule = goal(ToSchedule2), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
            ;
                /* $D$ */ 
                ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
            ), !
        ;
            F = let(Var : FreshVar, Def, Body), /* $D$ */ 
            evaluate_formula(ToplevelFormula, FormulaPath, 
                             CurrentStep, StartTimes, EndTime, Def, Level, _, _, _, _, _),
            % only proceed if FreshVar has been bound in the previous evaluation
            (ground(FreshVar) ->
                /* $D$ */ 
                subst_in_term(Var, FreshVar, Body, Body2),
                evaluate_formula(ToplevelFormula, FormulaPath, 
                                 CurrentStep, StartTimes, EndTime, Body2, 
                                 Level, Results, OverallResult, ToSchedule2, ScheduleParams2, _)
            ; % TODO: should we throw an exception here?
                /* $D$ */ 
                apply_unique_result(StartTimes, not_ok, Results),
                OverallResult = not_ok				
            ),			
            ((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
                /* $D$ */ 
                ToSchedule = ToSchedule2, HasChanged = true, ScheduleParams = ScheduleParams2
            ;
                /* $D$ */ 
                ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
            ), !
        ;
            F = match(VarSpecs, Def, Body), /* $D$ */ 
            evaluate_formula(ToplevelFormula, FormulaPath, 
                             CurrentStep, StartTimes, EndTime, Def, Level, _, _, _, _, _),
            (foreach(VarSpec, VarSpecs), fromto(Body, BodyIn, BodyOut, Body2), 
             fromto(true, GIn, GOut, GroundStat) do
                /* $D$ */ 
                VarSpec = Var : FreshVar,
                % only proceed if FreshVar has been bound in the previous evaluation
                (ground(FreshVar) ->
                    /* $D$ */ 
                    subst_in_term(Var, FreshVar, BodyIn, BodyOut),
                    GOut = GIn						
                ;
                    /* $D$ */ 
                    BodyOut = BodyIn,
                    GOut = false
                )
            ),
            (GroundStat = true ->
                /* $D$ */ 
                evaluate_formula(ToplevelFormula, FormulaPath, 
                                 CurrentStep, StartTimes, EndTime,
                                 Body2, Level, Results, OverallResult, ToSchedule2, ScheduleParams2, _)
            ;
                /* $D$ */ 
                apply_unique_result(StartTimes, not_ok, Results),
                OverallResult = not_ok
            ),
            ((OverallResult = nondet, ! ; OverallResult = ambiguous) ->
                /* $D$ */ 
                ToSchedule = ToSchedule2, HasChanged = true, ScheduleParams = ScheduleParams2
            ;
                /* $D$ */ 
                ToSchedule = OverallResult, HasChanged = true, ScheduleParams = []
            ), !
        ;
            /* $D$ */ 
            % for all remaining cases: restore original formula for Level > 0 or nondet.
            % store original formula as value for ToSchedule later
            shelf_create(orig/1, null, Shelf),
            shelf_set(Shelf,1,F),
            ScheduleParams = [],
            ( 
                F = changed(P, Q, ExpectedNow), /* $D$ */ 
                % for now ignore ToSchedule output of changed
                evaluate_changed(ToplevelFormula, FormulaPath, 
                                 StartTimes, EndTime, P, Q, ExpectedNow, 
                                 Level, Results),	
                !
            ;
                F = pfswitch(PFName, ExpectedNow), /* $D$ */ 
                evaluate_persistent_fluent_switched(PFName, StartTimes, 
                                                    ExpectedNow, Results),
                !
            ;
                F = occur(ActionTerm), /* $D$ */ 
                % for occur, only the start time is relevant since only tick can occur
                % during a time advance phase
                evaluate_action_occurred(ActionTerm, StartTimes, Results),
                !
            ;
                % is golog program possible?
                F = possible(CurrentStep, GologProg, InitSit), /* $D$ */ 
                (do2(GologProg, InitSit, _) -> 
                    apply_unique_result(StartTimes, ok, Results) ; 
                    apply_unique_result(StartTimes, not_ok, Results)), !
            ;
                /* $D$ */ 
                % comparison or boolean fluent
                functor(F, Functor, _),
                (
                    reifiable_op(Functor), !, /* $D$ */ 
                    test_reifiable(F, Res2)					
                ;				
		    /* $D$ */ 	
                    (constraint_op(Functor, _), ! /* $DS$ */ ; fluent(Functor, _, boolean), ! /* $DS$ */ ;
                     derived_fluent(Functor, _, boolean) /* $DS$ */ ),
                    (call(F) -> /* $D$ */ Res2 = ok ; /* $D$ */ Res2 = not_ok), !
                ), 
                apply_unique_result(StartTimes, Res2, Results), !
            ;		
                /* $D$ */ 
                % otherwise it's a function so call it to bind variables but don't use in schedule term
                call(F), apply_unique_result(StartTimes, ok, Results), !
            ), !,
            get_unanimous_result(Results, OverallResult),
            ToSchedule = OverallResult
        ),
        getval(negated, Negated),
        (foreach(Entry, Results), param(Negated, CFS, MyFailures,
                                        ToplevelFormula, FormulaPath,
                                        F, Level) do
            /* $D$ */ 
            Entry = s(IStart, _) : R,
            ((Negated = 0, R = not_ok ; Negated = 1, R = ok) ->
                /* $D$ */ 
                record(CFS, ref(MyFailures)),
                record(CFS, failure(R, ToplevelFormula, FormulaPath, IStart, F, Level))
            ;
                /* $D$ */ 
                erase_all(MyFailures)
            )
        ).




% Executes a reified version of the goal in F. This ensures that the domains of variables are not 
% reduced.
test_reifiable(F, Result) :-
        /* $D$ */ 
	F =.. [Op | Params],
	append(Params, [T], Params2),
	F2 =.. [Op | Params2],
        call(F2),
        (is_in_domain(1, T) -> /* $D$ */ Result = ok ; /* $D$ */ Result = not_ok).
	
% compiles and evaluates a formula ad hoc, i.e. without caching it		
evaluate_ad_hoc(F, Result) :-
	evaluate_ad_hoc(F, Result, s0).
		
evaluate_ad_hoc(F, Result, Situation) :-
	current_time(T),
	compile_formula(F, F2, Situation),
	erase_failure_stack,
	setval(negated, 0),
	evaluate_formula(null, [0], 0, 
		[s(T, T)], T, F2, 0, _, Result,
		_, _, _).

evaluate_ad_hoc_str(FStr, Result, Situation) :-
	term_string(F, FStr),
	evaluate_ad_hoc(F, Result, Situation).
	
		
test_ad_hoc(F, Situation) :-
		evaluate_ad_hoc(F, Result, Situation),
		Result = ok.

test_ad_hoc(F) :-
		test_ad_hoc(F, s0).



evaluate_checkall(ToplevelFormula, FormulaPath, 
                  CurrentStep, StartTimes, EndTime, Subformulas, Level, 
                  Results, OverallResult, ToSchedule, ScheduleParams, HasChanged) :- 
        %  check all subformulas. if result can be determined (ok, not_ok) only that result is stored 
        % TODO: "vector" result
        /* $D$ */ 
        apply_unique_result(StartTimes, ok, ResInitial),
        (fromto(Subformulas, SFIn, SFOut, []), 
         foreach(F2, Handled), fromto(ResInitial, ResIn, ResOut, Results), 
         fromto(ok, OvResIn, OvResOut, OverallResult),
         fromto(false, In2, Out2, HasChanged), 
         fromto([], In3, Out3, ScheduleParams), count(I,1,_),
         param(CurrentStep, StartTimes, EndTime, Level, ToplevelFormula, FormulaPath) do
            % handle cases
            /* $D$ */ 
            SFIn = [F | SFRest],
            append(FormulaPath, [1,I], SubPath),
            evaluate_formula(ToplevelFormula, SubPath, 
                             CurrentStep, StartTimes, EndTime, F, Level, 
                             Res2, _, Ts1, ScheduleParams2, HC1),
            and_resvector(ResIn, Res2, ResOut, OvRes2), 
            (OvRes2 = nondet, /* $D$ */ 
             OvResOut = nondet,
             SFOut = SFRest,
             F2 = Ts1, 
             HC2 = false,
             append(In3, ScheduleParams2, Out3), !
            ; OvRes2 = ok, /* $D$ */ 
              OvResOut = OvResIn,
              SFOut = SFRest,		
              F2 = ok, 
              Out3 = In3,
              HC2 = true, !
            ; OvRes2 = not_ok, !, /* $D$ */ 
              OvResOut = not_ok,
              SFOut = [], % break iteration
              F2 = not_ok,
              Out3 = In3,
              HC2 = true
            ; % ambiguous
             /* $D$ */ 
             OvResOut = ambiguous, !,
             SFOut = SFRest,
             F2 = Ts1, 
             HC2 = false,
             append(In3, ScheduleParams2, Out3)					
            ),
            
            % update changed flag
            ((In2 = true, ! ; HC1 = true, ! ; HC2 = true, !) -> /* $D$ */ Out2 = true ; /* $D$ */ Out2 = false)
        ), 
        flatten(Handled, ToSchedule).
			

evaluate_checkone(ToplevelFormula, FormulaPath, 
                  CurrentStep, StartTimes, EndTime, Subformulas, Level, 
                  Results, OverallResult, ToSchedule, ScheduleParams, HasChanged) :- 
        %  check all subformulas. if result can be determined (ok, not_ok) only that result is stored 
        /* $D$ */ 
        apply_unique_result(StartTimes, not_ok, ResInitial),
        (fromto(Subformulas, SFIn, SFOut, []), 
         foreach(F2, Handled), 
         fromto(ResInitial, ResIn, ResOut, Results), 
         fromto(not_ok, OvResIn, OvResOut, OverallResult),
         fromto(false, In2, Out2, HasChanged), 
         fromto([], In3, Out3, ScheduleParams), count(I,1,_),
         param(CurrentStep, StartTimes, EndTime, Level, ToplevelFormula, FormulaPath) do
            % handle cases
            /* $D$ */ 
            SFIn = [F | SFRest],
            append(FormulaPath, [1,I], SubPath),
            evaluate_formula(ToplevelFormula, SubPath, 
                             CurrentStep, StartTimes, EndTime, F, Level, 
                             Res2, _, Ts1, ScheduleParams2, HC1),
            or_resvector(ResIn, Res2, ResOut, OvRes2),
            (OvRes2 = nondet, /* $D$ */ 
             OvResOut = nondet,
             SFOut = SFRest,
             F2 = Ts1, 
             HC2 = false ,
             append(In3, ScheduleParams2, Out3), !
            ; OvRes2 = not_ok, /* $D$ */ 
              OvResOut = OvResIn,
              SFOut = SFRest,
              F2 = not_ok, 
              HC2 = true,
              Out3 = In3, !
            ; OvRes2 = ok, !, /* $D$ */ 
              OvResOut = ok,
              SFOut = [], % break iteration
              F2 = ok,
              HC2 = true,
              Out3 = In3
            ; % ambiguous
             /* $D$ */ 
             OvResOut = ambiguous, !,
             SFOut = SFRest,
             F2 = Ts1, 
             HC2 = false,
             append(In3, ScheduleParams2, Out3)			
            ),
            ((In2 = true, ! ; HC1 = true, ! ; HC2 = true, !) -> /* $D$ */ Out2 = true ; /* $D$ */ Out2 = false)
        ), 
        flatten(Handled, ToSchedule).


		
% checks whether the result of Now differs from the result of Last and Now = ExpectedNow. 
% - No until is allowed for Last and Now
% - The result can't be nondet
evaluate_changed(ToplevelFormula, FormulaPath, 
                 StartTimes, EndTime, Last, Now, 
                 ExpectedNow, Level, Results) :-	
        % for now: only support one single step
        /* $D$ */ 
        (
            StartTimes = [s(Start, Start)], /* $D$ */ !
        ;
            /* $D(IGNORED)$ */ 
            throw(no_intervals_allowed_for_evaluate_changed)
        ),
        append(FormulaPath, [2], SubPathNow),
        evaluate_formula(ToplevelFormula, SubPathNow, 0, 
                         StartTimes, EndTime, Now, Level, 
                         _, OvResNow, _, _, _),
        (not OvResNow = ExpectedNow ->
            /* $D$ */ 
            apply_unique_result(StartTimes, not_ok, Results)
        ;
            /* $D$ */ 
            append(FormulaPath, [1], SubPathLast),
            evaluate_formula(ToplevelFormula, SubPathLast, 0, 
                             StartTimes, EndTime, Last, Level, _, OvResLast, _ , _, _),
            (OvResNow \= OvResLast -> 
                /* $D$ */ 
                Res2 = ok
            ; 
                /* $D$ */ 
                Res2 = not_ok
            ),
            apply_unique_result(StartTimes, Res2, Results)			
        ).


% checks whether a persistent fluent's value has changed at Time		
evaluate_persistent_fluent_switched(Name, StartTimes, Expected,
                                    Results) :-
        /* $D$ */ 
        % for now: only support one single step
        (
            StartTimes = [s(Time, Time)], /* $D$ */ !
        ;
            /* $D(IGNORED)$ */ 
            throw(no_intervals_allowed_for_evaluate_persistent_fluent_switched)
        ),
	query_persistent_fluent(Name, CurrentState, LastChanged),
        ((CurrentState = Expected, LastChanged =:= Time) ->
            /* $D$ */ 
            Res = ok
        ;
	    /* $D$ */ 
            Res = not_ok
        ),
	apply_unique_result(StartTimes, Res, Results).
	
evaluate_action_occurred(ActionTerm, StartTimes, Results) :-
        % for now: only support one single step
        (
            StartTimes = [s(Time, Time)], /* $D$ */ !
        ;
            /* $D(IGNORED)$ */ 
            throw(no_intervals_allowed_for_evaluate_action_occurred)
        ),
        ActionTerm =.. [Action | Params],
        get_action_clock(Action, Params, T2),
        (T2 is Time -> /* $D$ */ Res = ok ; /* $D$ */ Res = not_ok),
        apply_unique_result(StartTimes, Res, Results).

		
% tests whether the given action has occurred in this
% situation
action_occurred(ActionTerm, Sit) :-
        /* $D$ */ 
        time(Time, Sit),
        evaluate_action_occurred(ActionTerm, 
                                 [s(Time, Time)], Results),
        get_unanimous_result(Results, ok).




% Evaluates F at the current time. If an Id is given, the result is stored in the list associated with the id
% and schedule params gathered during evaluation of F. 
% No schedule parameters are returned from here as the gathered params are "consumed".

evaluate_and_schedule(ToplevelFormula, FormulaPath, StartStep, StartTime, EndTime,
                      F, SitTerm, Level, ScheduleIdIn, CacheIdIn,
                      OverallResult, ScheduleIdOut, CacheIdOut, HasChanged) :-
        /* $D$ */ 
        shelf_create(orig/1, null, Shelf),
        shelf_set(Shelf,1,F),
        
        StartTimes = [s(StartTime, StartTime)], 
        evaluate_formula(ToplevelFormula, FormulaPath, 
                         StartStep, StartTimes, EndTime, F, Level, _, 
                         OverallResult, ToSchedule1, ScheduleParams, HasChanged1),
        
        % don't schedule invariants or goals
        (ToSchedule1 \= invariant(_), ToSchedule1 \= goal(_), !, /* $D$ */ 
         % we cache in two cases: 1.) always if the result was nondet 2.) if result is not undet then only if changed
         
         (CacheIdIn = new, !, /* $D$ */ 
          (OverallResult \= nondet ->
              /* $D$ */ 
              shelf_get(Shelf, 1, ToCacheRaw)
          ;
              /* $D$ */ 
              ToCacheRaw = ToSchedule1
          )
         ; OverallResult = nondet, (CacheIdIn = -1, ! ; HasChanged1 =
                                                                    true), !, /* $D$ */  
           ToCacheRaw = ToSchedule1
         ;
          /* $D$ */ 
          ToCacheRaw = none
         ),
         (ToCacheRaw \= none ->
             /* $D$ */ 
             % roll back situation replacement done in evaluate_for_all_timesteps
             subst_in_term(SitTerm, s0, ToCacheRaw, ToCache, [until, always, eventually]),
             cache_formula(ToplevelFormula, FormulaPath, ToCache, CacheIdOut), 
             HasChanged2 = true
         ;
             /* $D$ */ 
             CacheIdOut = CacheIdIn, HasChanged2 = false
         ),		
         (CacheIdOut \= -1 ->
             /* $D$ */ 
             RefTerm = cf(CacheIdOut)
         ;
             /* $D$ */ 
             RefTerm = OverallResult
         ),
         % create a new schedule entry if nondet or forced
         ( 
             (ScheduleIdIn = new, /* $D$ */ ! 
             ; ScheduleIdIn = -1, OverallResult = nondet, /* $D$ */ !)
         ->
             /* $D$ */ 
             HasChanged3 = true,
             get_goal_schedule_id(ToplevelFormula, Level, RefTerm, ScheduleParams, 
                                  ScheduleIdOut)
         ;
             /* $D$ */ 
             HasChanged3 = false,
             ScheduleIdOut = ScheduleIdIn
         ),
         (ScheduleIdOut \= -1 ->
             /* $D$ */ 
             (OverallResult = nondet ->
                 /* $D$ */ 
                 add_nondet_schedule_interval(ScheduleIdOut, Level,
                                              StartTime, EndTime)
             ;
                 /* $D$ */ 
                 apply_interval_decisions(ScheduleIdOut, Level, 
                                          [s(StartTime, StartTime) : OverallResult], EndTime)
             )
         
         ; % nothing to schedule here - go ahead
             /* $D$ */ 
             true
         ),
         ((HasChanged1 = true, /* $D$ */ ! ;
           HasChanged2 = true, /* $D$ */ ! ;
           HasChanged3 = true /* $DS$ */ ) ->
             /* $D$ */ 
             HasChanged = true
         ;
             /* $D$ */ 
             HasChanged = false
         )
        ; % don't schedule anything for invariant or goal 
	 /* $D$ */ 
         true
        ).


	
add_toplevel_goal(Name, F) :-
        /* $D$ */ 
        cache_formula(Name, [0], F, Id),
        store_set(toplevel_goals, Name, cf(Id)).

		

get_toplevel_goals(Goals) :-
        /* $D$ */ 
        stored_keys_and_values(toplevel_goals, L),
        (foreach(Entry, L), foreach(Goal, Goals) do
            /* $D$ */ 
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

		
% Evaluates all registered toplevel goals. 
% Results: list of Term with schema Name - Result
evaluate_toplevel(EndTime, Results) :-
        /* $D$ */ 
        get_current(time, [], CurrentTime),
        stored_keys_and_values(toplevel_goals, L),
        setval(negated, 0),
        getval(current_failure_stack, CFS),
        (fromto(L, In, Out, []), fromto([], In2, Out2, Results), 
         param(CFS, CurrentTime, EndTime) do
            /* $D$ */ 
            In = [Entry | Rest],
            Entry = Name - cf(CacheId),
            get_cached_formula(CacheId, F),
            record_create(MyFailureStack),
            setval(current_failure_stack, MyFailureStack),
            setval(negated, 0),
            % CurrentStep = 0
            % StartTime = CurrentTime
            evaluate_and_schedule(Name, [0], 0, CurrentTime, EndTime,			
                                  F, s0, 0, -1, CacheId, 
                                  R, _, _, _),
            (
                R = ok, /* $D$ */ 
                append(In2, [ok : Name], Out2), !
            ;
                R = not_ok, /* $D$ */ 
                append(In2, [not_ok : Name], Out2), !
            ;
                % nondet
                /* $D$ */ 
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
        /* $D$ */ 
        %printf("AP: %w | %w\n", [Params, F]),
        (foreach(P, Params), param(F) do
            /* $D$ */ 
            P = p(Path) : SchedId,
            get_subterm(F, Path, SubTerm),
            SubTerm = sched(_, Var, _),
            Var is SchedId		
        ).

evaluate_scheduled(Goal, EndTime, Results, OverallResult) :-
        /* $D$ */ 
        Goal = Key - StateVector,
        Key = g(Level, SchedId),
	StateVector = i(NondetIntervals, _, 
                        _, _),
        get_scheduled_goal_description(SchedId, Description),
        Description = s(ToplevelFormula, _, Term, Params),
        
        (Term = cf(CacheId) ->
            /* $D$ */ 
            get_cached_formula(CacheId, F)
        ; 
            /* $D$ */ 
            F = Term
        ),
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
        /* $D$ */ 
        getval(current_failure_stack, CFS),
        get_pending_goals(PendingGoals, all), % format: Entry = Key - StateVector, Key = g(Level, Id)
                                              % sort by level
        sort([1,1], >=, PendingGoals, SortedGoals),
        (foreach(Goal, SortedGoals), fromto([], RIn, ROut, Results), 
         param(EndTime, CFS) do
            /* $D$ */ 
            Goal = Key - _,
            Key = g(Level, SchedId),
            record_create(MyFailureStack),
            setval(current_failure_stack, MyFailureStack),
            evaluate_scheduled(Goal, EndTime, R, OvR),
            % only report properties with level 0
            (Level == 0 ->
                /* $D$ */ 
                get_scheduled_goal_description(SchedId, s(ToplevelFormula, _, _, _)),
                append(RIn, [r(ToplevelFormula, SchedId, R, OvR)], ROut)
            ;
	        /* $D$ */ 
                % don't report
                ROut = RIn
            ),
            recorded_list(MyFailureStack, MyFSList),
            (foreach(FSL, MyFSList), param(CFS) do 
                record(CFS, FSL)
            ),
            erase_all(MyFailureStack)
        ),
        setval(current_failure_stack, CFS).
	
		
evaluate_condition(GoalName, Params) :-
        /* $D$ */ 
        (foreach(P, Params), foreach(P2, Params2) do
            /* $D$ */ 
            P = zero -> /* $D$ */ P2 = 0 ; /* $D$ */ P2 = P
	),
        
	T =.. [GoalName | Params2],
        call(T).
		
% TODO: check if there's actually a difference
evaluate_function(FunctionName, Params) :-
        /* $D$ */ 
        T =.. [FunctionName | Params],
        call(T).

% printlog(format, params) :-
	% printf(format, params).
	
printlog(_, _) :- true.	