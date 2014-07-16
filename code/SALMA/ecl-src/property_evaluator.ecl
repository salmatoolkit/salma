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

% erases all formula cache entries except the ones that are referenced by toplevel entries.
clean_formula_cache :-
	stored_keys_and_values(toplevel_goals, L),
	% first build list of entries to keep
	(foreach(Entry, L), foreach(E2, EntriesToKeep) do
		Entry = _ - cf(E2)
	),
	stored_keys_and_values(formula_cache, L2),
	(foreach(CacheEntry, L2), param(EntriesToKeep) do
		CacheEntry = Id - _,
		(not member(Id, EntriesToKeep) ->
			store_delete(formula_cache, Id)
			;
			true
		)
	),
	stored_keys_and_values(formula_cache_candidates, L3),
	(foreach(CacheCandidateEntry, L3), param(EntriesToKeep) do
		CacheCandidateEntry = FormulaPath - Candidates,
		% keep only top level 
		(FormulaPath =.. [_, 0] ->
			intersection(Candidates, EntriesToKeep, NewCandidates),
			store_set(formula_cache_candidates, FormulaPath, NewCandidates)			
			; % not top level
			store_delete(formula_cache_candidates, FormulaPath)
		)
	).
	

result_and(Res1, Res2, Res3) :-
		(Res1 = not_ok ; Res2 = not_ok), Res3 = not_ok, !
		;
		Res1 = ok, Res2 = ok, Res3 = ok, !
		;
		Res3 = nondet.
		
result_or(Res1, Res2, Res3) :-
		(Res1 = ok ; Res2 = ok), Res3 = ok, !
		;
		Res1 = not_ok, Res2 = not_ok, Res3 = not_ok, !
		;
		Res3 = nondet.
				
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
		
	
% F: formula to evaluate
% InUntil: whether or not the subformula is part of a until operator
evaluate_formula(ToplevelFormula, FormulaPath, StartTime, F, Level, Result, 
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
			evaluate_formula(ToplevelFormula, SubPath, StartTime, F2, Level, Res2, ToSchedule2, ScheduleParams2, HasChanged2),
			flip_negated,
			(
				Res2 = nondet, Result = nondet ;
				Res2 = ok, Result = not_ok ;
				Res2 = not_ok, Result = ok
			),
			((Result = nondet, !; Level > 0) -> 	
				ToSchedule = not2(ToSchedule2),
				ScheduleParams = ScheduleParams2,
				HasChanged = HasChanged2
				;
				ToSchedule = Result, HasChanged = true, ScheduleParams = []
			), !
			;
			F = all(Sf2), 
			evaluate_checkall(ToplevelFormula, FormulaPath, StartTime, Sf2, Level, Result, OutVar2, ScheduleParams2, HasChanged2), 
			((Result = nondet, !; Level > 0) -> 
				ToSchedule = all(OutVar2), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = Result, HasChanged = true, ScheduleParams = []
			), !
			;
			F = one(Sf2),
			evaluate_checkone(ToplevelFormula, FormulaPath, StartTime, Sf2, Level, Result, OutVar2, ScheduleParams2, HasChanged2), 
			((Result = nondet, !; Level > 0) -> 
				ToSchedule = one(OutVar2), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
				;
				ToSchedule = Result, HasChanged = true, ScheduleParams = []
			), !
			;
		
			F = until(MaxTime, P, Q),
			evaluate_until(ToplevelFormula, FormulaPath, Level, StartTime, MaxTime, P, Q, Result, NewP, NewQ, ScheduleParams2, HasChanged2),
			((Result = nondet, ! ; Level > 0) ->
				ToSchedule = until(MaxTime, NewP, NewQ), HasChanged = HasChanged2, ScheduleParams = ScheduleParams2
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
				evaluate_changed(ToplevelFormula, FormulaPath, StartTime, P, Q, ExpectedNow, Level, Result, _),	
				!
				;
				F = pfswitch(PFName, ExpectedNow),
				evaluate_persistent_fluent_switched(PFName, StartTime, ExpectedNow, Result),
				!
				;
				F = occur(ActionTerm),
				evaluate_action_occured(ActionTerm, StartTime, Result),
				!
				;
				% is golog program possible?
				F = possible(GologProg, InitSit),
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
			
			((Result = nondet, ! ; Level > 0) ->
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
		evaluate_formula(null, [0], T, F2, 0, Result, _, _, _).

evaluate_ad_hoc_str(FStr, Result, Situation) :-
	term_string(F, FStr),
	evaluate_ad_hoc(F, Result, Situation).
	
		
test_ad_hoc(F, Situation) :-
		evaluate_ad_hoc(F, Result, Situation),
		Result = ok.

test_ad_hoc(F) :-
		test_ad_hoc(F, s0).

evaluate_checkall(ToplevelFormula, FormulaPath, StartTime, Subformulas, Level, Result, ToSchedule, ScheduleParams, HasChanged) :- 
		%  check all subformulas. if result can be determined (ok, not_ok) only that result is stored 
		% TODO break as soon as possible! Maybe use exit_block?
		(foreach(F, Subformulas), foreach(F2, Handled), fromto(ok, In, Out, Result), fromto(false, In2, Out2, HasChanged), 
			param(StartTime, Level, ToplevelFormula, FormulaPath), fromto([], In3, Out3, ScheduleParams), count(I,1,_) do
				% handle cases
				append(FormulaPath, [I], SubPath),
				evaluate_formula(ToplevelFormula, SubPath, StartTime, F, Level, Res2, Ts1, ScheduleParams2, HC1),
				((Res2 = nondet, ! ; Level > 0) -> 
					F2 = Ts1, 
					HC2 = false,
					append(In3, ScheduleParams2, Out3)
					; 
					F2 = [], 
					Out3 = In3,
					HC2 = true
				),
				result_and(In, Res2, Out),
				% update changed flag
				((In2 = true, ! ; HC1 = true, ! ; HC2 = true, !) -> Out2 = true ; Out2 = false)
		), 
		flatten(Handled, ToSchedule).
			

evaluate_checkone(ToplevelFormula, FormulaPath, StartTime, Subformulas, Level, Result, ToSchedule, ScheduleParams, HasChanged) :- 
		%  check all subformulas. if result can be determined (ok, not_ok) only that result is stored 
		% TODO break as soon as possible! Maybe use exit_block?
		(foreach(F, Subformulas), foreach(F2, Handled), fromto(not_ok, In, Out, Result), fromto(false, In2, Out2, HasChanged), 
			param(StartTime, Level, ToplevelFormula, FormulaPath), fromto([], In3, Out3, ScheduleParams), count(I,1,_) do
				% handle cases
				append(FormulaPath, [I], SubPath),
				evaluate_formula(ToplevelFormula, SubPath, StartTime, F, Level, Res2, Ts1, ScheduleParams2, HC1),
				((Res2 = nondet, ! ; Level > 0) -> 
					F2 = Ts1, 
					HC2 = false ,
					append(In3, ScheduleParams2, Out3)
					; 		
					F2 = [], 
					HC2 = true,
					Out3 = In3
				),
				result_or(In, Res2, Out),
				((In2 = true, ! ; HC1 = true, ! ; HC2 = true, !) -> Out2 = true ; Out2 = false)
		), 
		flatten(Handled, ToSchedule).


		
% checks whether the result of Now differs from the result of Last and Now = ExpectedNow. 
% - No until is allowed for Last and Now
% - The result can't be nondet
evaluate_changed(ToplevelFormula, FormulaPath, StartTime, Last, Now, ExpectedNow, Level, Result, ToSchedule) :-	
		append(FormulaPath, [2], SubPathNow),
		evaluate_formula(ToplevelFormula, SubPathNow, StartTime, Now, Level, ResNow, _, _, _),
		(not ResNow = ExpectedNow ->
			Result = not_ok
			;
			append(FormulaPath, [1], SubPathLast),
			evaluate_formula(ToplevelFormula, SubPathLast, StartTime, Last, Level, ResLast, _, _, _),
			(ResNow \= ResLast -> Result = ok ; Result = not_ok)
		),
		ToSchedule = Result.
		

% checks whether a persistent fluent's value has changed at Time		
evaluate_persistent_fluent_switched(Name, Time, Expected, Result) :-
		query_persistent_fluent(Name, CurrentState, LastChanged),
		((CurrentState = Expected, LastChanged is Time) ->
			Result = ok
			;
			Result = not_ok
		).
	
evaluate_action_occured(ActionTerm, Time, Result) :-
		ActionTerm =.. [Action | Params],
		get_action_clock(Action, Params, T2),
		(T2 is Time -> Result = ok ; Result = not_ok).
	
evaluate_until(ToplevelFormula, FormulaPath, Level, StartTime, MaxTime, P, Q, 
				Result, NewP, NewQ, ScheduleParams, HasChanged) :-
		NextLevel is Level + 1,
		getval(current_failure_stack, CFS),
		record_create(MyFailures),
		setval(current_failure_stack, MyFailures),
		shelf_create(pqres/5, null, Shelf),
		(
			shelf_set(Shelf, 1, P),
			shelf_set(Shelf, 2, Q),
			% changed = false
			shelf_set(Shelf, 4, false),
			%schedule params
			shelf_set(Shelf, 5, []),
			% also evaluate history: schedule for each time step and store in list. 
			
			% procedure:
			% - check p -> if not_ok result = not_ok ; if nondet, schedule and go on
			% - check q 
			% -- if ok: determine when it has become true --> t_q = starting point
			% --- verify that p had been true until t_q -> if not result is not_ok
			% --- for non-temporal formulas, this is just assumed if we find p=true now because 
			%     if p had not been true until now, we would not be here
			% -- if not_ok:
		    % --- just go on, it might become ok
			% - for both p and q: 
			% -- evaluate
			% -- if nondet: schedule goal with current time as starting point 
			% -- if a scheduled goal is evaluated to ok / not_ok, the scheduled formula is replaced by this goal
			%
			% we need a procedure that calculates the time when a goal has become true
			% - evaluate_and_schedule needs an output var for this time
			
			%e.g. scheduled(id)
			% -- scheduled at id: f(id,time) -> until(true, maxtime, all([xpos(rob1, _123, s0), _123 > 100]))
			
			append(FormulaPath, [2], SubPathP), % start with subterm 2 since first is max time
			append(FormulaPath, [3], SubPathQ),
			
			(
				% in this block we define EndP, the end point until P has been true. 
				% - not_ok means that beginning from the start time there was a sequence with length >= 1 with not_ok
				% - nondet means no ok is there yet.
				% - otherwise EndP >= StartTime
				% note that EndP can change in the following evaluation rounds
				% check if P is a scheduled subformula
				P = sched(_, PSchedId, PRefTerm) ->  
					% evaluate the formula of P for the current time and add also ok/not_ok to the id's history
					% - we might have a cached formula or a simple term (should currently be ok/not_ok in this case)
					(PRefTerm = cf(PCacheId) ->
						get_cached_formula(PCacheId, SubP)
						;
						SubP = PRefTerm,
						PCacheId is -1
					),
					
					evaluate_and_schedule(ToplevelFormula, SubPathP, SubP, PCacheId, NextLevel, PSchedId, _, ToScheduleP, _, HasChangedP), 
					
					% retrieve the successive sequence of "ok" entries in the schedule beginning from 
					% our starting point. Due to the call above, this also includes the current time point.
					
					getEndP(PSchedId, StartTime, EndP, LatestPossibleEndP)
					;
					% not scheduled yet so evaluate and possibly schedule with fresh id. don't force adding to history if ok/not_ok
					evaluate_and_schedule(ToplevelFormula, SubPathP, P, -1, NextLevel, -1, ResP, ToScheduleP, PSchedId, HasChangedP),
					(
						ResP = ok, current_time(EndP), LatestPossibleEndP = nondet, !
						; 
						% if not_ok we can stop only if we're at StartTime. Otherwise it might be that Q does fit.
						ResP = not_ok, 
						(
							current_time(T2),
							(T2 > StartTime ->
								EndP is T2 - 1
								;
								EndP = not_ok
							), 
							LatestPossibleEndP is T2 - 1
						), !
						; ResP = nondet, EndP = nondet, LatestPossibleEndP = nondet
					)
			),	
			(EndP = not_ok ->
				shelf_set(Shelf, 3, not_ok),
				record(MyFailures, until_fail_p(not_ok, ToplevelFormula, FormulaPath, StartTime, P, Level)),
				HasChangedQ = true
				;	
				% handle Q in a very similiar fashion as P
				(		
					% in this block we look for StartQ, the earliest time that Q was true
					Q = sched(_, QSchedId, QRefTerm) ->  
						% evaluate the formula of Q for the current time and add also ok/not_ok to the id's history
						(QRefTerm = cf(QCacheId) ->
							get_cached_formula(QCacheId, SubQ)
							;
							SubQ = QRefTerm,
							QCacheId is -1
						),
						evaluate_and_schedule(ToplevelFormula, SubPathQ,SubQ, QCacheId, NextLevel, QSchedId, _, ToScheduleQ, _, HasChangedQ),						
						getStartQ(QSchedId, StartTime, StartQ, EarliestPossibleStartQ)
						;
						% not scheduled yet so evaluate and possibly schedule with fresh id
						evaluate_and_schedule(ToplevelFormula, SubPathQ, Q, -1, NextLevel, -1, ResQ, ToScheduleQ, QSchedId, HasChangedQ),
						(
							ResQ = ok, 
							current_time(StartQ), 
							EarliestPossibleStartQ  = StartQ 
							; 
							ResQ = nondet, 
							StartQ = nondet,
							current_time(EarliestPossibleStartQ)
							;
							ResQ = not_ok,
							StartQ = nondet,
							current_time(T),
							EarliestPossibleStartQ is T + 1
						)
				),
				% determine top level outcome
				(
					% we handled EndP = not_ok above
					EndP = nondet ->
						% this actually only covers the case when P was nondet at start time
						shelf_set(Shelf, 3, nondet)
					;
						% we have EndP >= 0 now
						(
							% we can stop if EarliestPossibleStartQ is higher that P's actual max time bound, which is
							% set either by the fact that there was a not_ok in the history (calculated in getEndP) or 
							% by StartTime+ MaxTime
							IntervalEnd is StartTime + MaxTime,
							getMin(LatestPossibleEndP, IntervalEnd, LatestPossibleEndP2),
							(EarliestPossibleStartQ  > LatestPossibleEndP2 + 1 ->
								shelf_set(Shelf, 3, not_ok),
								record(MyFailures, until_timeot(not_ok, ToplevelFormula, FormulaPath, StartTime, Q, Level))
								;
								(StartQ = nondet -> 
									shelf_set(Shelf, 3, nondet)
									;
									% we have StartQ >= 0 now
									((StartQ =< EndP + 1, StartQ =< StartTime + MaxTime) ->
										shelf_set(Shelf, 3, ok)
										;
										shelf_set(Shelf, 3, nondet)
									)
								)
							)
						)
				)
			),
			
			(not PSchedId is -1 ->
				KeyP =.. [p, SubPathP],
				var(VarPSchedId),
				SParams1 = [KeyP : PSchedId],
				shelf_set(Shelf, 1, sched(KeyP, VarPSchedId, ToScheduleP)) 
				; 
				SParams1 = []
			),
			(not QSchedId is -1 ->
				KeyQ =.. [q, SubPathQ],
				var(VarQSchedId),
				append(SParams1, [KeyQ : QSchedId], SParams2),
				shelf_set(Shelf, 2, sched(KeyQ, VarQSchedId, ToScheduleQ)) 
				; 
				SParams2 = SParams1
			),
			((HasChangedP, !; HasChangedQ,!) -> shelf_set(Shelf, 4, true) ; true),
			shelf_set(Shelf, 5, SParams2),
			shelf_get(Shelf, 0, pqres(NewP, NewQ, Result, HasChanged, ScheduleParams))
		),
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
	
% Retrieve the earliest  "ok" entry in the schedule beginning from 
% our starting point. This also includes the current time point.
% - We also retrieve a EarliestPossibleStartQ that marks the earliest registered start point.
getStartQ(QSchedId, StartTime, StartQ, EarliestPossibleStartQ) :-
		stored_keys_and_values(scheduled_goals, List1),
		shelf_create(starts(nondet, nondet), Shelf),
		(foreach(Entry, List1), param(Shelf, QSchedId, StartTime) do
			((Entry = sg(_,_, QSchedId, T) - Goal, T >= StartTime) ->
				% if we find ok, check whether we've got a new real minimum. 
				(Goal = ok ->
					shelf_get(Shelf, 1, OldMin),
					getMin(T, OldMin, NewMin),
					shelf_set(Shelf, 1, NewMin)			
					;
					true
				),
				% If we didn't get not_ok, check whether we've found a new possible minimum
				(not(Goal = not_ok) ->
					shelf_get(Shelf, 2, OldMinPossible),
					getMin(T, OldMinPossible, NewMinPossible),
					shelf_set(Shelf, 2, NewMinPossible)
					;
					% skip if not_ok
					true
				)
				;
				% skip if key or time doesn't match or
				true
			)
		),
		shelf_get(Shelf, 1, StartQ),
		shelf_get(Shelf, 2, EarliestPossibleStartQ),
		shelf_abolish(Shelf).
		

		
% pre: ListIn is list of Elements (T : Goal), sorted ascending by T.
checkTimeSequence(ListIn, Result, LatestPossibleResult) :-
		flatten(ListIn, List2),
		sort(List2, List3),
		shelf_create(t(not_ok, nondet), Shelf),
		(fromto(List3, In, Out, []), param(Shelf) do
			In = [Head | Tail],
			Head = T : Goal,
			(Goal = app(_, ok),
				shelf_set(Shelf, 1, T),
				Out = Tail,
				!
				;
			Goal = app(_, not_ok),
				% result will be not_ok iff the shelf entry hasn't been overwritten yet == at first
				Out = [],
				T2 is T - 1,
				shelf_set(Shelf,2,T2),
				!
				;
			% nondet
				shelf_get(Shelf, 1, CurrentT),
				% only set to nondet for first entry
				(CurrentT = not_ok -> shelf_set(Shelf, 1, nondet) ; true),
				Out = []				
			)
		),
		shelf_get(Shelf,1,Result),
		shelf_get(Shelf,2,LatestPossibleResult),
		shelf_abolish(Shelf).
			
			
		

	
		
% Retrieve the successive sequence of "ok" entries in the schedule beginning from 
% our starting point. This also includes the current time point.
getEndP(PSchedId, StartTime, EndP, LatestPossibleEndP) :-
		stored_keys_and_values(scheduled_goals, List1),
		% select all for given Id >= StartTime
		(foreach(Entry, List1), foreach(Out, List2), param(PSchedId, StartTime) do
			% level can be ignored since we assume that each id is unique
			(Entry = sg(_,_, PSchedId, T) - Goal , T >= StartTime) -> Out = [T:Goal] ; Out = []
		),
		checkTimeSequence(List2, EndP, LatestPossibleEndP).
				
		


% Evaluates F at the current time. If an Id is given, the result is stored in the list associated with the id
% and schedule params gathered during evaluation of F. 
% No schedule parameters are returned from here as the gathered params are "consumed".

evaluate_and_schedule(ToplevelFormula, FormulaPath, F, CacheId, Level, 
	ScheduleIdIn, Result, ToScheduleOut, ScheduleIdOut, HasChanged) :-
		get_current(time, [], CurrentTime),
		evaluate_formula(ToplevelFormula, FormulaPath, CurrentTime, F, Level, Result, ToSchedule1, ScheduleParams, HasChanged1),
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
						sg(ToplevelFormula, Level, ScheduleIdOut, CurrentTime), 
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
		
print_toplevel_goals :-
		stored_keys_and_values(toplevel_goals, L),
		nl,
		(foreach(Entry, L) do
			Entry = Name - cf(CacheId),
			get_cached_formula(CacheId, F),
			printf("%10s %10d %w\n",[Name, CacheId, F])
		).


		
print_formula_cache :-
		stored_keys_and_values(formula_cache, L),
		(foreach(Entry, L) do
			Entry = Id - F,
			printf("%10d %w\n",[Id, F])
		).

print_cache_candidates :-
		stored_keys_and_values(formula_cache_candidates, L),
		(foreach(Entry, L) do
			Entry = Key - IdList,
			printf("%w %w\n",[Key, IdList])
		).
		
% Evaluates all registered toplevel goals. 
% Results: list of Term with schema Name - Result
evaluate_toplevel(Results) :-
		stored_keys_and_values(toplevel_goals, L),
		setval(negated, 0),
		getval(current_failure_stack, CFS),
		(fromto(L, In, Out, []), fromto([], In2, Out2, Results), param(CFS) do
			In = [Entry | Rest],
			Entry = Name - cf(CacheId),
			get_cached_formula(CacheId, F),
			record_create(MyFailureStack),
			setval(current_failure_stack, MyFailureStack),
			setval(negated, 0),
			evaluate_and_schedule(Name, [0], F, CacheId, 0, -1, R, _, _, _),
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
		

		
apply_params(Params, F) :-
	(foreach(P, Params), param(F) do
		P = p(Path) : SchedId,
		get_subterm(F, Path, SubTerm),
		SubTerm = sched(_, Var, _),
		Var is SchedId		
	).

evaluate_scheduled(Key, Result) :-
	Key = sg(ToplevelFormula, Level, _, T),
	store_get(scheduled_goals, Key, FRef),
	FRef = app(Params, F2),
	(F2 = cf(CacheId) -> get_cached_formula(CacheId, F) ; F = F2),
	apply_params(Params, F),
	setval(negated, 0),
	evaluate_formula(ToplevelFormula, [0], T, F, Level, Result, ToSchedule, ScheduleParams2, HasChanged),
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
	store_set(scheduled_goals, Key, app(ScheduleParams2,ToSchedule2)	).
	

	
get_pending_goals(PendingGoals, LevelFilter) :-
	stored_keys_and_values(scheduled_goals, L),
	(foreach(Entry, L), fromto([], In, Out, PendingGoals), param(LevelFilter) do
		Entry = Key - app(_, Content),
		Key = sg(_, Level, _, _),
		( ( (LevelFilter = all, ! ; Level == LevelFilter),
			Content \= ok, Content \= not_ok
			) ->
			append(In, [Key], Out)
			;
			Out = In
		)
	).	
		
get_pending_toplevel_goals(PendingGoals) :-
	get_pending_goals(PendingGoals, 0).
	
% claim schedule ids when entering evaluation --> from outside to inside. in evaluation first sort and then evaluate in descending order.
% This makes sure that dependencies are resolved.
evaluate_all_scheduled(Results) :-
	getval(current_failure_stack, CFS),
	get_pending_goals(PendingGoals, all),
	% sort by  1st argument = level
	sort(2, >=, PendingGoals, SortedKeys),
	(fromto(SortedKeys, In, Out, []), fromto([], In2, Out2, Results), param(CFS) do
		In = [Key | Rest],
		Key = sg(_, Level, _, _),
		record_create(MyFailureStack),
		setval(current_failure_stack, MyFailureStack),
		evaluate_scheduled(Key, R),
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
	

% TODO: cleanup-procedure
% TODO: reuse compiled terms by referencing if nothing changed in evaluation
print_scheduled_goals(SortPositions) :-
	stored_keys(scheduled_goals, Keys),
	sort(SortPositions, =<, Keys, SortedKeys),
	nl,
	printf("%10s %10s %5s %10s %10s %s\n",["Name", "Time","Level","Id","Params","Term"]),
	printf("-------------------------------------------------------\n",[]),
	(foreach(Key, SortedKeys) do
		Key = sg(Name, Level, Id, T),
		store_get(scheduled_goals, Key, app(Params, F)),
		
		printf("%10s %10d %5d %10d %w %w\n",[Name, T, Level, Id, Params, F])
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
		evaluate_formula(null, [0], CurrentTime, Formula, 0, Result, _, _, _),
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
