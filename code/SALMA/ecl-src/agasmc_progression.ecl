:- local store(sit0).
:- local store(sit1).
:- local store(sit2).
:- local array(situations(3)).
:- local store(fluent_change_times).

:- local variable(cur_sit).
:- local variable(next_sit).
:- local variable(last_sit).

% :- local initialization(init).


% primitive_action(name, params=[name:sort,...])
:- dynamic primitive_action/2.
% declares that a primitive action is immediate
:- dynamic immediate_action/1.
:- dynamic stochastic_action/3.
% exogenous_action(name, qualifying-params, augmenting-params)
:- dynamic exogenous_action/3.
:- dynamic exogenous_action_choice/3.
:- dynamic ad_hoc_event/2.
:- dynamic schedulable_event/2.
:- dynamic caused_event/1.


:- dynamic last_initialized/0.
% clock for primitive and exogenous actions
% action_clock(name, params, clock_value)
:- dynamic action_clock/3.
:- dynamic action_count/3.

:- dynamic persistent_fluent/2.	
:- dynamic state_dirty/0.

:- dynamic time/2.

:- [axiom_construction].
:- [event_scheduling].

% action declaration
primitive_action(tick,[steps:integer]).
primitive_action(nop,[]).
immediate_action(nop).

fluent(time,[], integer).
untracked_fluent(time).

effect(time, tick(Steps), TOld, T, _) :-
	T is TOld + Steps.
	
poss(tick(_), _) :- true.
poss(nop, _) :- true.


last_initialized :- fail.
state_dirty :- true.

get_situation_store(SitName, S) :-
	getval(SitName, Index),
	getval(situations(Index), S).
	

make_key_term(Fluent, Params, T):-
    append([fl, Fluent], Params, TList),
    T =.. TList.

	
get_last_change_time(Fluent, Params, Time) :-
	make_key_term(Fluent, Params, T),
	store_get(fluent_change_times, T, Time), ! ; Time = -1.
	
get_current(Fluent, Params, Val):-
    get_situation_store(cur_sit, S), 
    make_key_term(Fluent, Params, T),
    (store_get(S, T, Val), ! ; 
	Fluent = domain, 
	Val = []).

is_undefined(Fluent, Params) :-
	get_situation_store(cur_sit, S), 
    make_key_term(Fluent, Params, T),
	not store_contains(S, T).

get_fluent_value(Fluent, Params, Value, Sit) :-
	(fluent(Fluent, _, Type), ! ; derived_fluent(Fluent, _, Type), !),
	(Type = boolean ->
		append(Params, [Sit], Params2),
		T =.. [Fluent | Params2],
		(call(T) -> Value = true ; Value = false)
		;
		append(Params, [Value, Sit], Params2),
		T =.. [Fluent | Params2],
		(call(T) -> true ; Value = none)
	), !
	;
	throw(undefined_fluent(Fluent)).

get_all_fluent_values(KeyValuePairs, Situation) :-
	store_create(Store),
	findall(F,fluent(F,_,_),L),
    (foreach(F2, L), param(Situation, Store) do
		not (
			choose_arguments(F2, Args),
			fluent(F2,_,RType),
			make_key_term(F2, Args, Key),
			(RType = boolean -> 
				append(Args, [Situation], AllArgs),
				T =.. [F2 | AllArgs],
				(call(T) -> store_set(Store, Key, true) 
					; store_set(Store, Key, false) )
				;
				append(Args, [Val, Situation], AllArgs),
				T =.. [F2 | AllArgs],
				call(T),
				store_set(Store, Key, Val) 
			),
			fail
		)
	),
	stored_keys_and_values(Store, KeyValuePairs).
		
		
current_time(T) :-
	get_current(time, [], T).

get_last(Fluent, Params, Val):-
	% todo: throw exception if last_sit == null
	get_situation_store(last_sit, S), 
    make_key_term(Fluent, Params, T),
    (store_get(S, T, Val), ! ; 
	Fluent = domain, 
	Val = []).	

fluent_has_changed(Fluent, OldValue, NewValue) :-
	(Fluent = domain, !,
		sort(OldValue, D1),
		sort(NewValue, D2),
		D1 \= D2
		;
	number(OldValue), number(NewValue), !,
		OldValue =\= NewValue
		;
	OldValue \= NewValue
	).
		


		
% callable from python so zero has to be handled due to PyCLP problems
set_current(Fluent, Params, Val):-
    get_situation_store(cur_sit, S), 
    make_key_term(Fluent, Params, T),
	subst_in_term(zero, 0, Val, Val2),
    store_set(S, T, Val2),
	
	((Fluent = domain, Params = [Sort], dynamic_sort(Sort), ! ; not untracked_fluent(Fluent)), 
		(store_get(S, T, CurrentValue), ! ; CurrentValue = none),
		fluent_has_changed(Fluent, CurrentValue, Val2) ->
			(store_get(S, fl(time), Time), ! ; Time = 0),
			store_set(fluent_change_times, T, Time),
			set_state_dirty(true),
			(Fluent = domain ->
				set_properties_unsynced(true),
				set_sort_hierarchy_unsynced(true)
				;
				true
			)				
		;
		true
	).

set_next(Fluent, Params, Val):-
    get_situation_store(next_sit, S), 
    make_key_term(Fluent, Params, T),
	subst_in_term(zero, 0, Val, Val2),
    store_set(S, T, Val2),
	% has changed?
	% TODO: 
	((Fluent = domain, Params = [Sort], dynamic_sort(Sort), ! ; not untracked_fluent(Fluent)),
		get_situation_store(cur_sit, CS),
		(store_get(CS, T, CurVal), ! ; CurVal = none),
		fluent_has_changed(Fluent, CurVal, Val2) ->
			(store_get(CS, fl(time), Time), ! ; Time = 0),
			store_set(fluent_change_times, T, Time),
			(Fluent = domain ->
				set_properties_unsynced(true),
				set_sort_hierarchy_unsynced(true)
				;
				true
			)		
		;
			true
	).
		
	


get_action_clock(Action, Params, T) :-
	(action_clock(Action, Params, T) -> true ; T is -1).

get_action_count(Action, Params, Count) :-
	(action_count(Action, Params, Count) -> true ; Count is 0).

% retrieves a description about the currently stored situations (current + next)
get_situations(S1, List1, S2, List2, S3, List3):-
	get_situation_store(last_sit, S1),
    stored_keys_and_values(S1, List1),
    get_situation_store(cur_sit, S2),
    stored_keys_and_values(S2, List2),
    get_situation_store(next_sit, S3),
    stored_keys_and_values(S3, List3).
	

last_state(S1, List1):-
    get_situation_store(last_sit, S1),
    stored_keys_and_values(S1, List1).



	
current_state(S1, List1):-
    get_situation_store(cur_sit, S1),
    stored_keys_and_values(S1, List1).

next_state(S1, List1):-
    get_situation_store(next_sit, S1),
    stored_keys_and_values(S1, List1).
	
advance_pointer(P) :-
	getval(P, V1),
	V2 is V1 + 1,
	mod(V2, 3, V3),
	setval(P, V3).
	


check_action(ActionTerm, Pattern, S) :-
	% unify
	% check poss
	Pattern = ActionTerm,
	% if unification worked then
	(poss(ActionTerm, S) ->
		true
		;
		% store failed action term in store for later reporting
		% need to include agent in action term
		record(failed_actions, ActionTerm),
		fail
	).
	
	
% check if the given sort is dynamic 
% and its domain has changed in this time step
domain_has_changed(Sort) :-
	dynamic_sort(Sort),
	get_last_change_time(domain, [Sort], T1),
	current_time(T2),
	T1 =:= T2.
	
	
% check if a domain of one of the dynamic sorts has changed
changed_domains(Sorts) :-
	findall(S, domain_has_changed(S), Sorts),
	not length(Sorts, 0).	
	

% Constructs situation and calculate the update value by regression.
% 
% Actions contains a list of action terms.
progress(Actions):-
	get_situation_store(next_sit, S), 
	% TODO: optimize?
	store_erase(S),
	set_state_dirty(true),
	(sort_hierarchy_unsynced -> init_sort_hierarchy(_) ; true),
	(properties_unsynced -> recompile_all ; true),
	% invalidate last situation (= next situation) since it is going to be manipulated
	get_current(time,[],Time),
	create_situation(Actions, Time, s0, Situation),
    findall(F,fluent(F,_,_),L),
    ( 
      foreach(F2, L),
      param(Situation)
    do
    (
     not (
	  choose_arguments(F2, Args),
	  fluent(F2,_,RType),
	  (
	   RType = boolean -> 
	   append(Args, [Situation], AllArgs),
	   T =.. [F2 | AllArgs],
	   (
	    call(T) -> set_next(F2, Args, true) ; set_next(F2, Args, false)
	   )
	  ;
	   append(Args, [Val, Situation], AllArgs),
	   T =.. [F2 | AllArgs],
	   call(T),
	   set_next(F2, Args, Val)
	  ),
	  fail
	 )
	 )
    ),
	%trace_point_port(secp, Invoc, second_part),
	% switch stores cyclically
	% advance last_sit only beginning from the second progression 
	%catch((
	(not last_initialized -> assert(last_initialized) ; advance_pointer(last_sit) ),
	advance_pointer(cur_sit),
	advance_pointer(next_sit),
	(sort_hierarchy_unsynced -> init_sort_hierarchy(_) ; true),
	(properties_unsynced -> recompile_all ; true).
	%), T, throw(gen_error(T))).	
 

progress_sequential(ActionSequence, FailedActions) :-
% progress with poss checks
	(foreach(Act, ActionSequence), fromto([], In, Out, FailedActions) do
		(poss(Act, s0) ->
			progress([Act]),
			Out = In
			;
			append(In, [Act], Out)
		)
	),
	clean_message_specs.
 

% TODO: optionally add constraints to ensure that all args are different? 
choose_arguments(Fluent, Arguments) :-
    fluent(Fluent,Args,_),
    (
     foreach(InArg,Args),
     foreach(OutArg, Arguments)
    do
    (
	 InArg = _ : InArgSort,
     domain(InArgSort, Entities),
     member(E, Entities),
     OutArg = E
    )).

set_state_dirty(IsDirty) :-
	(IsDirty = true ->
		(not(state_dirty) -> assert(state_dirty) ; true)
		;
		(state_dirty -> retract(state_dirty) ; true)
	).
		
	

init_progression :- 
	setval(situations(0), sit0),
	setval(situations(1), sit1),
	setval(situations(2), sit2),
	(last_initialized -> retract(last_initialized) ; true),
	set_state_dirty(true),
	store_erase(sit0), 
	store_erase(sit1), 
	store_erase(sit2), 
	store_erase(fluent_change_times),
    setval(cur_sit, 0), 
	setval(next_sit, 1), 
	setval(last_sit, 0),
	retractall(action_clock(_,_,_)),
	retractall(action_count(_,_,_)),
	construct_ssas,
	close_successor_state_axioms,
	init_event_scheduling.

	



% Checks recursively whether the given term could possibly affected by the given
% action instance.
term_affected_by_action(Term, Action) :-
	% if Term is fluent: check effect directly
	% if Term is derived fluent: check body of definition
	not var(Term),	
	Term =.. [Functor | Args],
	(
		fluent(Functor, _, Type), !,
		(Type = boolean ->
			L2 is length(Args) - 2
			; % not boolean -> result argument present
			L2 is length(Args) - 3
		),
		(L2 >= 0 ->
			sublist(Args, 0, L2, Args2)
			;
			Args2 = []
		),
		Term2 =.. [Functor | Args2],
		clause(effect(Term2, Action, _, _, _), _)
		;
		derived_fluent(Functor, _, _), !,
		(clause(Term, DFluentBody) -> 
			term_affected_by_action(DFluentBody, Action)
			;
			true
		)
		;
		member(Functor, [evaluate_ad_hoc, test_ad_hoc]),
		Args = [Formula | _],
		compile_formula(Formula, Formula2, _),
		term_affected_by_action(Formula2, Action), !
		;		
		member(Arg, Args),
		term_affected_by_action(Arg, Action), !
	).
		

	
create_situation([A], Time, S1, S2) :-
	A =.. [Act | RawParams],
	(foreach(RP, RawParams), foreach(P, Params) do
		subst_in_term(zero, 0, RP, P)
	),
	A2 =.. [Act | Params],
	(retract(action_clock(Act, Params, _)), ! ; true),
	assert(action_clock(Act, Params , Time)),
	get_action_count(Act, Params, OldCount),
	NewCount is OldCount + 1,
	(retract(action_count(Act, Params, _)), ! ; true),
	assert(action_count(Act, Params , NewCount)),
	S2 = do2(A2, S1), !.
		
create_situation([A | Tl], Time, S1, S2) :-
	create_situation([A], Time, S1, S3),
	create_situation(Tl, Time, S3, S2).

situation2action_list(s0, []).
situation2action_list(do2(A,S), L) :-
	situation2action_list(S, L1),
	append(L1, [A], L).

situation([A], S1, S2) :-
	S2 = do2(A, S1), !.

situation([A | Tl], S1, S2) :-
	situation([A], S1, S3),
	situation(Tl, S3, S2).

situation([A | Tl], S2) :- situation([A | Tl], s0, S2).

unfold_var_time_steps(S1, S2) :-
	S1 = do2(tick(Delta), SOld), !,
	(
		Delta =< 0,
		S2 = SOld, !
		;
		Delta = 1,
		S2 = S1, !
		; %	Delta > 1
		Delta2 is Delta - 1,
		unfold_var_time_steps(do2(tick(Delta2), do2(tick(1), SOld)), S2)
	)
	;
	S2 = S1.
	
	

% The following section contains functions that are used by the simulation engine
% for automatic initialization of the domain.
	
	
get_declared_fluents(Fluents) :-
	findall(f(FName,Params,Type),fluent(FName,Params,Type),Fluents).
	
get_declared_derived_fluents(Fluents) :-
	findall(f(FName,Params,Type),derived_fluent(FName,Params,Type),Fluents).

get_declared_constants(Constants) :-
	findall(c(CName,Params,Type),constant(CName,Params,Type),Constants).
	
retract_constant(ConstantName, Params) :-
	append(Params, [_], Params2),
	Head =.. [ConstantName | Params2],
	retractall(Head).
	
	
get_declared_primitive_actions(Actions) :-
	findall(pa(AName,Params),primitive_action(AName,Params),Actions).
get_declared_stochastic_actions(Actions) :-
	findall(pa(AName,Params,Outcomes),stochastic_action(AName,Params,Outcomes),Actions).
	
get_event_type(EventName, EType, TimeDependent) :-
	schedulable_event(EventName, TimeDependent), EType = schedulable, !
	;
	ad_hoc_event(EventName, TimeDependent), EType = ad_hoc, !
	;
	caused_event(EventName), EType = caused, !
	;
	is_choice_option(EventName, _), !, EType = choice_option, TimeDependent = false
	;
	throw(no_axiom_for_event(EventName)).
	
get_declared_exogenous_actions(EventDeclarations) :-
	findall(ea(Name,P1,P2),exogenous_action(Name,P1,P2),DeclaredEvents),
	(foreach(Event, DeclaredEvents), foreach(EDecl, EventDeclarations) do
		Event = ea(EventName, Params1, Params2),
		get_event_type(EventName, EType, TimeDependent),
		EDecl = ea(EventName, Params1, Params2, EType, TimeDependent)
	).
	
get_declared_exogenous_action_choices(ChoiceDeclarations) :-
	findall(eac(Name, Params, Options), 
		exogenous_action_choice(Name, Params, Options), 
		DeclaredChoices),
	(foreach(Choice, DeclaredChoices), foreach(CDecl, ChoiceDeclarations) do
		Choice = eac(ChoiceName, Params, Options),
		get_event_type(ChoiceName, EType, TimeDependent),
		CDecl = eac(ChoiceName, Params, Options, EType, TimeDependent)
	).
		
get_declared_immediate_actions(ImmediateActions) :-
	findall(ia(AName), immediate_action(AName), ImmediateActions).

	