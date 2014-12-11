
select_action_instance(ActionName, PossibleInstanceArgs, ActionTerm) :-
	(
		exogenous_action(ActionName, EntityParams, StochasticParams), !
		;
		exogenous_action_choice(ActionName, EntityParams, _),
		StochasticParams = []
	),
	(foreach(Param, EntityParams), foreach(Arg, PossibleInstanceArgs) do
		Param = _ : DomainName,
		domain(DomainName, Domain),
		member(Arg, Domain)
	), 
	% generate variable stochastic params to check possibility
	(foreach(_, StochasticParams), fromto(PossibleInstanceArgs, In, Out, TestArgs) do
		append(In, [_], Out)
	),
	ActionTerm =.. [ActionName | TestArgs].

possible_action_instance(ActionName, Situation, Time, 
	HandledHash, PossibleInstanceArgs) :-
	select_action_instance(ActionName, PossibleInstanceArgs, ActionTerm),
	(
		hash_get(HandledHash, (ActionName, PossibleInstanceArgs), Times), 
		!,
		not member(Time, Times)
		;
		true
	),
	poss(ActionTerm, Situation).

alternative_in_hash(ChoiceName, PossibleInstanceArgs, ScheduleHash) :-
	exogenous_action_choice(ChoiceName, _, Options),
	hash_keys(ScheduleHash, Keys),
	member(Key, Keys),
	Key = (OtherActionName, OtherArgs),
	member(OtherActionName, Options),
	OtherArgs = PossibleInstanceArgs, !.
	
	
get_possible_exogenous_action_instances(ActionName, Situation, Time, HandledHash, Candidates) :-
	findall(InstanceArgs, possible_action_instance(ActionName, Situation, Time,
							HandledHash, InstanceArgs), Candidates).
		

get_all_ad_hoc_event_instances(Situation, HandledHash, Candidates) :-
	time(Time, Situation),
	findall(ActionName, ad_hoc_event(ActionName), ActionNames),
	% filter out 
	(foreach(A, ActionNames), fromto([], CIn, COut, Candidates), 
		param(Situation, Time, HandledHash) do
		get_possible_exogenous_action_instances(A, Situation, Time, HandledHash, 
			CandidatesForAction),
		(length(CandidatesForAction) > 0 ->
			append(CIn, [A : CandidatesForAction], COut)
			;
			COut = CIn
		)
	).

% pre: CurrentTime <= Start <= TimeLimit
get_next_possible_ad_hoc_events(Start, TimeLimit, HandledEvents, Time, Events) :-
	(TimeLimit < Start -> throw(time_limit_before_start) ; true),
	current_time(CurrentTime),
	StartDelta is Start - CurrentTime,
	StartDelta2 is StartDelta + 1,
	make_schedule_hash(HandledEvents, HandledEventsHash),
	get_all_ad_hoc_event_instances(do2(tick(StartDelta), s0), HandledEventsHash, StartEvents),
	(length(StartEvents) > 0 ->
		Time = Start,
		Events = StartEvents
		;
		Limit is TimeLimit - CurrentTime + 1,
		(fromto(StartDelta2, CurrentDelta, NextDelta, Limit), fromto(-1, _, TOut, Time),
			fromto([], _, EvOut, Events), param(Limit, HandledEventsHash) do 
			Sit = do2(tick(CurrentDelta), s0),
			get_all_ad_hoc_event_instances(Sit, HandledEventsHash, Candidates),
			(length(Candidates) > 0 ->
				EvOut = Candidates,
				NextDelta = Limit,
				time(TOut, Sit)
				;
				EvOut = [],
				NextDelta is CurrentDelta + 1,
				TOut = -1
			)			
				
		)
	).
	

caused_action_instance(PerformedAction, ActionName, Situation, PossibleInstanceArgs) :-
	select_action_instance(ActionName, PossibleInstanceArgs, ActionTerm),
	caused(ActionTerm, PerformedAction, Situation).
	
get_caused_exogenous_action_instances(PerformedAction, ActionName, Situation, Candidates) :-
	findall(InstanceArgs, caused_action_instance(PerformedAction, ActionName, 
		Situation, InstanceArgs), Candidates).
		

get_exogenous_action_instances_by_cause(PerformedAction, Situation, Candidates) :-
	findall(ActionName, exogenous_action(ActionName, _, _), ActionNames),
	(foreach(A, ActionNames), fromto([], CIn, COut, Candidates), param(PerformedAction, Situation) do
		get_caused_exogenous_action_instances(PerformedAction, A, Situation, CandidatesForAction),
		(length(CandidatesForAction) > 0 ->
			append(CIn, [A : CandidatesForAction], COut)
			;
			COut = CIn
		)			
	).

% Determines which events are caused by the given list of actions. 
% This evaluates all defined caused-clauses and returns a list of
% Action : Event tuples.
get_all_caused_exogenous_action_instances(Actions, CausedEvents) :-
	(foreach(Action, Actions), fromto([], EvIn, EvOut, CausedEvents) do
		get_exogenous_action_instances_by_cause(Action, s0, Candidates),
		(length(Candidates) > 0 ->
			append(EvIn, [c(Action, Candidates)], EvOut)
			;
			EvOut = EvIn
		)
	).
	
	
% -------------------
% SCHEDULABLE EVENTS
% -------------------


schedulable_action_instance(ActionName, Situation, Time, CurrentScheduleHash, HandledHash,
	PossibleInstanceArgs) :-
	select_action_instance(ActionName, PossibleInstanceArgs, ActionTerm),
	not hash_contains(CurrentScheduleHash, (ActionName, PossibleInstanceArgs)),
	% skip this instance if it has already been handled in this same step (situation)
	(
		hash_get(HandledHash, (ActionName, PossibleInstanceArgs), Times), 
		!,
		not member(Time, Times)
		;
		true
	),		
	not alternative_in_hash(ActionName, PossibleInstanceArgs, CurrentScheduleHash),
	schedulable(ActionTerm, Situation).
	
get_schedulable_exogenous_action_instances(ActionName, Situation, Time, CurrentScheduleHash, 
	HandledHash, Candidates) :-
	findall(InstanceArgs, 
		schedulable_action_instance(ActionName, Situation, Time, CurrentScheduleHash,	
			HandledHash, InstanceArgs), 
		Candidates).
		
get_all_schedulable_event_instances_internal(Situation, CurrentScheduleHash, 
	HandledHash, Candidates) :-
	time(Time, Situation),
	findall(ActionName, schedulable_event(ActionName), ActionNames),
	(foreach(A, ActionNames), fromto([], CIn, COut, Candidates), 
		param(Situation, Time, CurrentScheduleHash, HandledHash) do
		get_schedulable_exogenous_action_instances(A, Situation, Time, CurrentScheduleHash, 
			HandledHash, CandidatesForAction),
		(length(CandidatesForAction) > 0 ->
			append(CIn, [A : CandidatesForAction], COut)
			;
			COut = CIn
		)
	).
	
make_schedule_hash(Schedule, ScheduleHash) :-
	hash_create(ScheduleHash),
	(foreach(Entry, Schedule), param(ScheduleHash) do
		Entry = ev(Time, EventName, EventParams),
		Key = (EventName, EventParams),
		(
			hash_get(ScheduleHash, Key, Times),
			append(Times, [Time], Times2)
			;
			Times2 = [Time]
		),
		hash_set(ScheduleHash, Key, Times2)
	).

get_all_schedulable_event_instances(Situation, CurrentSchedule, HandledInStep, Candidates) :-
	make_schedule_hash(CurrentSchedule, CurrentScheduleHash),
	make_schedule_hash(HandledInStep, HandledHash),
	get_all_schedulable_event_instances_internal(Situation, CurrentScheduleHash, 
		HandledHash, Candidates).
	

% pre: TimeLimit > current time
get_next_schedulable_events(Start, TimeLimit, CurrentSchedule, HandledInStep, 
	Time, Events) :-
	(TimeLimit < Start -> throw(time_limit_before_start) ; true),
	current_time(CurrentTime),
	StartDelta is Start - CurrentTime,
	StartDelta2 is StartDelta + 1,
	make_schedule_hash(CurrentSchedule, CurrentScheduleHash),
	make_schedule_hash(HandledInStep, HandledHash),
	
	get_all_schedulable_event_instances_internal(do2(tick(StartDelta), s0), 
		CurrentScheduleHash, HandledHash, CurrentEvents),
	(length(CurrentEvents) > 0 ->
		Time = Start,
		Events = CurrentEvents
		;
		Limit is TimeLimit - CurrentTime + 1,
		(fromto(StartDelta2, CurrentDelta, NextDelta, Limit), fromto(-1, _, TOut, Time),
			fromto([], _, EvOut, Events), param(Limit, CurrentScheduleHash, HandledHash) do 
			Sit = do2(tick(CurrentDelta), s0),
			get_all_schedulable_event_instances_internal(Sit, CurrentScheduleHash, 
				HandledHash, Candidates),
			(length(Candidates) > 0 ->
				EvOut = Candidates,
				NextDelta = Limit,
				time(TOut, Sit)
				;
				EvOut = [],
				NextDelta is CurrentDelta + 1,
				TOut = -1
			)			
				
		)
	).
	


% -----------------
% GENERAL FUNCTIONS
% -----------------

make_variable_event_term(EventName, Term) :-
	(exogenous_action(EventName, EntityParams, StochasticParams), !,
	TotalParamLength is length(EntityParams) + length(StochasticParams)
	;
	exogenous_action_choice(EventName, EntityParams, _), !,
	TotalParamLength is length(EntityParams)
	),	
	length(NewArgs, TotalParamLength),
	Term =.. [EventName | NewArgs].
	
is_choice_option(ActionName, Choice) :-
	exogenous_action_choice(Choice, _, Options),
	member(ActionName, Options).

% Completes exogenous action information:
% - marks exogenous actions with only a poss axiom as ad hoc
% - For events that only have a schedulable axiom, a default poss axiom
%	is added that uses the same body as the schedulability axiom.
% - for events that only have ha caused clause, a default poss axiom
%	is added that is always true.
init_event_scheduling :-
	retractall(ad_hoc_event(_)),
	retractall(schedulable_event(_)),
	retractall(caused_event(_)),
	findall(ExoActionName, exogenous_action(ExoActionName, _, _), ExoActionNames),
	findall(EventChoiceName, exogenous_action_choice(EventChoiceName, _, _), EventChoiceNames),
	append(ExoActionNames, EventChoiceNames, EventNames),
	(foreach(EventName, EventNames) do
		make_variable_event_term(EventName, EventTerm),
		(
			is_choice_option(EventName, _), !,
			(
				clause(schedulable(EventTerm, _), _),
				throw(schedulability_axiom_for_choice_option(EventName))
				;
				clause(caused(EventTerm, _, _), _),
				throw(cause_axiom_for_choice_option(EventName))
				;
				true
			)
			;
			clause(poss(EventTerm, _), _), 
			not clause(schedulable(EventTerm, _), _),
			not clause(caused(EventTerm, _, _), _), !,
			assert(ad_hoc_event(EventName)) 
			;
			clause(schedulable(EventTerm, S), SchedBody),
			assert(schedulable_event(EventName)),
			(not clause(poss(EventTerm, _), _) ->
				assert((poss(EventTerm, S) :- SchedBody))
				;
				true
			), !			
			;
			clause(caused(EventTerm, _, _), _),
			assert(caused_event(EventName)), !					
			;
			true
		),
		(not clause(poss(EventTerm, _), _) ->
			assert(poss(EventTerm, _))
			;
			true
		)
	).
				
				
				
		
	

