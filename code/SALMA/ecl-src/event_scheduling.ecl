
select_action_instance(ActionName, PossibleInstanceArgs, ActionTerm) :-
	exogenous_action(ActionName, EntityParams, StochasticParams),
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

possible_action_instance(ActionName, Situation, PossibleInstanceArgs) :-
	select_action_instance(ActionName, PossibleInstanceArgs, ActionTerm),
	poss(ActionTerm, Situation).
	
get_possible_exogenous_action_instances(ActionName, Situation, Candidates) :-
	findall(InstanceArgs, possible_action_instance(ActionName, Situation, InstanceArgs), Candidates).
		

get_all_possible_exogenous_action_instances(Situation, Candidates) :-
	findall(ActionName, exogenous_action(ActionName, _, _), ActionNames),
	(foreach(A, ActionNames), fromto([], CIn, COut, Candidates), param(Situation) do
		get_possible_exogenous_action_instances(A, Situation, CandidatesForAction),
		(length(CandidatesForAction) > 0 ->
			append(CIn, [A : CandidatesForAction], COut)
			;
			COut = CIn
		)
	).

% pre: TimeLimit > current time
get_next_possible_events(TimeLimit, Time, Events) :-
	current_time(CurrentTime),
	get_all_possible_exogenous_action_instances(s0, CurrentEvents),
	(length(CurrentEvents) > 0 ->
		Time = CurrentTime,
		Events = CurrentEvents
		;
		Limit is TimeLimit - CurrentTime + 1,
		(fromto(1, CurrentDelta, NextDelta, Limit), fromto(-1, _, TOut, Time),
			fromto(_, _, EvOut, Events), param(Limit) do 
			Sit = do2(tick(CurrentDelta), s0),
			get_all_possible_exogenous_action_instances(Sit, Candidates),
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
	findall(InstanceArgs, possible_action_instance(PerformedAction, ActionName, 
		Situation, InstanceArgs), Candidates).
		

get_exogenous_action_instances_by_cause(PerformedAction, Situation, Candidates) :-
	findall(ActionName, exogenous_action(ActionName, _, _), ActionNames),
	(foreach(A, ActionNames), foreach(C, Candidates), param(PerformedAction, Situation) do
		get_caused_exogenous_action_instances(PerformedAction, A, Situation, CandidatesForAction),
		C = A : CandidatesForAction
	).

% Determines which events are caused by the given list of actions. 
% This evaluates all defined caused-clauses and returns a list of
% Action : Event tuples.
get_all_caused_exogenous_action_instances(Actions, CausedEvents) :-
	(foreach(Action, Actions), foreach(EventsByCause, CausedEvents) do
		get_exogenous_action_instances_by_cause(Action, s0, Candidates),
		EventsByCause = c(Action, Candidates)
	).
	
	


