possible_action_instance(ActionName, Situation, PossibleInstanceArgs) :-
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
	ActionTerm =.. [ActionName | TestArgs],
	poss(ActionTerm, Situation).
	
get_exogenous_action_instances(ActionName, Situation, Candidates) :-
	findall(InstanceArgs, possible_action_instance(ActionName, InstanceArgs), Candidates).
		

get_all_possible_exogenous_action_instances(Situation, Candidates) :-
	findall(ActionName, exogenous_action(ActionName, _, _), ActionNames),
	(foreach(A, ActionNames), foreach(C, Candidates) do
		get_exogenous_action_instances(A, CandidatesForAction),
		C = A : CandidatesForAction
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
			fromto(_, _, EvOut, Events) do 
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
	
	
get_schedulable_events(Events) :-
	true.

