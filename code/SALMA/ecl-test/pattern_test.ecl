:- ['../ecl-src/agasmc'].
:- dynamic s_velocity/3.

sorts([agent, transferProcess]).

exogenous_action(teansferEnds, [t: transferProcess, a: agent, params: list], [error: float]).


% SrcFluent as last parameter
sense(Name, AgentSort, SenseAction, _) :-
	asserta(fluent(Name, [a:AgentSort], float)),
	asserta(primitive_action(SenseAction, [a:AgentSort])),
	
	var(Agent), var(Val), var(A), var(S),
	Head =.. [Name, Agent, Val, do2(A,S)],
	var(OldVal),
	OldCall =.. [Name, Agent, OldVal, S],
	
	
	Body =  (OldCall,
			(A = transferEnds(Name, Agent, Params, Error) ->
				transferInput(Name, Agent, Params, TempVal, S),
				Val is TempVal + Error
				;
				Val = OldVal)),
	asserta((Head :- Body)).

				