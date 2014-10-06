:- ['../../ecl-src/agasmc'].
:- ['../../ecl-src/test_utils'].
:- [domaindesc_dependency_graph].

setPosition(Agent, X, Y) :-
	set_current(xpos, [Agent], X),
	set_current(ypos, [Agent], Y).

setVelocity(Agent, VX, VY) :-
	set_current(vx, [Agent], VX),
	set_current(vy, [Agent], VY).

	
init :- 
	init_agasmc,
	setDomain(robot, [rob1, rob2]),
	init_sort_hierarchy(_),
	setPosition(rob1, 0, 0),
	setVelocity(rob1, 1, 1),
	setPosition(rob2, 100, 0),
	setVelocity(rob2, -1, 1),
	set_current(time, [], 0).
	
analyze_term(Term, Prefix) :-
	(var(Term) ->
		Functor = Term, Params = []
		;
		Term =.. [Functor | Params]
	),
	printf("%s%w\n",[Prefix, Functor]),
	concat_strings(Prefix, "   ", Prefix2),
	(foreach(P, Params), param(Prefix2) do
		analyze_term(P, Prefix2)
	).
	
	
analyze_fluent(Fluent) :-
	fluent(Fluent, Params, Type),
	(Type = boolean ->
		Arity is length(Params)
		;
		Arity is length(Params) + 1
	),
	length(NewParams, Arity),
	append(NewParams, [do2(A,S)], NewParams2),
	Head =.. [Fluent | NewParams2],
	clause(Head, Body),
	printf("Head: %w\nBody: %w", [Head, Body]),
	nl,
	analyze_term(Body, "").
	

	
	
	