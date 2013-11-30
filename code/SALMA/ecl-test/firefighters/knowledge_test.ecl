:- ['../../ecl-src/agasmc'].
:- ['knowledge'].

domain(agent, [rob1, rob2]).


knows(Agent1, position(Agent2), do2(A,S)) :-
	Agent1 = Agent2,
	A = sensePos(Agent1), ! 
	;
	Agent1 \= Agent2, 
	A = receivePos(Agent1, Agent2), !
	;
	knows(Agent1, position(Agent2), S), !.

knows(_, position(_), s0) :- false.

possible_instances(knows, 


init :-
	init_agasmc,
	init_sort_hierarchy(_),
	set_current(position, [rob1], 10),
	set_current(position, [rob2], 42),
	set_current(time, [], 0).
	