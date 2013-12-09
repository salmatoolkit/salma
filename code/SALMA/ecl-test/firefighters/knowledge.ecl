:- dynamic position/3.
	
sorts([agent]). 

fluent(position,[a:agent], integer).

primitive_action(move, [a:agent, delta:integer]).

poss(move(_,_), _) :- true.

position(Agent, Pos, do2(A,S)) :-
	position(Agent, OldPos, S),
	(
		A = move(Agent, Delta) ->
			Pos is OldPos + Delta
			;
			Pos is OldPos
	).
			