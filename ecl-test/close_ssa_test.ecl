:- ['../ecl-src/agasmc'].
:- dynamic xpos/3, attached/3.

primitive_action(move_right,[robot]).
primitive_action(move_left,[robot]).
primitive_action(attach, [robot, robot]).

fluent(xpos, [robot], integer).
fluent(attached, [robot, robot], boolean).

poss(move_right(_), _) :- true.
poss(move_left(_), _) :- true.
poss(attach(R1,R2), S) :- not attached(R1,R2,S).

xpos(Rob, Pos, do2(A,S)) :- 
	xpos(Rob, POld, S),
	(
	 A = move_right(Rob), 
	 Pos is POld + 1, !
	;
	 A = move_left(Rob),
	 Pos is POld - 1, !
	;
	 Pos is POld, !
	).
	
attached(R1, R2, do2(A,S)) :-
	A = attach(R1, R2), !
	;
	A \= attach(R1, R2), 
	attached(R1,R2, S).
		
init_domaindesc :- true.

init_scenario :-
	init_agasmc,
	assert(domain(robot, D) :- D =[rob1,rob2]).
	set_current(time,[],0).