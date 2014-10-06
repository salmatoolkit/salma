:- dynamic xpos/3, ypos/3, vx/3, vy/3, effect/5.

sorts([robot]).
subsort(robot, agent).

fluent(xpos, [r:robot], integer).
fluent(ypos, [r:robot], integer).
fluent(vx, [r:robot], integer).
fluent(vy, [r:robot], integer).

primitive_action(set_velocity,[r:robot, vx:integer, vy:integer]).
primitive_action(jump, [r:robot, nx:integer]).

poss(set_velocity(_, _, _), _) :- true.


effect(xpos(Robot), tick(Steps), OldX, X, S) :-
	vx(Robot, Vx, S),
	X is OldX + Steps*Vx.

effect(xpos(Robot), jump(Robot, NewX), _, X, _) :-
	X = NewX.
	
construct_fluents :-
	findall(Fluent, clause(effect(Fluent, _, _, _, _), _), Fluents),
	
	print(Fluents).
	

% xpos(Robot, X, do2(A, S)) :-
	% xpos(Robot, OldX, S),
	% (A = tick(Steps) ->
		% vx(Robot, Vx, S),
		% X is OldX + Steps*Vx
		% ;
		% X is OldX
	% ).
		
ypos(Robot, Y, do2(A, S)) :-
	ypos(Robot, OldY, S),
	(A = tick(Steps) ->
		vy(Robot, Vy, S),
		Y is OldY + Steps*Vy
		;
		Y is OldY
	).

vx(Robot, Vx, do2(A, S)) :-
	A = set_velocity(Robot, Vx, _), !
	;
	vx(Robot, Vx, S).

vy(Robot, Vy, do2(A, S)) :-
	A = set_velocity(Robot, _, Vy), !
	;
	vy(Robot, Vy, S).

