:- dynamic xpos/3, ypos/3, vx/3, vy/3, active/2.

sorts([robot]).
subsort(robot, agent).

fluent(xpos, [r:robot], integer).
fluent(ypos, [r:robot], integer).
fluent(vx, [r:robot], integer).
fluent(vy, [r:robot], integer).
fluent(active, [r:robot], boolean).

derived_fluent(dist, [r1:robot, r2:robot], float).



primitive_action(set_velocity,[r:robot, vx:integer, vy:integer]).
primitive_action(jump, [r:robot, nx:integer, ny:integer]).
primitive_action(activate, [r:robot]).
primitive_action(deactivate, [r:robot]).

exogenous_action(collide, [r1:robot, r2:robot], [severity:integer]).
exogenous_action(breakdown, [r1:robot], []).

poss(set_velocity(_, _, _), _) :- true.
poss(jump(_, _, _), _) :- true.
poss(activate(_), _) :- true.

poss(collide(R1, R2, _), S) :- dist(R1,R2, Dist, S), Dist =< 10.

caused(breakdown(Rob), collide(R1, R2, Severity), Sit) :-
	

effect(xpos(Robot), tick(Steps), OldX, X, S) :-
	vx(Robot, Vx, S),
	X is OldX + Steps*Vx.
	
poss(deactivate(_), _) :- true.
	
effect(xpos(Robot), jump(Robot, NewX, _), _, X, _) :-
	X = NewX.
	
effect(ypos(Robot), tick(Steps), OldY, Y, S) :-
	vy(Robot, Vy, S),
	Y is OldY + Steps*Vy.
	
effect(ypos(Robot), jump(Robot, _, NewY), _, Y, _) :-
	Y = NewY.
	
effect(vx(Robot), set_velocity(Robot, Vx, _), _, Vx, _).
effect(vy(Robot), set_velocity(Robot, _, Vy), _, Vy, _). 	

effect(active(Robot), activate(Robot), _, true, _).
effect(active(Robot), deactivate(Robot), _, false, _).


dist(R1, R2, Dist, S) :-
	xpos(R1, X1, S), ypos(R1, Y1, S),
	xpos(R2, X2, S), ypos(R1, Y2, S),
	Dist is sqrt( (X2 - X1)^2 + (Y2 - Y1)^2).

