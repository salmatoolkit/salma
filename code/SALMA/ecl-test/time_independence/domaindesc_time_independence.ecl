:- dynamic xpos/3, ypos/3, vx/3, vy/3, active/2.

sorts([robot]).
subsort(robot, agent).

fluent(xpos, [r:robot], integer).
fluent(ypos, [r:robot], integer).
fluent(vx, [r:robot], integer).
fluent(vy, [r:robot], integer).
fluent(active, [r:robot], boolean).

primitive_action(set_velocity,[r:robot, vx:integer, vy:integer]).
primitive_action(jump, [r:robot, nx:integer, ny:integer]).
primitive_action(activate, [r:robot]).
primitive_action(deactivate, [r:robot]).

poss(set_velocity(_, _, _), _) :- true.
poss(jump(_, _, _), _) :- true.
poss(activate(_), _) :- true.


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
effect(vy(Robot), set_velocity(Robot, Vy, _), _, Vy, _). 	

effect(active(Robot), activate(Robot), _, true, _).
effect(active(Robot), deactivate(Robot), _, false, _).
