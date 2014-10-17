:- dynamic xpos/3, ypos/3, vx/3, vy/3, active/2, reporting/3.

sorts([robot, controller]).
subsorts([robot, controller], agent).


fluent(xpos, [r:robot], integer).
fluent(ypos, [r:robot], integer).
fluent(vx, [r:robot], integer).
fluent(vy, [r:robot], integer).
fluent(active, [r:robot], boolean).
fluent(reporting, [r:robot, c:controller], boolean).

primitive_action(set_velocity,[r:robot, vx:integer, vy:integer]).
primitive_action(activate, [c:controller, r:robot]).
primitive_action(deactivate, [c:controller, r:robot]).
primitive_action(requestReport, [c:controller, r:robot]).

primitive_action(startReporting, [r:robot, c:controller]).
exogenous_action(finishReporting, [r:robot, c:controller]).


poss(set_velocity(_, _, _), _) :- true.
poss(activate(C,R), S) :- not active(R, S).
poss(deactivate(C, R), S) :- active(R, S).
poss(requestReport(C, R), S) :- active(R, S).
poss(startReporting(R, C), S) :- active(R, S), not reporting(R, C, S).

poss(finishReporting(R, C), S) :- active(R, S), reporting(R, C, S).

effect(xpos(Robot), tick(Steps), OldX, X, S) :-
	vx(Robot, Vx, S),
	X is OldX + Steps*Vx.

	
effect(ypos(Robot), tick(Steps), OldY, Y, S) :-
	vy(Robot, Vy, S),
	Y is OldY + Steps*Vy.
	
effect(vx(Robot), set_velocity(Robot, Vx, _), _, Vx, _).
effect(vy(Robot), set_velocity(Robot, Vy, _), _, Vy, _). 


effect(active(Robot), activate(_, Robot), _, true, _).
effect(active(Robot), deactivate(_, Robot), _, false, _).

effect(reporting(R, C), startReporting(R, C), _, true, _).
effect(reporting(R, C), finishReporting(R, C), _, false, _).
