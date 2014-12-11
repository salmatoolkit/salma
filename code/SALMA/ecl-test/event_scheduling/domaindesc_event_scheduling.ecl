:- dynamic world_width/0, world_height/0, safety_distance/0, 
	xpos/3, ypos/3, vx/3, vy/3, active/2, wheels_wet/2.

sorts([robot]).
subsort(robot, agent).

constant(world_width, [], integer).
constant(world_height, [], integer).
constant(safety_distance, [], integer).

fluent(xpos, [r:robot], integer).
fluent(ypos, [r:robot], integer).
fluent(vx, [r:robot], integer).
fluent(vy, [r:robot], integer).
fluent(active, [r:robot], boolean).
fluent(wheels_wet, [r:robot], boolean).


derived_fluent(dist, [r1:robot, r2:robot], float).
derived_fluent(speed, [r:robot], float).


primitive_action(set_velocity,[r:robot, vx:integer, vy:integer]).
primitive_action(jump, [r:robot, nx:integer, ny:integer]).
primitive_action(activate, [r:robot]).
primitive_action(deactivate, [r:robot]).

exogenous_action(collide, [r1:robot, r2:robot], [severity:integer]).
exogenous_action(breakdown, [r1:robot], []).
exogenous_action(slip, [r:robot], []).

exogenous_action(lightning_strike, [r:robot], []).
exogenous_action(asteroid_hit, [r:robot], [size:float]).

exogenous_action_choice(disaster, [r:robot], [lightning_strike, asteroid_hit]).

exogenous_action(welcome_message, [r:robot], [code:int]).


exogenous_action(wall_alert, [r:robot], []).
%exogenous_action(crash_into_wall, [r:robot], [severity:integer]).



poss(set_velocity(_, _, _), _) :- true.
poss(jump(_, _, _), _) :- true.
poss(activate(_), _) :- true.

poss(collide(R1, R2, _), S) :- R1 @< R2, dist(R1,R2, Dist, S), Dist =< 10.

poss(wall_alert(R), S) :- 
	xpos(R, X, S), ypos(R, Y, S),
	vx(R, Vx, S), vy(R, Vy, S),
	(
		% approaching left wall
		X < safety_distance, 
		Vx < 0, !
		;
		% approaching right wall
		X > world_width - safety_distance,
		Vx > 0, !
		;
		% approaching top wall
		Y < safety_distance,
		Vy < 0, !
		;
		% approaching bottom wall
		Y > world_height - safety_distance,
		Vy > 0, !
	).
	
caused(breakdown(Rob), collide(R1, R2, Severity), Sit) :-
	(Rob = R1, ! ; Rob = R2),
	Severity > 0.5,
	speed(Rob, Speed, Sit),
	Speed >= 10.0.

	
poss(slip(Rob), S) :- wheels_wet(Rob, S).

	


schedulable(disaster(Rob), S) :-
	ypos(Rob, Y, S),
	Y > 100.

schedulable(welcome_message(Rob, _), S) :-
	ypos(Rob, Y, S),
	Y > 50,
	xpos(Rob, X, S),
	X >= 1000.


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
	xpos(R2, X2, S), ypos(R2, Y2, S),
	Dist is sqrt( (X2 - X1)^2 + (Y2 - Y1)^2).

speed(Rob, Speed, S) :-
	vx(Rob, Vx, S),
	vy(Rob, Vy, S),
	Speed is sqrt(Vx^2 + Vy^2).