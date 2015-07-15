:- dynamic xpos/3, ypos/3, vx/3, vy/3, carrying/3, 
	robot_radius/2, moving/2, dist_from_station/4, next_task/3,
	broken/2.


sorts([robot, item, station, movable_object]).
subsorts([robot, station], agent).
subsorts([robot, item], movable_object).


% FLUENT DEFINITIONS

fluent(broken, [r:robot], boolean).

fluent(xpos, [o:movable_object], integer).
fluent(ypos, [o:movable_object], integer).

fluent(vx, [r:robot], integer).
fluent(vy, [r:robot], integer).

fluent(carrying, [r:robot, i:item], boolean).

fluent(next_task, [r:robot], item).

derived_fluent(dist_from_station, [r:robot, s:station], float).
derived_fluent(moving, [r:robot], boolean).



constant(stationX, [s:station], integer).
constant(stationY, [s:station], integer).
constant(robot_radius, [r:robot], float).

% ACTIONS

primitive_action(move_right,[r:robot]).
atomic_action(move_right).
primitive_action(move_left, [r:robot]).
atomic_action(move_left).
primitive_action(move_down, [r:robot]).
atomic_action(move_down).
primitive_action(move_up, [r:robot]).
atomic_action(move_up).

primitive_action(grab, [r:robot, i:item]).
primitive_action(drop, [robot,item]).

primitive_action(assign_task, [s:station, r:robot, i:item]).

% a stochastic action wit two outcomes
stochastic_action(jump, [r:robot, height:float], [land_on, crash]).
primitive_action(land_on, [r:robot, x:integer, y:integer]).
primitive_action(crash, [r:robot]).

exogenous_action(finish_step, [r:robot], []).

exogenous_action(accidental_drop, [r:robot, i:item], []).

exogenous_action(collision, [r1:robot, r2:robot], [severity:integer]).


% POSS AXIOMS

	
poss(move_right(R), S) :- not broken(R,S).
poss(move_left(R), S) :- not broken(R,S).
poss(move_down(R), S) :- not broken(R,S).
poss(move_up(R), S):- not broken(R,S).

poss(grab(R,I), S) :- 
	not broken(R, S),
	domain(robot, Robots),
	domain(item, Items),
	xpos(R, Xr, S), xpos(I, Xi, S), Xr =:= Xi,
	ypos(R, Yr, S), ypos(I, Yi, S), Yr =:= Yi,	
	not (member(R2, Robots), carrying(R2, I)).

poss(drop(R,I), S) :- not broken(R,S), carrying(R,I,S).
	
poss(accidental_drop(R,I), S) :- carrying(R,I,S).

poss(collision(R1, R2, _), S) :- 
	R1 \= R2, xpos(R1, X, S), xpos(R2, X, S), 
	ypos(R1, Y, S), ypos(R2, Y, S),
	(moving(R2, S), ! ; moving(R2, S)).


% land_on and crash are meant as outcome for stochastic action jump
poss(land_on(_,_,_), _):-true.
poss(crash(_), _) :- true.

	

% SCHEDULABILITY AXIOMS
schedulable(finish_step(Rob), S) :-
	moving(Rob, S).
schedulable(accidental_drop(R,I), S) :- 
	action_occurred(grab(R,I), S).

	
% EFFECT AXIOMS

effect(xpos(O), finish_step(Robot), _, X, S) :-
	(O = Robot, ! ; carrying(Robot, O, S)),	
	vx(Robot, Vx, S),
	xpos(Robot, OldX, S),
	X is OldX + Vx.

effect(xpos(O), land_on(Robot, X, _), _, X, _) :-
	(O = Robot, ! ; carrying(Robot, O, S)).
	
effect(ypos(O), finish_step(Robot), _, Y, S) :-
	(O = Robot, ! ; carrying(Robot, O, S)),	
	vy(Robot, Vy, S),
	ypos(Robot, OldY, S),
	Y is OldY + Vy.

effect(ypos(O), land_on(Robot, _, Y), _, Y, _) :-
	(O = Robot, ! ; carrying(Robot, O, S)).	
	
effect(vx(Robot), finish_step(Robot), _, 0, _).
effect(vy(Robot), finish_step(Robot), _, 0, _).

effect(vx(Robot), move_right(Robot), _, 1, _).
effect(vx(Robot), move_left(Robot), _, -1, _).
effect(vx(Robot), move_up(Robot), _, 0, _).
effect(vx(Robot), move_down(Robot), _, 0, _).

effect(vy(Robot), move_right(Robot), _, 0, _).
effect(vy(Robot), move_left(Robot), _, 0, _).
effect(vy(Robot), move_up(Robot), _, -1, _).
effect(vy(Robot), move_down(Robot), _, 1, _).



effect(carrying(Rob, Item), grab(Rob, Item), _, true, _).
effect(carrying(Rob, Item), drop(Rob, Item), _, false, _).
effect(carrying(Rob, Item), accidental_drop(Rob, Item), _, false, _).
	
effect(next_task(Rob), assign_task(_, Rob, Item), _, Item, _).

effect(broken(Rob), crash(Rob), _, true, _).
effect(broken(Rob), collision(R1, R2, Severity), _, B, _) :-
	(R1 = Rob, ! ; R2 = Rob),
	Severity > 7.


% DERIVED FLUENTS   


dist_from_station(Rob, Station, Dist, S) :-
	xpos(Rob, X, S),
	ypos(Rob, Y, S),
	stationX(Station, Sx), stationY(Station, Sy),
	Dist is sqrt((X - Sx)^2 + (Y - Y)).
	

moving(Rob, S) :-
	vx(Rob, Vx, S), abs(Vx) > 0, !
	;
	vy(Rob, Vy, S), abs(Vy) > 0.
	

		
init_domaindesc :- true.
        
