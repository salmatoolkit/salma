:- dynamic xpos/3, ypos/3, vx/3, vy/3, carrying/3, 
	robot_radius/2, moving/2, dist_from_station/4, next_task/3.


sorts([robot, item, station, movable_object]).
subsorts([robot, station], agent).
subsorts([robot, item], movable_object).


% FLUENT DEFINITIONS

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

% a stochastic action wit two outcomes
stochastic_action(jump, [r:robot, height:float], [land_on, crash]).
primitive_action(land_on, [r:robot, x:integer, y:integer]).
primitive_action(crash, [r:robot]).



exogenous_action(finish_step, [r:robot], []).

exogenous_action(accidental_drop, [r:robot, i:item], []).

exogenous_action(collision, [r1:robot, r2:robot], [severity:integer]).


% POSS AND SCHED
schedulable(finish_step(Rob), S) :-
	moving(Rob, S).
	
	
% EFFECT AXIOMS

effect(xpos(Robot), finish_step(Robot), OldX, X, S) :-
	vx(Robot, Vx, S),
	X is OldX + Vx.

effect(xpos(Robot), land_on(Robot, X, _), _, X, _).


effect(ypos(Robot), finish_step(Robot), OldY, Y, S) :-
	vy(Robot, Vy, S),
	Y is OldY + Vy.

effect(ypos(Robot), land_on(Robot, _, Y), _, Y, _).
	
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

poss(move_right(_), _) :- true.
poss(move_left(_), _) :-true.
poss(move_down(_), _) :-true.
poss(move_up(_), _):-true.









poss(grab(R,I), S) :- 
	domain(robot, Robots),
	domain(item, Items),
	xpos(R, Xr, S), xpos(I, Xi, S), Xr =:= Xi,
	ypos(R, Yr, S), ypos(I, Yi, S), Yr =:= Yi,	
	not (member(R2, Robots), carrying(R2, I)).

poss(drop(R,I), S) :- carrying(R,I,S).

schedulable(accidental_drop(R,I), S) :- 
	action_occurred(grab(R,I), S).
	
	
poss(accidental_drop(R,I), S) :- carrying(R,I,S).

poss(collision(R1, R2, _), S) :- 
	R1 \= R2, xpos(R1, X, S), xpos(R2, X, S), 
	ypos(R1, Y, S), ypos(R2, Y, S).


% land_on and crash are meant as outcome for stochastic action jump
poss(land_on(_,_,_), _):-true.
poss(crash(_), _) :- true.

poss(paint(_,_), _) :- true.

poss(mark(_,_,_), _) :- true.

% successor state axioms

effect(carrying(Rob, Item), grab(Rob, Item), _, true, _).
effect(carrying(Rob, Item), drop(Rob, Item), _, false, _).
effect(carrying(Rob, Item), accidental_drop(Rob, Item), _, false, _).

effect(active(Rob), crash(Rob), _, false, _).
effect(active(Rob), collision(R1, R2, I), _, false, _) :-
	member(Rob, [R1, R2]), I >= 50.

effect(painted(Item), paint(_, Item), _, true, _).

effect(marking(Item), mark(_, Item, Data), _, Data, _).	
	

%restoreSitArg(xpos(Rob, Pos), S, xpos(Rob, Pos, S)).

    


dist_from_station(Rob, Station, Dist, S) :-
	xpos(Rob, X, S),
	ypos(Rob, Y, S),
	stationX(Station, Sx), stationY(Station, Sy),
	Dist is sqrt((X - Sx)^2 + (Y - Y).
	

moving(Rob, S) :-
	vx(Rob, Vx, S), abs(Vx) > 0, !
	;
	vy(Rob, Vy, S), abs(Vy) > 0.
	

		
init_domaindesc :- true.
        
