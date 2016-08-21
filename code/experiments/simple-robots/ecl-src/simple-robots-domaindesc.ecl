:- dynamic xpos/3, ypos/3, vx/3, vy/3, carrying/3, 
	robot_radius/2, moving/2, dist_from_station/4, next_task/3,
	broken/2, unassigned/2, ready/2, delivered_to/3, undelivered/2,
	task_item/3, task_workstation/3, request_queue/3,
	delivered_item_count/3.


sorts([robot, item, coordinator, workstation, movable_object]).
subsorts([robot, coordinator], agent).
subsorts([robot, item], movable_object).


% FLUENT DEFINITIONS

fluent(broken, [r:robot], boolean).

fluent(xpos, [o:movable_object], integer).
fluent(ypos, [o:movable_object], integer).

fluent(vx, [r:robot], integer).
fluent(vy, [r:robot], integer).

fluent(carrying, [r:robot, i:item], boolean).

fluent(next_task, [r:robot], term).

fluent(delivered_to, [i:item], workstation).

fluent(request_queue, [c:coordinator], list).

fluent(delivered_item_count, [ws:workstation], integer).

derived_fluent(dist_from_station, [r:robot, ws:workstation], float).
derived_fluent(moving, [r:robot], boolean).

derived_fluent(unassigned, [r:robot], boolean).
derived_fluent(ready, [r:robot], boolean).


derived_fluent(undelivered, [i:item], boolean).

derived_fluent(task_item, [r:robot], item).
derived_fluent(task_workstation, [r:robot], workstation).

constant(stationX, [ws:workstation], integer).
constant(stationY, [ws:workstation], integer).
constant(robot_radius, [r:robot], float).

% ACTIONS

primitive_action(move_right,[r:robot]).
%atomic_action(move_right).
primitive_action(move_left, [r:robot]).
%atomic_action(move_left).
primitive_action(move_down, [r:robot]).
%atomic_action(move_down).
primitive_action(move_up, [r:robot]).
%atomic_action(move_up).

stochastic_action(pickUp, [r:robot, i:item], [grab, drop]).
primitive_action(grab, [r:robot, i:item, grip:integer]).
primitive_action(drop, [r:robot, i:item]).
exogenous_action(accidental_drop, [r:robot, i:item], []).


primitive_action(deliver, [r:robot, i:item, ws:workstation]).

primitive_action(assign_task, [c:coordinator, r:robot, i:item, ws:workstation]).


exogenous_action(step_succeeded, [r:robot], []).
exogenous_action(step_failed, [r:robot], []).
exogenous_action_choice(step_finished, [r:robot], [step_succeeded, step_failed]).


exogenous_action(collision, [r1:robot, r2:robot], [severity:integer]).

exogenous_action(request, [ws:workstation, c:coordinator], []).


% POSS AXIOMS

	
poss(move_right(R), S) :- not broken(R,S).
poss(move_left(R), S) :- not broken(R,S).
poss(move_down(R), S) :- not broken(R,S).
poss(move_up(R), S):- not broken(R,S).


poss(pickUp(R, I), S) :- 
	not broken(R, S),	
	xpos(R, Xr, S), xpos(I, Xi, S), Xr =:= Xi,
	ypos(R, Yr, S), ypos(I, Yi, S), Yr =:= Yi,	
	domain(robot, Robots),
	not (member(R2, Robots), carrying(R2, I, S)).

poss(grab(_, _, _), _) :- true.
poss(drop(_, _), _) :- true.

poss(deliver(R, I, Station), S) :- 
	not broken(R, S), carrying(R, I, S),
	xpos(R, Xr, S), stationX(Station, Xs), Xr =:= Xs,
	ypos(R, Yr, S), stationY(Station, Ys), Yr =:= Ys.
	
poss(accidental_drop(R,I), S) :- carrying(R,I,S).

poss(collision(R1, R2, _), S) :- 
	R1 \= R2, xpos(R1, X, S), xpos(R2, X, S), 
	ypos(R1, Y, S), ypos(R2, Y, S),
	(moving(R2, S), ! ; moving(R2, S)).

poss(request(_, _), _) :- true.


poss(assign_task(_, _, _, _), _) :- true.
	
 
% SCHEDULABILITY AXIOMS
schedulable(step_finished(Rob), S) :-
	moving(Rob, S).
schedulable(accidental_drop(R,I), S) :- 
	action_occurred(grab(R,I), S).

schedulable(request(_, _), _) :- true.
	
% EFFECT AXIOMS

effect(xpos(O), step_succeeded(Robot), _, X, S) :-
	(O = Robot, ! ; carrying(Robot, O, S)),	
	vx(Robot, Vx, S),
	xpos(Robot, OldX, S),
	X is OldX + Vx.
	
effect(ypos(O), step_succeeded(Robot), _, Y, S) :-
	(O = Robot, ! ; carrying(Robot, O, S)),	
	vy(Robot, Vy, S),
	ypos(Robot, OldY, S),
	Y is OldY + Vy.
	
effect(vx(Robot), step_succeeded(Robot), _, 0, _).
effect(vy(Robot), step_succeeded(Robot), _, 0, _).

effect(vx(Robot), step_failed(Robot), _, 0, _).
effect(vy(Robot), step_failed(Robot), _, 0, _).

effect(vx(Robot), move_right(Robot), _, 1, _).
effect(vx(Robot), move_left(Robot), _, -1, _).
effect(vx(Robot), move_up(Robot), _, 0, _).
effect(vx(Robot), move_down(Robot), _, 0, _).

effect(vy(Robot), move_right(Robot), _, 0, _).
effect(vy(Robot), move_left(Robot), _, 0, _).
effect(vy(Robot), move_up(Robot), _, -1, _).
effect(vy(Robot), move_down(Robot), _, 1, _).


effect(carrying(Rob, Item), grab(Rob, Item, _), _, true, _).
effect(carrying(Rob, Item), drop(Rob, Item), _, false, _).
effect(carrying(Rob, Item), deliver(Rob, Item, _), _, false, _).
effect(carrying(Rob, Item), accidental_drop(Rob, Item), _, false, _).
	
effect(next_task(Rob), assign_task(_, Rob, Item, Workstation), _, d(Item, Workstation), _).
effect(next_task(Rob), deliver(Rob, _, _), _, none, _).
effect(next_task(Rob), drop(Rob, _), _, none, _).
effect(next_task(Rob), accidental_drop(Rob, _), _, none, _).

effect(delivered_to(Item), deliver(_, Item, Station), _, Station, _).
effect(delivered_item_count(Ws), deliver(_, _, Ws), OldCount, NewCount, _) :-
	NewCount is OldCount + 1.

effect(broken(Rob), collision(R1, R2, Severity), _, true, _) :-
	(R1 = Rob, ! ; R2 = Rob),
	Severity > 7.

effect(request_queue(C), request(Ws, C), OldQueue, NewQueue, _) :-
	append(OldQueue, [Ws], NewQueue).
effect(request_queue(C), assign_task(C, _, _, Ws), OldQueue, NewQueue, _) :-
	delete(Ws, OldQueue, NewQueue), !.
% DERIVED FLUENTS   


dist_from_station(Rob, Station, Dist, S) :-
	xpos(Rob, X, S),
	ypos(Rob, Y, S),
	stationX(Station, Sx), stationY(Station, Sy),
	Dist is abs(X - Sx) + abs(Y - Sy).
	

moving(Rob, S) :-
	vx(Rob, Vx, S), abs(Vx) > 0, !
	;
	vy(Rob, Vy, S), abs(Vy) > 0.
	
unassigned(Rob, S) :-
	next_task(Rob, none, S).
	
ready(Rob, S) :-
	not moving(Rob, S), not broken(Rob, S).
	
undelivered(Item, S) :-
	delivered_to(Item, none, S),
	domain(robot, Robots, S),
	not (member(R, Robots), next_task(R, d(Item, _), S)).
	
task_item(Rob, Item, S) :-
	next_task(Rob, Task, S),
	Task = d(Item, _), !
	;
	Item = none.

task_workstation(Rob, Ws, S) :-
	next_task(Rob, Task, S),
	Task = d(_, Ws), !
	;
	Ws = none.	
	
init_domaindesc :- true.
        
