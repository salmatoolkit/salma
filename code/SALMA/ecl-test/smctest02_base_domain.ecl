:- dynamic xpos/3, ypos/3, vx/3, vy/3, carrying/3, active/2, painted/2, 
	robot_radius/2, marking/3,
	gravity/1, partner/3, moving/2, dist_from_origin/3, robotLeftFrom/3, canPaint/3, grip/3, destX/2, destY/2.

% domain description for worldtest.py

% no activities

% domain(robot,D) :- D = [rob1, rob2].
% domain(item,D) :- D = [coffee, chocolate].
sorts([robot, item]).
subsort(robot, agent).
subsort(item, object).


% MOTION

primitive_action(move_right,[r:robot]).
atomic_action(move_right).
primitive_action(move_left, [r:robot]).
atomic_action(move_left).
primitive_action(move_down, [r:robot]).
atomic_action(move_down).
primitive_action(move_up, [r:robot]).
atomic_action(move_up).

primitive_action(land_on, [r:robot, x:integer, y:integer]).
stochastic_action(jump, [r:robot, height:float], [land_on, crash]).

fluent(xpos, [o:object], integer).
fluent(ypos, [o:object], integer).

derived_fluent(dist_from_origin, [r:robot], float).
derived_fluent(moving, [r:robot], boolean).

fluent(vx, [r:robot], integer).
fluent(vy, [r:robot], integer).



exogenous_action(finish_step, [r:robot], []).

schedulable(finish_step(Rob), S) :-
	moving(Rob, S).
	

effect(xpos(Robot), finish_step(Robot), OldX, X, S) :-
	vx(Robot, Vx, S),
	X is OldX + Vx.

effect(xpos(Robot), land_on(Robot, X, _), _, X, _).

effect(xpos(Item), drop(Rob, Item), _, NewX, S) :-
	xpos(Rob, RobX, S),
	NewX is RobX.

effect(xpos(Item), drop(Rob, Item), _, NewX, S) :-
	xpos(Rob, RobX, S),
	NewX is RobX.

effect(ypos(Item), drop(Rob, Item), _, NewY, S) :-
	ypos(Rob, RobY, S),
	NewY is RobY.
	
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


% OTHER ACTIONS

stochastic_action(pickUp, [r:robot, i:item], [grab]).

primitive_action(grab, [r:robot, i:item, grip:integer]).



primitive_action(drop, [r:robot, i:item]).
% land_on and crash are meant as outcome for stochastic action jump



primitive_action(crash, [r:robot]).

primitive_action(paint, [r:robot, i:item]).

primitive_action(mark, [r:robot, i:item, data:term]).

primitive_action(join, [r1:robot, r2:robot]).

% distinguish between discriminating entity parameters and additional "augmenting" parameters
exogenous_action(accidental_drop, [r:robot, i:item], []).

exogenous_action(collision, [r1:robot, r2:robot], [severity:integer]).



% fluent declaration: fluent_name, [arg_domains], value_domain


fluent(carrying, [r:robot, i:item], boolean).
fluent(grip, [r:robot], integer).


fluent(active, [r:robot], boolean).
% for now we just tread paint as a boolean attribute
fluent(painted, [i:item], boolean).
fluent(marking, [i:item], term).

fluent(partner, [r:robot], robot).


constant(robot_radius, [r:robot], float).
constant(gravity, [], float).
% poss




poss(pickUp(R, I, _), S) :- test_ad_hoc(
		and(
			not(exists([i, item], carrying(R, i))),
			not(exists([r, robot], carrying(r, I)))
		)
	).
poss(grab(_,_,_), _) :- true.


%poss(drop(R,I), S) :- carrying(R,I,S).
poss(drop(R,I), S) :- true.

schedulable(accidental_drop(R,I), S) :- 
	action_occurred(grab(R,I,_), S).
	
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

effect(carrying(Rob, Item), grab(Rob, Item, _), _, true, _).
effect(carrying(Rob, Item), drop(Rob, Item), _, false, _).
effect(carrying(Rob, Item), accidental_drop(Rob, Item), _, false, _).

effect(grip(Rob), grab(Rob, _, NewGrip), _, NewGrip, _).

effect(active(Rob), crash(Rob), _, false, _).
effect(active(Rob), collision(R1, R2, I), _, false, _) :-
	member(Rob, [R1, R2]), I >= 50.

effect(painted(Item), paint(_, Item), _, true, _).

effect(marking(Item), mark(_, Item, Data), _, Data, _).	
	

%restoreSitArg(xpos(Rob, Pos), S, xpos(Rob, Pos, S)).

    
robotLeftFrom(Rob, Pos, S) :-
        xpos(Rob, P, S),
        P < Pos.
        
canPaint(_, Item, S) :-
		not painted(Item, S).


dist_from_origin(Rob, Dist, S) :-
	xpos(Rob, X, S),
	ypos(Rob, Y, S),
	Dist is sqrt(X*X + Y*Y).

moving(Rob, S) :-
	vx(Rob, Vx, S), abs(Vx) > 0, !
	;
	vy(Rob, Vy, S), abs(Vy) > 0.
		
effect(partner(Rob), join(R1, R2), _, Partner, _) :-
	Rob = R1, 
	Partner = R2, !	
	; 
	Rob = R2,
	Partner = R1.
	

	
constant(destX, [i:item], integer).
constant(destY, [i:item], integer).


	
% partner(rob, partner, do2(a, s)) :-
	% partner(rob, partner, s).
		
init_domaindesc :- true.
        
