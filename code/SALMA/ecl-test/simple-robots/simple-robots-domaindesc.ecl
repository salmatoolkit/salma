:- dynamic xpos/3, ypos/3, holding/3.

% domain description for worldtest.py

% no activities

% domain(robot,D) :- D = [rob1, rob2].
% domain(item,D) :- D = [coffee, chocolate].
sorts([robot, item, object]).
subsorts([robot, item], object).


primitive_action(move_right,[r:robot]).
primitive_action(move_left, [r:robot]).
primitive_action(move_down, [r:robot]).
primitive_action(move_up, [r:robot]).

primitive_action(grab, [r:robot, i:item]).
primitive_action(drop, [robot,item]).
% land_on and crash are meant as outcome for stochastic action jump
primitive_action(land_on, [r:robot, x:integer, y:integer]).


primitive_action(crash, [r:robot]).

primitive_action(paint, [r:robot, i:item]).


% distinguish between discriminating entity parameters and additional "augmenting" parameters
exogenous_action(accidental_drop, [r:robot, i:item], []).

exogenous_action(collision, [r1:robot, r2:robot], [severity:integer]).

stochastic_action(jump, [r:robot, height:float], [land_on, crash]).


% fluent declaration: fluent_name, [arg_domains], value_domain

fluent(xpos, [r:robot], integer).
fluent(ypos, [r:robot], integer).
fluent(carrying, [r:robot, i:item], boolean).
fluent(active, [r:robot], boolean).
% for now we just tread paint as a boolean attribute
fluent(painted, [i:item], boolean).

constant(robot_radius, [r:robot], float).
constant(gravity, [], float).
% poss


poss(move_right(_), _) :- true.
poss(move_left(_), _) :-true.
poss(move_down(_), _) :-true.
poss(move_up(_), _):-true.

poss(grab(R,I), S) :- test_ad_hoc(
						and(
							not(exists([i, item], carrying(R, i))),
							not(exists([r, robot], carrying(r, I)))
						)
					).

poss(drop(R,I), S) :- carrying(R,I,S).
poss(accidental_drop(R,I), S) :- carrying(R,I,S).
poss(collision(R1, R2, _), S) :- 
	R1 \= R2, xpos(R1, X, S), xpos(R2, X, S), 
	ypos(R1, Y, S), ypos(R2, Y, S).


% land_on and crash are meant as outcome for stochastic action jump
poss(land_on(_,_,_), _):-true.
poss(crash(_), _) :- true.

poss(paint(_,_), _) :- true.


% successor state axioms


xpos(Rob, Pos, do2(A,S)) :- 
    (A = land_on(Rob, X, _) ->
		Pos is X
		;			
		xpos(Rob, POld, S),
		(
		 A = move_right(Rob), 
		 Pos is POld + 1, !
		;
		 A = move_left(Rob),
		 Pos is POld - 1, !
		;
		 Pos is POld, !
		)
	).
	
ypos(Rob, Pos, do2(A,S)) :- 
	(A = land_on(Rob, _, Y) ->
		Pos is Y
		;
		ypos(Rob, POld, S),
		(
		 A = move_down(Rob), 
		 Pos is POld + 1, !
		;
		 A = move_up(Rob),
		 Pos is POld - 1, !
		;
		 Pos is POld, !
		)
	).
	
carrying(Rob, Item, do2(A,S)) :-
    A = grab(Rob, Item), ! 
	;
	A \= drop(Rob, Item), A \= accidental_drop(Rob, Item), 
    carrying(Rob, Item, S).
	
	

active(Rob, do2(A,S)) :-
	A \= crash(Rob),
	% assume that collision iwth intensity of higher than 50 leads
	% to destruction
	not (A = collision(R1, R2, I), member(Rob, [R1, R2]), I >= 50),
	active(Rob, S).
	
	
painted(Item, do2(A,S)) :-
	A = paint(_, Item), !
	;
	painted(Item, S).
	
		

%restoreSitArg(xpos(Rob, Pos), S, xpos(Rob, Pos, S)).

    
robotLeftFrom(Rob, Pos, S) :-
        xpos(Rob, P, S),
        P < Pos.
        
canPaint(_, Item, S) :-
		not painted(Item, S).

init_domaindesc :- true.
        
