:- dynamic time/2, xpos/3, ypos/3, carrying/3.

% domain description for worldtest.py

% no activities

% domain(robot,D) :- D = [rob1, rob2].
% domain(item,D) :- D = [coffee, chocolate].

% action declaration
primitive_action(tick,[]).
primitive_action(move_right,[robot]).
primitive_action(move_left,[robot]).
primitive_action(move_down,[robot]).
primitive_action(move_up,[robot]).

primitive_action(grab,[robot,item]).
primitive_action(drop,[robot,item]).

exogenous_action(accidental_drop, [robot, item]).

% land_on and crash are meant as outcome for stochastic action jump


primitive_action(land_on, [robot, integer, integer]).
primitive_action(crash, [robot]).





% fluent declaration: fluent_name, [arg_domains], value_domain
fluent(time,[], integer).
fluent(xpos, [robot], integer).
fluent(ypos, [robot], integer).
fluent(carrying, [robot, item], boolean).
fluent(active, [robot], boolean).

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
	
xpos(Rob, Pos, s0) :- get_current(xpos, [Rob], Pos).
xpos(Rob, Pos, slast) :- get_last(xpos, [Rob], Pos).

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
	
ypos(Rob, Pos, s0) :- get_current(ypos, [Rob], Pos).
ypos(Rob, Pos, slast) :- get_last(ypos, [Rob], Pos).

carrying(Rob, Item, do2(A,S)) :-
    A = grab(Rob, Item), ! 
	;
	A \= drop(Rob, Item),
    carrying(Rob, Item, S).
	
	
carrying(Rob, Item, s0) :- 
    get_current(carrying, [Rob, Item], X), X = true.
	
carrying(Rob, Item, slast) :- 
    get_last(carrying, [Rob, Item], X), X = true.

active(Rob, do2(A,S)) :-
	A \= crash(Rob),
	active(Rob, S).
	
active(Rob, s0) :-
	get_current(active, [Rob], X), X = true.

active(Rob, slast) :-
	get_last(active, [Rob], X), X = true.
	
time(T,do2(A,S)) :-
		time(TOld, S),
		(A = tick -> 
			T is TOld + 1
		;
			T  is TOld
		).
			
time(T, s0) :- get_current(time, [], T).		
time(T, slast) :- get_last(time, [], T).			

% transient fluent (i.e. function with situation argument as last position)
% this models a sensor that returns the item a robot is standing above or null
item_sensor(Robot, Item, Sit) :-
	domain(item, Items),
	xpos(Robot, X0, Sit),
	ypos(Robot, Y0, Sit),
	(fromto(Items, In1, Out1, []), fromto(null, In2, Out2, Item), param(Robot, Sit) do
		In1 = [I | Rest],
		( (xpos(I, X, Sit), X is X0, ypos(I, Y, Sit), Y is Y0) ->
			Out2 is I
			;
			Out2 is null
		)
	).


        
     
init_domaindesc :- true.
	
