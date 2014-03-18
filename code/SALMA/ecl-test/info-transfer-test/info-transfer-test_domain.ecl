:- dynamic xpos/3, ypos/3, carrying/3, target_item/3, target_assignment.
:- ['macro_expander'].

% domain description for worldtest.py

% no activities

% domain(robot,D) :- D = [rob1, rob2].
% domain(item,D) :- D = [coffee, chocolate].
sorts([robot, controller, item, object]).
subsorts([robot, item], object).


primitive_action(move_right,[r:robot]).
primitive_action(move_left, [r:robot]).
primitive_action(move_down, [r:robot]).
primitive_action(move_up, [r:robot]).

primitive_action(grab, [r:robot, i:item]).
primitive_action(drop, [robot,item]).

% fluent declaration: fluent_name, [arg_domains], value_domain

fluent(xpos, [o:object], integer).
fluent(ypos, [o:object], integer).

fluent(target_item, [r:robot], item).

fluent(carrying, [r:robot, i:item], boolean).

bidi_channel(target_assignment, [r:robot, c: controller]).

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

% successor state axioms

affected_by_move(Object, MovedObject, S) :-
	MovedObject = Object, ! 
	; 
	Object \= MovedObject, 
	carrying(MovedObject, Object, S).

xpos(Object, Pos, do2(A,S)) :- 
   	xpos(Object, POld, S),
	(
		A = move_right(O2), 
		affected_by_move(Object, O2),
		Pos is POld + 1, !
		;
		A = move_left(O2), 
		affected_by_move(Object, O2),
		Pos is POld - 1, !
		;
		Pos is POld, !
	).
	
ypos(Object, Pos, do2(A,S)) :- 
   	ypos(Object, POld, S),
	(
		A = move_down(O2), 
		affected_by_move(Object, O2),
		Pos is POld + 1, !
		;
		A = move_up(O2), 
		affected_by_move(Object, O2),
		Pos is POld - 1, !
		;
		Pos is POld, !
	).
	
carrying(Rob, Item, do2(A,S)) :-
    A = grab(Rob, Item), ! 
	;
	A \= drop(Rob, Item), 
    carrying(Rob, Item, S).
	


init_domaindesc :- true.
        
