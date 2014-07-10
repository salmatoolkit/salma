:- dynamic xpos/3, carrying/3.

sorts([robot, item]).
subsort(item, object).
subsort(robot, agent).

primitive_action(move_right,[r:robot]).
primitive_action(grab,[r:robot,i:item]).


exogenous_action(accidental_drop, [r:robot, i:item], []).

fluent(xpos, [o:object], integer).
fluent(carrying, [robot, item], boolean).

poss(move_right(_), _) :- true.
poss(grab(R,I), S) :- true.
poss(accidental_drop(R,I), S) :- carrying(R,I,S).

xpos(O, Pos, do2(A,S)) :- 
	xpos(O, OldPos, S),
	( A = move_right(O), !, Pos is OldPos + 1 ;
	A = move_right(R), carrying(R, O, S), !, Pos is OldPos + 1 ;
	Pos = OldPos ).
	
carrying(Rob, Item, do2(A,S)) :-
    A = grab(Rob, Item), ! 
	;
	A \= accidental_drop(Rob, Item), carrying(Rob, Item, S).

init_domaindesc :- true.
        
