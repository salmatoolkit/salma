:- dynamic xpos/3, holding/3.

% domain description for worldtest.py

% no activities

% domain(robot,D) :- D = [rob1, rob2].
% domain(item,D) :- D = [coffee, chocolate].
sorts([robot, item, object]).
subsorts([robot, item], object).


primitive_action(move_right,[r:robot]).
primitive_action(move_left, [r:robot]).

primitive_action(grab, [r:robot, i:item]).
primitive_action(drop, [robot,item]).


% distinguish between discriminating entity parameters and additional "augmenting" parameters
exogenous_action(accidental_drop, [r:robot, i:item], []).
exogenous_action(recharged, [r:robot], []).

% fluent declaration: fluent_name, [arg_domains], value_domain

fluent(xpos, [r:object], integer).
fluent(holding, [r:robot, i:item], boolean).

derived_fluent(reachable, [r:robot, i:item], boolean).
% poss

poss(move_right(_), _) :- true.
poss(move_left(_), _) :-true.

poss(grab(R,I), S) :- test_ad_hoc(
						and(
							not(exists([i, item], carrying(R, i))),
							not(exists([r, robot], carrying(r, I)))
						)
					).

poss(drop(R,I), S) :- carrying(R,I,S).
poss(accidental_drop(R,I), S) :- carrying(R,I,S).

poss(recharged(R), S) :- xpos(R,S) $>= 100. 


% successor state axioms


xpos(Rob, Pos, do2(A,S)) :-  
	xpos(Rob, POld, S),
	(
	 A = move_right(Rob), 
	 Pos is POld + 1, !
	;
	 A = move_left(Rob),
	 Pos is POld - 1, !
	;
	 Pos is POld, !
	).
	
	
carrying(Rob, Item, do2(A,S)) :-
    A = grab(Rob, Item), ! 
	;
	A \= drop(Rob, Item), A \= accidental_drop(Rob, Item), 
    carrying(Rob, Item, S).

reachable(Rob, Item, S) :-
	xpos(Rob, X1, S),
	xpos(Item, X2, S),
	abs(X1 - X2) =< 5.

init_domaindesc :- true.
        
