:- ['../ecl-src/agasmc'].
:- dynamic xpos/3, xpos2/3.

sorts([robot]).

fluent(xpos, [r:robot], integer).
fluent(xpos2, [r:robot], integer).

primitive_action(move_right,[r:robot]).
primitive_action(move_left, [r:robot]).

poss(move_right(_), _) :- true.
poss(move_left(_), _) :-true.


domain(robot,D) :-
        D=[rob1,rob2].

xpos2(R, X, do2(A,S)) :-
	xpos(R, OldX, S),
	Delta::[1..5],	
	(A = move_right(R), !,
		X $= OldX + Delta
		;
	A = move_left(R), !,
		X $= OldX - Delta
		;
	X $=OldX).

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
	
proc(moveToX, [r:robot, targetX : integer],
	while(xpos(r) =\= targetX, 
		if(xpos(r) < targetX, 
			move_right(r),
			move_left(r)
		)
	) 
).
	
proc(moveToX2, [r:robot, targetX : integer],
	while(not(xpos2(r) $>= targetX), 
		move_right(r)
	) 
).

init :-
	init_agasmc,
	set_current(xpos, [rob1], 10), 
	set_current(xpos, [rob2], 20),
	set_current(xpos2, [rob1], 10), 
	set_current(xpos2, [rob2], 20),
	set_current(time, [], 0).

%test1(s) :- do2(if( xpos(rob1) < 24, move_right(rob1), move_left(rob1) ),s0,s).

%test2(S) :- do2(while(xpos(rob1) < 24, move_right(rob1),s0,S).
test3(S) :- do2(while(xpos(rob1) < 24, move_right(rob1)),s0,S).
	
	
		



