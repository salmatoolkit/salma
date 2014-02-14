:- ['../ecl-src/agasmc'].
:- dynamic xpos/3.

domain(robot,D) :-
        D=[rob1,rob2,rob3].

xpos(R, X, do2(A,S)) :-
	xpos(R, OldX, S),
	Delta::[2..5],	
	(A = move_right(R), !,
		X $= OldX + Delta
		;
	A = move_left(R), !,
		X $= OldX - Delta
		;
	X $=OldX).

xpos(_, 0, s0).
	
	
		



