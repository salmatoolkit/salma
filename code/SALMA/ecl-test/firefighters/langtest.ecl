knows(A, one, Val) :-
	Val is A + 1, !.
	
knows(A, two, Val) :-
	Val is A + 2, !.
	
knows(A, B, Val) :-
	Val is A + B.

