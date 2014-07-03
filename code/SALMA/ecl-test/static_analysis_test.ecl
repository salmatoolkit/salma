:- ['../ecl-src/agasmc'].
:- ['domaindesc'].


test(Body) :-
	clause(xpos(Rob, Pos, S), Body),
	write(Rob),nl,
	write(Pos),nl,
	write(S),nl,
	write(Body),nl.