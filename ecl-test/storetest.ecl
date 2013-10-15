:- local store(mystore).

test(Id, Path) :-
	T =.. [Id | Path],
	write(T),nl.
	
test2(T, CList) :-
	
	(count(I,0,3), fromto([], In, Out, Vars), fromto([], In2, Out2, CList) do
		var(X),
		append(In, [X], Out),
		append(In2, [(X is I)], Out2)
	),
	T =.. [sum, Vars, R].
	
		