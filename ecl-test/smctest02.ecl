call_twice(T1, T2, Var1, Var2):-
	Var1 = test1,
	call(T1),
	Var2 = test2,
	call(T2).

test(T1, T2) :-
	var(X1),
	T1 =.. [write,X1],
	var(X2),
	T2 =.. [write,X2],
	call_twice(T1,T2,X1,X2).
	
	
itertest(L, L2) :-
	(fromto(L, In, Out, []),
	foreach(E, L2) do
	In = [A | Out],
	(A > 3 -> E = A ; E = [])).
	
itertest2(L,L2, Result) :-
	(fromto(L, In, Out, done(Result)), foreach(X2, L3) do
		(In = [], Out = done(true), X2 = [], !
		;
		In = [A | B], 
		(A > 5 -> X2 = A, Out = B ; X2 = [], Out = done(false)), !
		)
	), flatten(L3, L2).
	
itertest3(L,L2) :-
	(foreach(X, L), foreach(X2, L2) do
		X > 5 -> X2 = X ; X2 = []
	).