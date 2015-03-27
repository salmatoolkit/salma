:- lib(document).
:- lib(ic).

:- comment(test/1, [
		summary : "Test",
		index: ["Test index"],
		args: ["R": "The result"],

    amode: test(-),
    

    fail_if: "A solution cannot be found.",

    desc: html("Tests checkTimeSequence")
   ]). 
   
checkTimeSequence(ListIn, Result) :-
		flatten(ListIn, List2),
		sort(List2, List3),
		shelf_create(t(-1), Shelf),
		(fromto(List3, In, Out, []), param(Shelf) do
			In = [Head | Tail],
			Head = T : Goal,
			(Goal = ok ->
				shelf_set(Shelf, 1, T),
				Out = Tail
				;
				Out = []
			)
		),
		shelf_get(Shelf,1,Result).
		


   
test(R) :-
	L = [5:not_ok, 3:ok, 2:ok, 1:ok, 6:ok],
	checkTimeSequence(L, R).

test2(L):-
	(fromto(L, In, Out, []), fromto(-1, _, Out2, Result) do
		In = [H|T],
		(H > 5 -> Out = [], Out2 is H ; Out = T, Out2 is -1)
	),
	write(Result),nl.

test3(L,L2) :-
	(foreach(X,L), foreach(Y, L2) do
		append([X],[0], Y)
		).
	

test4(X, Y, TargetX, TargetY) :-
	(foreach(DX, [-1,0,1]), param(X, Y, TargetX, TargetY) do
		(foreach(DY, [-1,0,1]), param(DX, X, Y, TargetX, TargetY) do
			X + DX $\= TargetX,
			Y + DY $\= TargetY
		)
	).
					
				
combinations(Domains, Combinations) :-
	findall(R, create_combination(Domains, R), Combinations).
		
create_combination(Domains, Instance) :-
	(foreach(D, Domains), foreach(Arg, Instance) do
		member(Arg, D)
	).
	

ortest(X,Y) :-
	(X = a ; X = b), Y = 1 ; (X = c; X = d), Y = b1.

fromtotest(End, Start, Stop, Result) :-
	(fromto(Start, I, O, End), fromto(0, I2, O2, Result), param(End, Stop) do
		print(I), nl,
		O2 is I2 + 1,
		(I = Stop -> O = End ; O is I +1)
	).
fromtotest2 :-
	OkIntervals = [s(5,10), s(15,20), s(25,30)],
	StartTimes = [s(1,10)],
	MaxTime = 10,
	(foreach(Int, OkIntervals), 
			fromto(StartTimes, STIn, STOut, Unhandled1),
			fromto([], Res1In, Res1Out, Res1),
			fromto([], OkDecIn, OkDecOut, OkDecisionPoints),
			param(MaxTime) do
			print(Int), nl, print(MaxTime), nl,
			STOut = STIn,
			append(Res1In, [Int : ok], Res1Out),
			append(OkDecIn, [Int : ok], OkDecOut)
	),
	printf("%w - %w - %w\n", [Unhandled1, Res1, OkDecisionPoints]).
			
