:- op(1010, xfy, ->).
:- op(1010, xfy, implies).
:- op(1005, xfy, \/).
:- op(1005, xfy, or).
:- op(1000, xfy, /\).
:- op(1000, xfy, and).
% =, \= at, etc. at 700
% not at 900
:- op(900, fy, not).
:- op(900, fy, !).


print_term(T, Level) :-
	T =.. [Functor | Args],
	printf("%d: %w ( %w )\n" , [Level, Functor, Args]),
	NewLevel is Level + 1,
	(foreach(A, Args), param(NewLevel) do
		print_term(A, NewLevel)
	).

test :-
	T = (x /\ y = 5 -> z),
	print_term(T, 0).
	