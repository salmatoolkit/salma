:- lib(ic).
:- lib(branch_and_bound).

calc_cost(VarList, VarCosts, Cost) :-
	(foreach(V, VarList), foreach(VC, VarCosts), fromto(0, C1, C2, Cost) do
		V = 0 -> C2 is C1 + VC ; C2 is C1
	).
	
calc_solution(X,Y,Z,V,W) :- (X or Y or Z) and ((neg V) or (neg W)),
	labeling([X,Y,Z,V,W]).

solution_with_cost(X,Y,Z,V,W,VarCosts,Cost) :-
	calc_solution(X,Y,Z,V,W),
	calc_cost([X,Y,Z,V,W], VarCosts, Cost).

best_solution(X,Y,Z,V,W,VarCosts,Cost) :-
	minimize(solution_with_cost(X,Y,Z,V,W,VarCosts,Cost), Cost).
	