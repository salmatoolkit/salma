

subst_in_term(Var, Rep, T, T2) :-
	subst_in_term(Var, Rep, T, T2, []).
	
subst_in_term(Var, Rep, T, T2, BlockMargins):-
		var(T), 
		T2 = T, !
		;		
        T = Var, 
        T2 = Rep, !
		;
		functor(T, Functor, _),
		member(Functor, BlockMargins),
		T2 = T, !
        ;
		arity(T,N),
		(N is 0 ->
			T2 = T
		;
			T =.. Tl,
			subst_in_list(Var, Rep, Tl, Tl2, BlockMargins),
			T2 =.. Tl2
		).
		
		
subst_in_list(Var, Rep, [H], Tl2, BlockMargins) :-
        subst_in_term(Var, Rep, H, T2, BlockMargins),
        Tl2 = [T2], !.

subst_in_list(Var,Rep,[H | Tl], Tl2, BlockMargins) :-
		subst_in_term(Var, Rep, H, T2, BlockMargins),
        subst_in_list(Var,Rep,Tl,Tl3, BlockMargins),
        Tl2 = [T2 | Tl3].
		
get_subterm(TermIn, [Pos], TermOut) :-
	
	(Pos is 0 -> 
		TermOut = TermIn 
	; 
		(is_list(TermIn) -> array_list(T, TermIn) ; T = TermIn),
		arg(Pos, T, TermOut)
	), !.
	
get_subterm(TermIn, [Pos | PTail], TermOut) :-
	get_subterm(TermIn, [Pos], T),
	get_subterm(T, PTail, TermOut), !.
	
sublist([Head | Tail], Start, End, Sublist) :-
	(
		Start < 0, throw(start_out_of_bounds)
		;
		End < Start, throw(end_before_start)
		;
		Start = 0,
		
		(End = 0 ->
			Sublist = [Head]
			;
			E2 is End - 1,
			sublist(Tail, 0, E2, Sub2),
			append([Head], Sub2, Sublist)
		), !
		; % Start > 0
		S2 is Start -1,
		E2 is End -1,
		sublist(Tail, S2, E2, Sublist)
	).
	%,printf("H: %w, T: %w, S: %d, E: %d -> %w\n",[Head, Tail, Start, End, Sublist])	.
	
sublist([], _, _, Sublist) :-
	Sublist = [].

last_element(List, Last) :-
	length(List, 0), Last = none, !
	;
	I is length(List) - 1,
	sublist(List, I, I, L2),
	L2 = [Last].
	
% Calculates the minimum among V1 and V2. Handles nondet.
getMin(V1, V2, Result) :-
	V1 = nondet, !,
		(V2 = nondet -> Result = nondet ; Result is V2)
	;
	V2 = nondet, !,
		(V1 = nondet -> Result = nondet ; Result is V1)
	;
	(V2 < V1 -> Result is V2 ; Result is V1).

getMax(V1, V2, Result) :-
	V1 = nondet, !,
		(V2 = nondet -> Result = nondet ; Result is V2)
	;
	V2 = nondet, !,
		(V1 = nondet -> Result = nondet ; Result is V1)
	;
	(V2 > V1 -> Result is V2 ; Result is V1).