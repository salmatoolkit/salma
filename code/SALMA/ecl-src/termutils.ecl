

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
	