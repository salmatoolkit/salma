subst_in_term(Var, Rep, T, T2):-
        (T = Var ->
            T2 = Rep
        ;
            arity(T,N),
            (N is 0 ->
                T2 = T
            ;
                T =.. Tl,
                subst_in_list(Var, Rep, Tl, Tl2),
                T2 =.. Tl2
            )
        ).


subst_in_list(Var, Rep, [H], Tl2) :-
        subst_in_term(Var, Rep, H, T2),
        Tl2 = [T2].

subst_in_list(Var,Rep,[H | Tl], Tl2) :-
		subst_in_term(Var, Rep, H, T2),
        subst_in_list(Var,Rep,Tl,Tl3),
        Tl2 = [T2 | Tl3].
		
get_subterm(TermIn, [Pos], TermOut) :-
	(Pos is 0 -> TermOut = TermIn ; arg(Pos, TermIn, TermOut)
	), !.
	
get_subterm(TermIn, [Pos | PTail], TermOut) :-
	(Pos is 0 -> T = TermIn ; arg(Pos, TermIn, T)),
	get_subterm(T, PTail, TermOut), !.
	