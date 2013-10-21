
get_subterm(TermIn, [Pos], TermOut) :-
	(Pos is 0 -> TermOut = TermIn ; arg(Pos, TermIn, TermOut)
	), !.
	
get_subterm(TermIn, [Pos | PTail], TermOut) :-
	(Pos is 0 -> T = TermIn ; arg(Pos, TermIn, T)),
	get_subterm(T, PTail, TermOut), !.
	