streamtest(X, Contents) :-
	open(string("foobar"), update, S),
    read(S,X),
	close(S).