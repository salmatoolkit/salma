foo(X):-
	trace_point_port(one,Invoc,X>3),
	X > 3,
	write("one\n"),
	trace_point_port(two,Invoc,X>6),
	X > 6,
	write("two\n").
	