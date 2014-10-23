:- ['../ecl-src/agasmc'].
:- lib(ic).
:- dynamic vx/3, vy/3, x/3, y/3, ax/3, ay/3.

sort(robot).

fluent(x, [r:robot], float).
fluent(y, [r:robot], float).
fluent(vx, [r:robot], float).
fluent(vy, [r:robot], float).

vx(R, Vx, do2(A, S)) :-
	vx(R, Vx, S).
	
vy(R, Vy, do2(A, S)) :-
	vy(R, Vy, S).
	
ax(R, Ax, do2(A, S)) :-
	ax(R, Vx, S).
	
ay(R, Ay, do2(A, S)) :-
	vy(R, Ay, S).	

x(R, X, do2(A, S)) :-
	x(R, OldX, S),
	(A = tick2(T) ->
		vx(R, Vx, S),
		ax(R, Ax, S),
		X $= OldX + Vx * T + 0.5 * Ax * T^2
		;
		X $= OldX
	).
			
y(R, Y, do2(A, S)) :-
	y(R, OldY, S),
	(A = tick2(T) ->
		vy(R, Vy, S),
		ay(R, Ay, S),
		Y $= OldY + Vy * T + 0.5 * Ay * T^2
		;
		Y $= OldY
	).	
	
init :-
	init_agasmc,
	setDomain(robot, [rob1, rob2]),
	init_sort_hierarchy(_),
	set_current(x, [rob1], 0),
	set_current(y, [rob1], 0),
	set_current(x, [rob2], 100),
	set_current(y, [rob2], 0),
	
	set_current(vx, [rob1], 0),
	set_current(vy, [rob1], 0),
	set_current(vx, [rob2], 0),
	set_current(vy, [rob2], 0),
	
	set_current(ax, [rob1], 1.0),
	set_current(ay, [rob1], 0),
	
	set_current(ax, [rob2], -1.0),
	set_current(ay, [rob2], 0),
	set_current(time, [], 0).
	
	
% show property until(10, x(rob1) < x(rob2), x(rob1) > 20.
test(T, T2, X1, X12, X22) :-
	init,
	T :: 0.0..10.0,
	S = do2(tick2(T), s0),
	x(rob1, X1, S),
	X1 $> 20,
	get_min(T, TMin),
	% TODO: show in which cases this is ok
	% TODO: how to handle nested untils?
	T2 :: 0.0..TMin,
	S2 = do2(tick2(T2), s0),
	x(rob1, X12, S2),
	x(rob2, X22, S2),
	not X12 $>= X22.
	
test2(X, Y) :-
	X :: 0..10,
	Y $= sin(X*0.1),
	Y $> 0.2,
	indomain(X).
	
test3(X, Y, Min) :-
	X :: 0.0..10.0,
	Y $= sin(X*0.1),
	Y $> Min.
	
test4(T, X1, X2) :-
	init,
	set_current(x, [rob1], 10),
	S = do2(tick2(T), s0),
	x(rob1, X1, S),
	x(rob2, X2, S).
	
	