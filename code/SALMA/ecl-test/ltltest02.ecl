:- ['../ecl-src/agasmc'].
:- [domaindesc].

init :- 
	init_agasmc,
	setDomain(robot, [rob1, rob2]),
	setDomain(item, [item1, item2]),
	init_sort_hierarchy(_),
	set_current(xpos, [rob1], 10),
	set_current(ypos, [rob1], 10),
	set_current(xpos, [rob2], 10),
	set_current(ypos, [rob2], 20),
	set_current(vx, [rob1], 0),
	set_current(vy, [rob1], 0),
	set_current(vx, [rob2], 0),
	set_current(vy, [rob2], 0),
	domain(robot, D1),
	domain(item, D2),
	(foreach(R, D1), param(D2) do
		(foreach(I, D2), param(R) do
			set_current(carrying, [R, I], false)
		),
		set_current(partner, [R], none)
	),
	set_current(time, [], 0).

test_carry :-
	init,
	progress([grab(rob1,item1), move_right(rob1)]),
	xpos(rob1, 11, s0).

	
grabAll :-
	L = [grab(rob1, item1), grab(rob2, item2)],
	progress(L).

dropAll :-
	L = [drop(rob1, item1), drop(rob2, item2)],
	progress(L).
	
moveAll :-
	L = [move_right(rob1), move_right(rob2), finish_step(rob1), finish_step(rob2)],
	progress(L).

	
test1 :-
	init,
	F = (xpos(rob1) < 12),
	register_property(f, F, _).

test2 :-
	init,
	F = invariant(xpos(rob1) < 15),
	register_property(f, F, _).

test3(TimeLimit, XMax) :-
	init,
	F = always(TimeLimit, xpos(rob1) < XMax),
	register_property(f, F, _).
	
test4(TimeLimit, Target) :-
	init,
	F = eventually(TimeLimit, xpos(rob1) =:= Target),
	register_property(f, F, _).
	
test4b(TimeLimit, Target) :-
	init,
	F = invariant(eventually(TimeLimit, xpos(rob1) =:= Target)),
	register_property(f, F, _).
	
test5 :-
	init,
	F = always(50, eventually(5, (xpos(rob1) mod 5) =:= 0)),
	register_property(f, F, _).

test6 :-
	init,
	F = until(50, xpos(rob1) > 5, xpos(rob1) =:= 15),
	register_property(f, F, _).

test7 :-
	init,
	F = until(50, xpos(rob1) > 5, occur(grab(rob1, item1))),
	register_property(f, F, _).

test8 :-
	init,
	F = forall(r:robot,
			implies(
				occur(paint(r,?)),
				until(40,
					true,
					xpos(r) >= 15
				)
			)
		),
	register_property(f, F, _).

test9 :-
	init,
	F = invariant(forall(r:robot,
			implies(
				occur(paint(r,?)),
				until(40,
					true,
					xpos(r) >= 15
				)
			)
		)),
	register_property(f, F, _).	

test10 :-
	init,
	F = until(40,
			implies(
				occur(paint(rob1,item1)),
				until(10,
					true,
					occur(paint(rob2, item2)))),
			occur(paint(rob1, item2))),
	register_property(f, F, _).	

test11 :-
	init,
	F = until(40,
			implies(
				occur(paint(rob1,item1)),
				let(x1 : xpos(rob1),
					eventually(10, xpos(rob1) > x1))
			),			
			occur(paint(rob1, item2))),
	register_property(f, F, _).
	
	
evstep :-
	evstep(1).
	
evstep(TimeDelta) :-
	current_time(T),
	T2 is T + TimeDelta,
	evaluation_step(T2, ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
	printf("%d : \n Toplevel: %w \n Scheduled: %w \n Pending: %w \n FailureStack: %w\n",
		[T,ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
	print_scheduled_goals(stdout,4),
	print("\n-------\n"),
	print_formula_cache(stdout),
	print("\n-------\n"),
	print_cache_candidates(stdout),
	moveAll,
	progress([tick(TimeDelta)]).
