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
	set_current(marking, [item1], 0),
	set_current(marking, [item2], 0),
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

	
	

test :-
	init,
	F = forall([r,robot],
            forall([j,item],
                implies(
                    occur(grab(r,j)),
                    until(15,
                        xpos(r) < 15,
                        xpos(r) > 20
                    )
                )
            )
        ),
	register_property(f, F, _),
	grabAll,
	(count(I,0,20) do
				evaluation_step(ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
				printf("%d : %w %w %w %w\n",[I,ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
				moveAll,
				(I = 5 -> progress([drop(rob1, item1)]) ; true),
				progress([tick(1)])
	).

test1_b :-
	init,
	F = implies(
			occur(grab(rob1,item1)),
			until(5,
				carrying(rob1,item1),
				not(carrying(rob1,item1))
			)
		),
	register_property(f, F, _).

test1_c :-
	init,
	F = implies(
			marking(item1) = 1,
			until(5,
				marking(item1) >= 0,
				marking(item1) = -1
			)
		),
	register_property(f, F, _).
	
test1_d :-
	init,
	F = implies(
			marking(item1) > 0,
			let(x : marking(item1),
				until(5,
					marking(item1) < 2 * x,
					marking(item1) =:= -1 * x
				)
			)
		),
	register_property(f, F, _).
	
test1_e :-
	init,
	F = forall(i : item, 	
			implies(
				marking(i) = 1,
				until(5,
					marking(i) >= 0,
					marking(i) = -1
				)
			)
		),
	register_property(f, F, _).
	
test2 :-
	init,
	F = until(20,
			implies(
				occur(grab(rob1, item1)),
				until(2,
					carrying(rob1,item1),
					not(carrying(rob1,item1))
				)
			),
			xpos(rob1) >= 20
		),
	register_property(f, F, F2),
	printf("F: %w \n F2: %w\n",[F,F2]),
	progress([grab(rob1, item1)]),
	(count(I,0,20) do
				evaluation_step(ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
				printf("%d : %w %w %w %w\n",[I,ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
				moveAll,
				(time(10, s0) ->
					progress([drop(rob1, item1)])
					;
					true
				),
				progress([tick(1)])
	).
	
test2_b :-
	init,
	F = until(20,
			implies(
				xpos(rob1) = 10,
				until(5,
					carrying(rob1,item1),
					not(carrying(rob1,item1))
				)
			),
			xpos(rob1) >= 29
		),
	register_property(f, F, F2),
	grabAll,
	printf("F: %w \n F2: %w\n",[F,F2]).
	
test2_c :-
	init,
	F = until(20,
			until(5,
				carrying(rob1,item1),
				not(carrying(rob1,item1))
			),
			xpos(rob1) >= 25
		),
	register_property(f, F, F2),
	grabAll,
	printf("F: %w \n F2: %w\n",[F,F2]).	

test2_d :-
	init,
	F = until(20,
			implies(
				occur(grab(rob1, item1)),
				until(10,
					carrying(rob1,item1),
					not(carrying(rob1,item1))
				)
			),
			xpos(rob1) >= 29
		),
	register_property(f, F, F2),
	printf("F: %w \n F2: %w\n",[F,F2]),
	progress([grab(rob1, item1)]),
	evaluation_step(3, ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
	printf("%d : %w %w %w %w\n",[0,ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
	progress([tick(3)]),
	moveAll, moveAll, moveAll, moveAll,
	progress([drop(rob1, item1)]),
	evaluation_step(20, ToplevelResults2, ScheduledResults2, PendingGoals2, FailureStack2),
	printf("%d : %w %w %w %w\n",[4,ToplevelResults2, ScheduledResults2, PendingGoals2, FailureStack2]).

test2_e :-
	init,
	F = until(20,
			implies(
				occur(grab(rob1, item1)),
				until(5,
					carrying(rob1,item1),
					not(carrying(rob1,item1))
				)
			),
			xpos(rob1) >= 29
		),
	register_property(f, F, F2),
	%grabAll,
	printf("F: %w \n F2: %w\n",[F,F2]).

test2_f :-
	init,
	F =  forall([r, robot], forall([i, item],
                until(50,
                    implies(
                        occur(grab(r,i)),
                        until(4,
                            carrying(r,i),
                            not(carrying(r,i))
                        )
                    ),
                    xpos(r) >= 20
                )
            )),
	register_property(f, F, F2),
	%grabAll,
	printf("F: %w \n F2: %w\n",[F,F2]).	

	
test2_g :-
	init,
	F = implies(marking(item1) = 1,
			until(20,
				implies(
					occur(grab(rob1, item1)),
					until(10,
						carrying(rob1,item1),
						not(carrying(rob1,item1))
					)
				),
				marking(item1) = 0
			)),
	register_property(f, F, _).		

	
pstep(Num) :-
	Period = 8,
	Delta = 3,
	Start = 0,
	(count(_, 1, Num), param(Period, Delta, Start) do
		current_time(T),
		printf("T = %d\n", [T]),
		M is mod(T - Start, Period), 
		(M =:= 0,
			grabAll, print("  Grab\n"), !
		; M =:= Delta,
			dropAll, print("  Drop\n"), !
		; true
		),
		evstep
	).
	
	
	
test3 :-
	init,
	F = forall([r, robot],
			until(20,
				implies(
					occur(grab(r, item1)),
					until(5,
						true,
						not(carrying(r,item1))
					)
				),
				xpos(r) >= 29
			)
		),
	register_property(f, F, F2),
	printf("F: %w \n F2: %w\n",[F,F2]),
	progress([grab(rob1, item1)]),
	(count(I,0,20) do
				evaluation_step(ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
				printf("%d : %w %w %w %w\n",[I,ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
				moveAll,
				(time(4, s0) ->
					progress([drop(rob1, item1)])
					;
					true
				),
				progress([tick(1)])
	).
	
	
test4 :-
	init,
	F = forall(r:robot, forall(i:item,
			until(20,
				implies(
					occur(grab(r, i)),
					until(5,
						carrying(r,i),
						not(carrying(r,i))
					)
				),
				xpos(r) >= 29
			)
		)),
	open("testlog/test4_log.txt", write, SD),
	register_property(f, F, F2),
	printf(SD, "F: %w \n F2: %w\n",[F,F2]),
	printf(SD, "Before:\n--------------------------------------------\n\n",[]),
	printf(SD, "\n\nToplevel Goals:\n",[]),
	print_toplevel_goals(SD),
	printf(SD, "\n\nScheduled Goals:\n",[]),
	print_scheduled_goals(SD,4),
	printf(SD, "\n\nFormula Cache:\n",[]),
	print_formula_cache(SD),
	printf(SD, "\n\nCache Candidates:\n",[]),
	print_cache_candidates(SD),
	current_state(_, State),
	printf(SD, "\n%w\n",[State]),
	printf(SD, "\n--------------------------------------------\n\n",[]),
	close(SD),
	progress([grab(rob1, item1)]),
	progress([grab(rob2, item2)]),
	(count(I,0,20) do
				current_time(CurrentTime),
				NextTime is CurrentTime + 1,
				evaluation_step(NextTime, ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
				sprintf(FName, "testlog/step_%d.log",[I]),
				open(FName, write, SD2),
				printf(SD2, "%d : %w %w %w %w\n",[I,ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
				printf(SD2, "Scheduled Goals:\n",[]),
				print_scheduled_goals(SD2,4),
				printf(SD2, "\n\nFormula Cache:\n",[]),
				print_formula_cache(SD2),
				printf(SD2, "\n\nCache Candidates:\n",[]),
				print_cache_candidates(SD2),
				current_state(_, State2),
				printf(SD2, "\n\nSate:\n%w\n",[State2]),
				printf(SD2, "\n--------------------------------------------\n\n",[]),
				close(SD2),
				moveAll,
				(time(4, s0) ->
					progress([drop(rob1, item1)]),
					progress([drop(rob2, item2)])
					;
					true
				),
				progress([tick(1)])
	).
	
	
test5 :-
	init,
	F = forall([r,robot],
			implies(
				xpos(r) = 12,
				until(20, xpos(r) > 0, xpos(r) > 16)
			)            
        ),
	register_property(f, F, _),
	grabAll.

	
	
	
test6 :-
	init,
	F = forall([r,robot],
			implies(
				occur(grab(r, item1)),
				until(20, xpos(r) > 0, xpos(r) > 16)
			)            
        ),
	register_property(f, F, _),
	grabAll.

test7 :-
	init,
	F = until(50, xpos(rob1) > 15, xpos(rob1) > 12),
	register_property(f, F, _).
	
evstep :-
	evstep(1).
	
evstep(TimeDelta) :-
	current_time(T),
	T2 is T + TimeDelta -1,
	evaluation_step(T2, ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
	printf("%d : %w %w %w %w\n",[T,ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
	print_scheduled_goals(stdout,4),
	print("\n-------\n"),
	print_formula_cache(stdout),
	print("\n-------\n"),
	print_cache_candidates(stdout),
	moveAll,
	progress([tick(TimeDelta)]).

test8 :-
	init,
	set_current(partner, [rob1], rob2),
	F = (xpos(partner(rob1)) =:= 10),
	compile_formula(F, F2),
	printf("F2: %w\n", [F2]),
	evaluate_ad_hoc(F, Result),
	Result = ok.
	
test_nested :-
	init,
	F = invariant(until(20,
			implies(
				occur(grab(rob1, item1)),
				until(5,
					carrying(rob1,item1),
					not(carrying(rob1,item1))
				)
			),
			xpos(rob1) >= 29
		)),
	register_property(f, F, _).

test_nested2 :-
	init,
	F = until(20,
				implies(
					occur(grab(rob1, item1)),
					until(5,
						carrying(rob1,item1),
						not(carrying(rob1,item1))
					)
				),
				xpos(rob1) >= 29
			),
	register_property(f, F, _).
	
test_nested3 :-
	init,
	F = forall(r:robot, until(20,
				implies(
					occur(grab(r, item1)),
					until(5,
						carrying(r,item1),
						not(carrying(r,item1))
					)
				),
				xpos(r) >= 29
			)),
	register_property(f, F, _).
	
test_nested3b :-
	init,
	F = invariant(forall(r:robot, until(20,
				implies(
					occur(grab(r, item1)),
					until(5,
						carrying(r,item1),
						not(carrying(r,item1))
					)
				),
				xpos(r) >= 29
			))),
	register_property(f, F, _).
	
	
test_nested4 :-
	init,
	F = until(20,
			until(5,
				not(carrying(rob1,item1)),
				carrying(rob1,item1)
			),
			xpos(rob1) >= 29
		),
	register_property(f, F, _).
	
	
test_inv1 :-
	init,
	F = invariant(
			implies(
				occur(grab(rob1, item1)),
				until(5,
					carrying(rob1,item1),
					not(carrying(rob1,item1))
				)
			)
		),
	register_property(f, F, _).
	
test_inv2 :-
	init,
	F = invariant(until(50, xpos(rob1) >= 10, xpos(rob1) > 20)),
	register_property(f, F, _).
	
test_plain_until :-
	init,
	F = until(50, xpos(rob1) >= 10, xpos(rob1) > 20),
	register_property(f, F, _).
	
	
test_nested_eventually :-
	init,
	F = implies(marking(item1) = 1,
			always(20,
				implies(
					occur(grab(rob1, item1)),
					eventually(5,
						not(carrying(rob1,item1))
					)
				)
			)
		),
	register_property(f, F, _).