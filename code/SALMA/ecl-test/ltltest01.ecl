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
	domain(robot, D1),
	domain(item, D2),
	(foreach(R, D1), param(D2) do
		(foreach(I, D2), param(R) do
			set_current(carrying, [R, I], false)
		)
	),
	set_current(time, [], 0).

test_carry :-
	init,
	progress([grab(rob1,item1), move_right(rob1)]),
	xpos(rob1, 11, s0).

	
grabAll :-
	L = [grab(rob1, item1)],
	progress(L).

moveAll :-
	L = [move_right(rob1), move_right(rob2)],
	progress(L).

	
	

test :-
	init,
	F = forall([r,robot],
            forall([j,item],
                implies(
                    occur(grab(r,j)),
                    until(12,
                        carrying(r,j),
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
				progress([tick(1)])
	).
	
test2 :-
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
				until(5,
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
	progress([tick(4)]),
	moveAll, moveAll, moveAll, moveAll,
	progress([drop(rob1, item1)]),
	evaluation_step(20, ToplevelResults2, ScheduledResults2, PendingGoals2, FailureStack2),
	printf("%d : %w %w %w %w\n",[4,ToplevelResults2, ScheduledResults2, PendingGoals2, FailureStack2]).

	
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
	F = forall([r, robot], forall([i, item],
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
				
				evaluation_step(ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
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
	
evstep :-
	current_time(T),
	evaluation_step(ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
	printf("%d : %w %w %w %w\n",[T,ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
	print_scheduled_goals(stdout,4),
	print("\n-------\n"),
	print_formula_cache(stdout),
	print("\n-------\n"),
	print_cache_candidates(stdout),
	moveAll,
	progress([tick(1)]).
	