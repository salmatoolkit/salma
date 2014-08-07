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
	L = [move_right(rob1)],
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
				progress([tick])
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
				progress([tick])
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
				progress([tick])
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
	register_property(f, F, F2),
	printf("F: %w \n F2: %w\n",[F,F2]),
	progress([grab(rob1, item1)]),
	progress([grab(rob2, item2)]),
	(count(I,0,20) do
				evaluation_step(ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
				printf("%d : %w %w %w %w\n",[I,ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
				moveAll,
				(time(4, s0) ->
					progress([drop(rob1, item1)]),
					progress([drop(rob2, item2)])
					;
					true
				),
				progress([tick])
	).