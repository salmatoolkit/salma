:- ['../ecl-src/agasmc'].
:- ['kroiss_aamas2014_domaindesc.ecl'].


init :- 
	init_agasmc,
	setDomain(robot, [rob1, rob2, rob3]),
	setDomain(item, [item1, item2, item3]),
	init_sort_hierarchy(_),
	set_current(xpos, [rob1], 10),
	set_current(xpos, [rob2], 10),
	set_current(xpos, [rob3], 10),
	
	domain(robot, D1),
	domain(item, D2),
	(foreach(R, D1), param(D2) do
		(foreach(I, D2), param(R) do
			set_current(carrying, [R, I], false)
		)
	),
	(foreach(I, D2) do
		set_current(xpos, [I], 10)
	),
	set_current(time, [], 0).

test_carry :-
	init,
	progress([grab(rob1,item1), move_right(rob1)]),
	xpos(rob1, 11, s0),
	xpos(item1, 11, s0).

	
grabAll :-
	L = [grab(rob1, item1),grab(rob2, item2),grab(rob3, item3)],
	progress(L).

moveAll :-
	L = [move_right(rob1),move_right(rob2),move_right(rob3)],
	progress(L).

	
test :-
	init,
	F = forall([r,robot],
            forall([j,item],
                implies(
                    occur(grab(r,j)),
                    until(12,
                        carrying(r,j),
                        xpos(j) > 20
                    )
                )
            )
        ),
	register_property(f, F, _),
	grabAll,
	(count(I,0,20) do
				evaluation_step(ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
				printf("%d : %w %w %w %w\n",[I,ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
				moveAll
	).