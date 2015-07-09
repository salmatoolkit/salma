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
		setConstant(robot_radius, [R, 10]),
		(foreach(I, D2), param(R) do
			set_current(carrying, [R, I], false)
		),
		set_current(partner, [R], none)
	),
	set_current(time, [], 0).


	
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
	progress([tick(TimeDelta)]).

test01 :-
	init,
	F = forall(i:item, marking(i) < 10),
	register_property(f, F, _).
	
