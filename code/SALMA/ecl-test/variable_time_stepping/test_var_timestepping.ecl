:- ['../../ecl-src/agasmc'].
:- ['../../ecl-src/test_utils'].
:- [domaindesc_vartimestepping].

setPosition(Agent, X, Y) :-
	set_current(xpos, [Agent], X),
	set_current(ypos, [Agent], Y).

setVelocity(Agent, VX, VY) :-
	set_current(vx, [Agent], VX),
	set_current(vy, [Agent], VY).

	
init :- 
	init_agasmc,
	setDomain(robot, [rob1, rob2]),
	setDomain(controller, [con1, con2]),
	init_sort_hierarchy(_),
	setPosition(rob1, 0, 0),
	setVelocity(rob1, 1, 1),
	setPosition(rob2, 100, 0),
	setVelocity(rob2, -1, 1),
	set_current(time, [], 0).
	
test1(Steps) :-
	init,
	F = forall(r1:robot,
			forall(r2:robot,
				implies(r1 \= r2, 
					not(
						and(
							xpos(r1) =:= xpos(r2),
							ypos(r1) =:= ypos(r2)
						)
					)
				)
			)
		),
	compile_formula(F, F2),
	subst_in_term(s0, do2(tick(Steps),s0), F2, F3),
	printf("F2: %w\nF3: %w\n",[F2, F3]),
	evaluate_formula(f, [0], 0, 0, 10, F3, 0, Result, 
		ToSchedule, ScheduleParams, HasChanged),
	printf(" Result=%w\n ToSchedule=%w\n ScheduleParams=%w\n HasChanged=%w",
		[Result, ToSchedule, ScheduleParams, HasChanged]).
	
		
test2(End, MaxX) :-
	init,
	F = always(xpos(rob1) < MaxX),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	evaluate_formula(f, [0], 0, 0, End, F2, 0, Result, 
		ToSchedule, ScheduleParams, HasChanged),
	printf(" Result=%w\n ToSchedule=%w\n ScheduleParams=%w\n HasChanged=%w",
		[Result, ToSchedule, ScheduleParams, HasChanged]).

test3(TLimit, XTarget) :-
	init,
	F = until(TLimit, xpos(rob1) >= 0, xpos(rob1) > XTarget),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	evaluate_formula(f, [0], 0, 0, 10, F2, 0, Result, 
		ToSchedule, ScheduleParams, HasChanged),
	printf(" Result=%w\n ToSchedule=%w\n ScheduleParams=%w\n HasChanged=%w",
		[Result, ToSchedule, ScheduleParams, HasChanged]),
	print_scheduled_goals(stdout, 2),
	print_formula_cache(stdout).
	
test4(TLimit, XTarget) :-
	init,
	F = until(TLimit, xpos(rob1) >= 0, xpos(rob1) > XTarget),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]).

evstep(TimeDelta) :-
	current_time(T),
	EndTime is T + TimeDelta,
	evaluation_step(EndTime, ToplevelResults, ScheduledResults, 
		PendingGoals, FailureStack),
	printf("%d : %w %w %w %w\n",[T, ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
	print_scheduled_goals(stdout,4),
	print("\n-------\n"),
	print_formula_cache(stdout),
	TD2 is TimeDelta + 1,
	progress([tick(TD2)]).
	
	
	
test5(XTarget, EndTime) :-
	init,
	F = always(
		xpos(rob1) < XTarget
	),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	evaluation_step(EndTime, ToplevelResults, ScheduledResults, 
		PendingGoals, FailureStack),
	
	printf(" ToplevelResults=%w\n ScheduledResults=%w\n PendingGoals=%w\n FailureStack=%w",
		[ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
	print_scheduled_goals(stdout, 2),
	print_formula_cache(stdout).

test6(XTarget, EndTime) :-
	init,
	F = eventually(
		xpos(rob1) > XTarget
	),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	evaluation_step(EndTime, ToplevelResults, ScheduledResults, 
		PendingGoals, FailureStack),
	
	printf(" ToplevelResults=%w\n ScheduledResults=%w\n PendingGoals=%w\n FailureStack=%w",
		[ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
	print_scheduled_goals(stdout, 2),
	print_formula_cache(stdout).	

test6(TLimit, XTarget, EndTime) :-
	init,
	F = always(
		until(TLimit, xpos(rob1) >= 0, xpos(rob1) > XTarget)
	),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	evaluation_step(EndTime, ToplevelResults, ScheduledResults, 
		PendingGoals, FailureStack),
	
	printf(" ToplevelResults=%w\n ScheduledResults=%w\n PendingGoals=%w\n FailureStack=%w",
		[ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
	print_scheduled_goals(stdout, 2),
	print_formula_cache(stdout).
	

test7(Steps, X, EndTime) :-
	init,
	F = always(xpos(rob1) =:= X),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	progress([tick(Steps)]),
	evaluation_step(EndTime, ToplevelResults, ScheduledResults, 
		PendingGoals, FailureStack),
	
	printf(" ToplevelResults=%w\n ScheduledResults=%w\n PendingGoals=%w\n FailureStack=%w",
		[ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
	print_scheduled_goals(stdout, 2),
	print_formula_cache(stdout).

% occur(requestReport(con1, rob1))
test8 :-	
	init,
	F = forall(r:robot,
			forall(c:controller,
				implies(
					occur(activate(c, r)),
					until(50,
						implies(
							occur(startReporting(r, c)),
							until(10,
								and(
									reporting(r, c),
									xpos(r) < 100
								),
								occur(finishReporting(r, c))
							)
						),
						and(
							not(active(r)),
							xpos(r) >= 20
						)
					)
				)
			)
		),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]).

test8_ok :-
	test8,
	progress([activate(con1, rob1)]),
	evstep(10),
	progress([startReporting(rob1, con1)]),
	evstep(5),
	progress([finishReporting(rob1, con1)]),
	evstep(10),
	progress([deactivate(con1, rob1)]),
	evstep(10).
	
test8_one_misses_outer_goal :-
	test8,
	progress([activate(con1, rob1), activate(con2, rob2)]),
	evstep(10),
	progress([startReporting(rob1, con1), startReporting(rob2, con2)]),
	evstep(5),
	progress([finishReporting(rob1, con1), finishReporting(rob2, con2)]),
	evstep(10),
	progress([deactivate(con1, rob1)]),
	evstep(30),
	get_pending_toplevel_goals(PG),
	printf("pending: %w\n", [PG]),
	get_scheduled_goals(FailedGoals, not_ok, 0),
	printf("scheduled toplevel: %w\n", [FailedGoals]),
	FailedGoals = [sg(f, 0, 1, 0, 58)].
	
test8_one_misses_inner_goal :-
	test8,
	progress([activate(con1, rob1), activate(con2, rob2)]),
	evstep(10),
	progress([startReporting(rob1, con1), startReporting(rob2, con2)]),
	evstep(5),
	progress([finishReporting(rob1, con1)]),
	evstep(10),
	progress([deactivate(con1, rob1)]),
	evstep(30),
	get_pending_toplevel_goals(PG),
	printf("pending: %w\n", [PG]),
	get_scheduled_goals(FailedGoals, not_ok, 0),
	printf("scheduled toplevel: %w\n", [FailedGoals]),
	FailedGoals = [sg(f, 0, 1, 0, 58)].