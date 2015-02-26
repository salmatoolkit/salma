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
	
test1_b :-
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
	register_property(f, invariant(F), _).
		
test2(TimeDelta, MaxX) :-
	init,
	F = invariant(xpos(rob1) < MaxX),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	evstep(TimeDelta).

test_notemp1(TimeDelta) :-
	init,
	F = (xpos(rob1) =:= 0),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	evstep(TimeDelta).

test_notemp2(TimeDelta) :-
	init,
	F = invariant(xpos(rob1) =:= 0),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	evstep(TimeDelta).
	
test3(TimeDelta, Target) :-
	init,
	F = goal(xpos(rob1) =:= Target),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	evstep(TimeDelta).
	
test4(Deadline, MaxX) :-
	F = always(Deadline, xpos(rob1) < MaxX),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]).

test4_ok_1step :-
	init,
	test4(10, 5),
	evstep(10).
	
	
test5(TimeDelta, Deadline, Target) :-
	init,
	F = eventually(Deadline, xpos(rob1) =:= Target),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	evstep(TimeDelta).
	
test6(TimeDelta, TLimit, XTarget) :-
	init,
	F = until(TLimit, xpos(rob1) >= 0, xpos(rob1) > XTarget),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	evstep(TimeDelta).

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
	


test7(TimeDelta, TLimit, XTarget) :-
	init,
	F = invariant(
		until(TLimit, xpos(rob1) >= 0, xpos(rob1) > XTarget)
	),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	evstep(TimeDelta).
	


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