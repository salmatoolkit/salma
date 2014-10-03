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
	
test4(TLimit, XTarget, EndTime) :-
	init,
	F = until(TLimit, xpos(rob1) >= 0, xpos(rob1) > XTarget),
	register_property(f, F, F2),
	printf("F2: %w\n",[F2]),
	evaluation_step(EndTime, ToplevelResults, ScheduledResults, 
		PendingGoals, FailureStack),
	
	printf(" ToplevelResults=%w\n ScheduledResults=%w\n PendingGoals=%w\n FailureStack=%w",
		[ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
	print_scheduled_goals(stdout, 2),
	print_formula_cache(stdout).
