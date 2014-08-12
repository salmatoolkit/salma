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

	
write_step_report(SD, I,ToplevelResults, ScheduledResults, PendingGoals, FailureStack) :-
	printf(SD, "%d : %w %w %w %w\n",[I,ToplevelResults, ScheduledResults, PendingGoals, FailureStack]),
	printf(SD, "Scheduled Goals:\n",[]),
	print_scheduled_goals(SD,4),
	printf(SD, "\n\nFormula Cache:\n",[]),
	print_formula_cache(SD),
	printf(SD, "\n\nCache Candidates:\n",[]),
	print_cache_candidates(SD),
	current_state(_, State2),
	printf(SD, "\n\nState:\n%w\n",[State2]),
	printf(SD, "\n--------------------------------------------\n\n",[]).

report_step(I,ToplevelResults, ScheduledResults, PendingGoals, FailureStack) :-
	sprintf(FName, "testlog/step_%d.log",[I]),
	open(FName, write, SD),
	write_step_report(SD, I,ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
	%write_step_report(stdout, I,ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
	close(SD).
	
	
grabAll :-
	L = [grab(rob1, item1)],
	progress(L).

moveAll :-
	L = [move_right(rob1)],
	progress(L).

	
	
test_var_1 :-
	init,
	F = let(z : xpos(rob1) + ypos(rob1),
		xpos(rob1)*4 > z),
	compile_formula(F, F2),
	printf("F: %w \n F2: %w\n",[F,F2]),
	evaluate_formula(f, [0], 0, F2, 0, Result, 
	ToSchedule, ScheduleParams, HasChanged),
	printf("%w %w %w %w\n",[Result, ToSchedule, ScheduleParams, HasChanged]).
	
test_var_2 :-
	init,
	F = let(z : xpos(rob1) + ypos(rob1),	
		until(10, true, xpos(rob1) > z)),
	compile_formula(F, F2),
	printf("F: %w \n F2: %w\n",[F,F2]),
	evaluate_formula(f, [0], 0, F2, 0, Result, 
	ToSchedule, ScheduleParams, HasChanged),
	printf("%w %w %w %w\n",[Result, ToSchedule, ScheduleParams, HasChanged]).		
				
test_var_3 :-
	init,
	F = forall([r, robot],
			let(z : xpos(r) + ypos(r),	
				until(10, true, xpos(r) > z))),
	compile_formula(F, F2),
	printf("F: %w \n F2: %w\n",[F,F2]),
	evaluate_formula(f, [0], 0, F2, 0, Result, 
	ToSchedule, ScheduleParams, HasChanged),
	printf("%w %w %w %w\n",[Result, ToSchedule, ScheduleParams, HasChanged]).		

test_var_4 :-
	init,
	F = let(z : xpos(rob1) + ypos(rob1) - 5,	
				until(10, true, 
					let(w : xpos(rob1) - 1,
						w > z))),
	register_property(f, F, F2),
	printf("F: %w \n F2: %w\n",[F,F2]),
	
	evaluate_formula(f, [0], 0, F2, 0, Result, ToSchedule, ScheduleParams, HasChanged),
	printf("%w %w %w %w\n",[Result, ToSchedule, ScheduleParams, HasChanged]),
	
	
	(count(I,0,20) do
		report_step(I,ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
		evaluation_step(ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
		moveAll,
		progress([tick])
	).

test_var_5 :-
	init,
	F = let(z : xpos(rob1) + ypos(rob1),	
			let(w : xpos(rob1) - 1, 
				w > 10)),
	compile_formula(F, F2),
	printf("F: %w \n F2: %w\n",[F,F2]),
	evaluate_formula(f, [0], 0, F2, 0, Result, 
	ToSchedule, ScheduleParams, HasChanged),
	printf("%w %w %w %w\n",[Result, ToSchedule, ScheduleParams, HasChanged]).	
	
test_var_6 :-
	init,
	F = let(z : xpos(rob1) + ypos(rob1),	
			let(w : xpos(rob1) - 1, 
				z+ w > 10)),
	compile_formula(F, F2),
	printf("F: %w \n F2: %w\n",[F,F2]),
	evaluate_formula(f, [0], 0, F2, 0, Result, 
	ToSchedule, ScheduleParams, HasChanged),
	printf("%w %w %w %w\n",[Result, ToSchedule, ScheduleParams, HasChanged]).	
	
	
	
	
get_prop_7_1(F) :-
	F = let(z : xpos(rob1) + ypos(rob1) + 5,	
				until(30, 
					implies(
						occur(grab(rob1, item1)),
						let(startx : xpos(rob1),
							until(5, 
								xpos(rob1) >= startx,
								not(carrying(rob1, item1))
							)
						)
					),					
					let(w : xpos(rob1) - 1,
						w > z)
				)
		).
	
	
test_var_7 :-
	init,
	get_prop_7_1(F),	
	register_property(f, F, F2),
	printf("F: %w \n F2: %w\n",[F,F2]),
	
	evaluate_formula(f, [0], 0, F2, 0, Result, ToSchedule, ScheduleParams, HasChanged),
	printf("%w %w %w %w\n",[Result, ToSchedule, ScheduleParams, HasChanged]),
	
	
	(count(I,0,20) do
		time(Time, s0),
		(mod(Time, 6, 0),
			progress([grab(rob1,item1)]), !
			;
		mod(Time, 6, 4),
			progress([drop(rob1, item1)]), !
			;
			true			
		),
		report_step(I,ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
		evaluation_step(ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
		moveAll,
		progress([tick])
	).