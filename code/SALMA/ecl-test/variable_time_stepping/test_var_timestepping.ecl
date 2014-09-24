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
	setVelocity(rob1, 10, 10),
	setPosition(rob2, 100, 0),
	setVelocity(rob2, -10, 10).
	
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
	evaluate_formula(f, [0], 
		invariant, 0, 5, F3, 0, Result, 
		ToSchedule, ScheduleParams, HasChanged),
	printf(" Result=%w\n ToSchedule=%w\n ScheduleParams=%w\n HasChanged=%w",
		[Result, ToSchedule, ScheduleParams, HasChanged]).
	
		
	
	
