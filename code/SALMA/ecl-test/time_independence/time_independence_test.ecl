:- ['../../ecl-src/agasmc'].
:- ['../../ecl-src/test_utils'].
:- [domaindesc_time_independence].

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
	set_current(active, [rob1], true),
	set_current(active, [rob2], false),
	set_current(time, [], 0).
	
test1 :-
	init,
	% test that proper "normal-form" SSAs have been constructed
	xpos(rob1, 6, do2(tick(6), s0)),
	xpos(rob1, 0, s0),
	xpos(rob1, 6, do2(activate(rob2), do2(tick(6), s0))),
	F1 = (xpos(rob1) + vx(rob1)*3 > 0),
	compile_formula(F1, F1C),
	F2 = forall(r:robot, active(r)),
	compile_formula(F2, F2C),
	term_affected_by_action(F1C, tick(_)),
	not term_affected_by_action(F2C, tick(_)),
	not term_affected_by_action(F1C, jump(rob2, 100,100)),
	F3 = and(xpos(rob1) + ypos(rob2)*3 < 100, xpos(rob2) > 0),
	compile_formula(F3, F3C),
	term_affected_by_action(F3C, jump(rob2, 100,100)).
	

cnf_test :-
	init,
	F = implies(active(rob1), xpos(rob1) > 4 and xpos(rob1) < 100),
	compile_formula(F, F2),
	printf("F2: %w\n",[F2]).
	
	

	
	