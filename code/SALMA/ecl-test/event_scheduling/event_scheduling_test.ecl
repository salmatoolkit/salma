:- ['../../ecl-src/agasmc'].
:- ['../../ecl-src/test_utils'].
:- [domaindesc_event_scheduling].

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
	setVelocity(rob1, 1, 0),
	setPosition(rob2, 50, 0),
	setVelocity(rob2, -1, 0),
	set_current(active, [rob1], true),
	set_current(active, [rob2], true),
	set_current(wheels_wet, [rob1], false),
	set_current(wheels_wet, [rob2], true),
	set_current(time, [], 0).
	
test_schedulable_1 :-
	init,
	setPosition(rob1, 1000, 90),
	get_all_schedulable_event_instances(s0, [], C),
	C = [].

test_schedulable_2 :-
	init,
	setPosition(rob1, 1000, 110),
	get_all_schedulable_event_instances(s0, [], C),
	C = [lightning_strike : [[rob1]]].

test_schedulable_3 :-
	init,
	setPosition(rob1, 1000, 90),
	setVelocity(rob1, 1, 1),
	get_next_schedulable_events(100, [], T, C),
	print([T,C]).

test_schedulable_4 :-
	init,
	setPosition(rob1, 1000, 110),
	get_all_schedulable_event_instances(s0, [ev(2, collide, [rob1, rob2]), ev(5, lightning_strike, [rob2])], C),
	print(C).
	
test_all :-
	test_schedulable_1,
	test_schedulable_2.
	