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
	setConstant(safety_distance, [10]),
	setConstant(world_width, [500]),
	setConstant(world_height, [500]),
	set_current(time, [], 0).
	
test_ad_hoc_1 :-
	init,
	get_next_possible_ad_hoc_events(0, 100, [], Time, Events),
	Time = 0,
	Events = [slip : [[rob2]]].

test_ad_hoc_2 :-
	init,
	Handled = [ev(0, slip, [rob2])],
	get_next_possible_ad_hoc_events(0, 100, Handled, Time, Events),
	printf("Time = %w, Events = %w\n", [Time, Events]),
	Time = 1,
	Events = [slip : [[rob2]]].

test_ad_hoc_3 :-
	init,
	Handled = [ev(0, slip, [rob2]), ev(1, slip, [rob2])],
	get_next_possible_ad_hoc_events(0, 100, Handled, Time, Events),
	printf("Time = %w, Events = %w\n", [Time, Events]),
	Time = 2,
	Events = [slip : [[rob2]]].	

test_ad_hoc_4 :-
	init,
	set_current(wheels_wet, [rob2], false),
	get_next_possible_ad_hoc_events(0, 100, [], Time, Events),
	printf("Time = %w, Events = %w\n", [Time, Events]),
	Time = 20, Events = [collide : [[rob1, rob2]]].
	
	
test_schedulable_single_1 :-
	init,
	setPosition(rob1, 1000, 40),
	setVelocity(rob1, 1, 1),
	
	get_next_schedulable_events(0, 100, [], [], 
		Time, Events),
	printf("Time = %w, Events = %w\n", [Time, Events]),
	Time = 11, Events =   [welcome_message : [[rob1]]].

test_schedulable_single_2 :-
	init,
	setPosition(rob1, 1000, 40),
	setVelocity(rob1, 1, 1),
	
	get_next_schedulable_events(0, 8, [], [], 
		Time, Events),
	printf("Time = %w, Events = %w\n", [Time, Events]),
	Time = -1, Events =  [].

test_schedulable_single_3 :-
	init,
	setPosition(rob1, 1000, 60),
	setVelocity(rob1, 1, 1),
	
	get_next_schedulable_events(0, 8, [], [], 
		Time, Events),
	printf("Time = %w, Events = %w\n", [Time, Events]),
	Time = 0, Events = [welcome_message : [[rob1]]].

test_schedulable_single_4 :-
	init,
	setPosition(rob1, 1000, 60),
	setVelocity(rob1, 1, 1),
	Handled = [ev(0, welcome_message, [rob1])],
	get_next_schedulable_events(0, 100, [], Handled, 
		Time, Events),
	printf("Time = %w, Events = %w\n", [Time, Events]),
	Time = 1, Events = [welcome_message : [[rob1]]].
	
test_schedulable_single_5 :-
	init,
	setPosition(rob1, 1000, 60),
	setVelocity(rob1, 1, 1),
	Schedule = [ev(31, welcome_message, [rob1])],
	get_next_schedulable_events(0, 35, Schedule, [], 
		Time, Events),
	printf("Time = %w, Events = %w\n", [Time, Events]),
	Time = -1, Events = [].
	
test_schedulable_choice_1 :-
	init,
	setPosition(rob1, 500, 101),
	setVelocity(rob1, 1, 1),
	Handled = [ev(0, disaster, [rob1])],
	get_next_schedulable_events(0, 100, [], Handled, 
		Time, Events),
	printf("Time = %w, Events = %w\n", [Time, Events]),
	Time = 1, Events = [disaster : [[rob1]]].

test_schedulable_choice_2 :-
	init,
	setPosition(rob1, 500, 101),
	setVelocity(rob1, 1, 1),
	Schedule = [ev(10, lightning_strike, [rob1])],
	get_next_schedulable_events(0, 100, Schedule, [], 
		Time, Events),
	printf("Time = %w, Events = %w\n", [Time, Events]),
	Time = -1, Events =  [].	
	

test_all :-
	test_schedulable_1,
	test_schedulable_2.
	