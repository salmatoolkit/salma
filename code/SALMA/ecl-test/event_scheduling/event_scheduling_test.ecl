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
	set_current(active, [rob2], false),
	set_current(time, [], 0).
	
	