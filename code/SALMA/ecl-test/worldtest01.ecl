:- ['../ecl-src/agasmc'].
:- ['../ecl-src/test_utils'].
:- ['domaindesc'].

setPosition(Agent, X, Y) :-
	set_current(xpos, [Agent], X),
	set_current(ypos, [Agent], Y).

setVelocity(Agent, VX, VY) :-
	set_current(vx, [Agent], VX),
	set_current(vy, [Agent], VY).

init :-
	init_agasmc,
	setDomain(robot, [rob1, rob2]),
	setDomain(item, [item1, item2]),
	init_sort_hierarchy(_),
	setPosition(rob1, 0, 0),
	%setVelocity(rob1, 0, 0),
	setPosition(rob2, 10, 10),
	setVelocity(rob2, 0, 0),
	domain(robot, Robots, s0),
	domain(item, Items, s0),
	setConstant(gravity, [9.81]),
	(foreach(R, Robots), param(Items) do
		setConstant(robot_radius, [R, 1]),
		set_current(active, [R], true),
		set_current(partner, [R], none),
		(foreach(I, Items), param(R) do
			set_current(painted, [I], false),
			set_current(marking, [I], none),
			set_current(carrying, [R, I], false)
		)
	),
	set_current(time, [], 0).

test_steps :-
	init,
	progress([move_right(rob1), move_down(rob2)]),
	get_next_schedulable_events(0, 100, [], [], 
		Time, Events),
	Time =:= 0,
	Events = [finish_step : [[rob1], [rob2]]],
	progress([tick(5), finish_step(rob1)]),
	xpos(rob1, 1, s0),
	time(5, s0),
	vx(rob1, 0, s0),
	vy(rob1, 0, s0),
	vx(rob2, 0, s0),
	vy(rob2, 1, s0).
	
		