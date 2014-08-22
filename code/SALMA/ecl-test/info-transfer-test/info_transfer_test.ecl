:- ['../../ecl-src/agasmc'].
:- [domaindesc_info_transfer_test].

setPosition(Agent, X, Y) :-
	set_current(xpos, [Agent], X),
	set_current(ypos, [Agent], Y).

init :- 
	init_agasmc,
	setDomain(robot, [rob1, rob2]),
	setDomain(controller, [con1, con2]),
	init_sort_hierarchy(_),
	setPosition(rob1, 20, 20),
	setPosition(rob2, 55, 30),
	setPosition(con1, 30, 30),
	setPosition(con2, 50, 40),
	
	domain(robot, D1),
	(foreach(R, D1) do	
		set_current(batteryLevel, [R], 100)
	),	
	setDomain(channel, [rob2rob, con2rob]),
	setDomain(sensor, [batteryLevelL]),
	setDomain(remoteSensor, [batteryLevelR]),	
	setDomain(message, []),
	init_sort_hierarchy(_),

	domain(channel, Channels),
	(foreach(C, Channels) do
		set_current(channel_in_queue, [C], [])
	),	
	set_current(time, [], 0).

