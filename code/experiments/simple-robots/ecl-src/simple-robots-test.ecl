:- ['../../../SALMA/ecl-src/agasmc'].
:- ['simple-robots-domaindesc.ecl'].

init :- 
	init_agasmc,
	setDomain(robot, [rob1, rob2]),
	setDomain(item, [item1, item2]),
	setDomain(station, [base]),
	init_sort_hierarchy(_),
	set_current(xpos, [rob1], 10),
	set_current(ypos, [rob1], 10),
	set_current(xpos, [rob2], 10),
	set_current(ypos, [rob2], 20),
	set_current(marking, [item1], 0),
	set_current(marking, [item2], 0),
	set_current(vx, [rob1], 0),
	set_current(vy, [rob1], 0),
	set_current(vx, [rob2], 0),
	set_current(vy, [rob2], 0),
	domain(robot, D1),
	domain(item, D2),
	(foreach(R, D1), param(D2) do
		setConstant(robot_radius, [R, 10]),
		set_current(broken, [R], false),
		(foreach(I, D2), param(R) do
			set_current(carrying, [R, I], false)
		),
		set_current(partner, [R], none)
	),
	set_current(time, [], 0).