:- ['../../ecl-src/agasmc'].
:- ['e-mobility-domain-with-abstractions'].

assignment__sam(Sam, do2(A,Sit)) :- true.


init :-
	init_agasmc,
	setDomain(crossing, [c1, c2, c3, c4]),
	setDomain(vehicle, [vehicle1, vehicle2]),
	setDomain(plcs, [plcs1, plcs2]),
	setDomain(poi, [poi1, poi2]),
	setDomain(plcssam, [sam1]),
	init_sort_hierarchy(_),
	
	%setConstant(connected, [c1,c2, true]),
	%setConstant(connected, [c2,c3, true]),
	%setConstant(connected, [c3,c4, true]),
	%setConstant(connected, [c4,plcs1, true]),
	
	set_current(vehiclePosition, [vehicle1], pos(c1,c1,0)),
	set_current(currentRoute, [vehicle1], [c1,c2,c3,c4,plcs1]),
	set_current(vehicleSpeed, [vehicle1], 10),
	set_current(time, [], 0).
	
	
	