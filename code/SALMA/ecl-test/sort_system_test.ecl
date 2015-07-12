:- ['../ecl-src/agasmc'].

sorts([robot, item, station, movable_object]).
subsorts([robot, station], agent).
subsorts([robot, item], movable_object).

init :- 
	init_agasmc,
	setDomain(robot, [rob1, rob2]),
	setDomain(item, [item1, item2]),
	setDomain(station, [base, st2]),
	init_sort_hierarchy(_).