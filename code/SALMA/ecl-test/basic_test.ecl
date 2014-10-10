:- ['../ecl-src/agasmc'].
:- [domaindesc].
:- [example_procedures].

init :-
	init_agasmc,
	setDomain(robot, [rob1, rob2]),
	setDomain(item, [item1, item2]),
	init_sort_hierarchy(_),
	set_current(xpos, [rob1], 20),
	set_current(ypos, [rob1], 20),
	set_current(xpos, [rob2], 50),
	%set_current(ypos, [rob2], 50),
	set_current(carrying, [rob1, item1], true),
	set_current(time, [], 0).
	

proc(test, [r1:robot, i:item, r2:robot],
		move_right(r1) : paint(r1,i) : move_left(r2)
	).
	
proc(test2, [r1:robot, i:item, targetX : integer],
	grab(r1,i) : 
	while(xpos(r1) < targetX, 
		move_right(r1)
	) :
	drop(r1,i)
	).
	
test1(F,Result) :-
	compile_formula(
		xpos(rob1) > 20, 
		F, 
		do2(move_right(rob1),s0)), 
	current_time(T),
	evaluate_formula(null, [0], T, T, F, 0, Result, _, _, _).