:- ['../ecl-src/agasmc'].
:- ['domaindesc3'].

domain(robot,D) :-
        D=[rob1,rob2,rob3].

domain(item, D) :- D=[item1, item2, item3].

init :-
	init_agasmc,
	set_current(xpos, [rob1], 20),
	set_current(xpos, [rob2], 30),
	set_current(xpos, [rob3], 40),
	set_current(ypos, [rob1], 20),
	set_current(ypos, [rob2], 30),
	set_current(ypos, [rob3], 40),
	set_current(time, [], 0),
	set_current(global_msg_queue, [], []),
	domain(robot,Robots),
	domain(item, Items),
	init_sort_hierarchy(_),
	(foreach(R, Robots), param(Items) do
		set_current(velocity_x, [R], 0),
		set_current(velocity_y, [R], 0),
		set_current(msg_in_buf, [R], null),
		
		(foreach(I, Items), param(R) do
			set_current(carrying,[R, I], false)
		)
	).
			
		
	
	