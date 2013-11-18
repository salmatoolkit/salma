:- ['../ecl-src/agasmc'].
:- [domaindesc_posstest].


proc(transportToX, [r1:robot, i:item, targetX : integer],
	grab(r1,i) : 
	while(xpos(r1) =\= targetX, 
		if(xpos(r1) < targetX, 
			move_right(r1),
			move_left(r1)
		)
	) :
	drop(r1,i)
	).


domain(robot,D) :-
        D=[rob1,rob2,rob3].

domain(item, D) :- D=[item1, item2, item3].

init :-
	init_agasmc,
	init_sort_hierarchy(_),
	RobotPositions = [p(10,10), p(10,50), p(10,100)],
	ItemPositions = [p(50,10), p(50,50), p(50,100)],
	domain(robot, D1),
	domain(item, D2),	
	(foreach(R, D1), foreach(RPos, RobotPositions), 
		param(D2, ItemPositions) do
		RPos = p(RX, RY),
		set_current(xpos, [R], RX), 
		set_current(ypos, [R], RY), 
		(foreach(I, D2), foreach(IPos, ItemPositions), param(R) do
			IPos = p(IX, IY),
			set_current(carrying, [R, I], false),
			set_current(xpos, [I], IX), 
			set_current(ypos, [I], IY) 
		)
	),
	set_current(time, [], 0),
	assert(wcet(move_right, 5)),
	assert(wcet(move_left, 3)),
	assert(wcet(grab, 11)),
	assert(wcet(drop, 2)).
	
% do2(transportToX(rob1,item1,7),s0,S).

	
	