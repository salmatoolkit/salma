:- ['../ecl-src/agasmc'].
:- [domaindesc].

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
	set_current(time, [], 0).
	
test1(R, L, CFS) :-
	init,
	set_current(carrying, [rob2, item3], true),
	F = forall([r, robot],
			forall([i, item],
				not(carrying(r,i))
			)
		),
	evaluate_ad_hoc(F, R),
	getval(current_failure_stack, CFS),
	findall(FItem, recorded(CFS, FItem), L).
	
test2(R, L, CFS) :-
	init,
	set_current(carrying, [rob2, item3], true),
	F = until(20,
			forall([r,robot],
				ypos(r) < 60),
			ypos(r) > 150
		),
	evaluate_ad_hoc(F, R),
	getval(current_failure_stack, CFS),
	findall(FItem, recorded(CFS, FItem), L).