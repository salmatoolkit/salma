:- ['../ecl-src/agasmc_progression'].
:- ['../ecl-src/property_evaluator'].
:- [domaindesc2].

domain(robot,D) :-
        D=[rob1,rob2,rob3].

domain(item, D) :- D=[item1, item2, item3].

init :-
	init_progression,
	init_smc,
	set_current(xpos, [rob1], 20),
	set_current(xpos, [rob2], 30),
	set_current(xpos, [rob3], 40),
	set_current(ypos, [rob1], 20),
	set_current(ypos, [rob2], 30),
	set_current(ypos, [rob3], 40),
	set_current(velocity_x, [rob1], 0),
	set_current(velocity_x, [rob2], 0),
	set_current(velocity_x, [rob3], 0),
	set_current(velocity_y, [rob1], 0),
	set_current(velocity_y, [rob2], 0),
	set_current(velocity_y, [rob3], 0),
	set_current(carrying,[rob1, item1], false),
	set_current(carrying,[rob2, item2], false),
	set_current(carrying,[rob3, item3], false),
	set_current(time, [], 0).
	
test1(Result, ToSchedule) :-
	checkall([xpos(rob1, P1, s0), P1 > 20, 
				checkuntil(
					checkall([xpos(rob1, P2, s0), P2 > 25], R1, T1), % P
					checkall([xpos(rob1, P3, s0), P3 > 30], R2, T2), % Q
					R3)],
				Result,
				ToSchedule).
								
	