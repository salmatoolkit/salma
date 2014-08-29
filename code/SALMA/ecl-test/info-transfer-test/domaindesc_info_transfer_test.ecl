:- dynamic xpos/3, ypos/3, batteryLevel/3, batteryLevelL/3.

sorts([robot, controller]).
subsort(robot, agent).
subsort(controller, agent).


primitive_action(move_right,[r:robot]).
primitive_action(move_left, [r:robot]).
primitive_action(move_down, [r:robot]).
primitive_action(move_up, [r:robot]).


fluent(xpos, [a:agent], integer).
fluent(ypos, [a:agent], integer).
fluent(batteryLevel, [r:robot], integer).

derived_fluent(dist, [a1:agent, a2:agent], float).

poss(move_right(_), _) :- true.
poss(move_left(_), _) :-true.
poss(move_down(_), _) :-true.
poss(move_up(_), _):-true.

channel(rob2rob, r1:robot, r2:robot, unicast).
channel(con2rob, con:controller, r:robot, multicast).
sensor(batteryLevelL, robot, batteryLevel).
remoteSensor(batteryLevelR, controller, batteryLevelL, robot).

ensemble(batteryLevelR, Con, Robot, S) :-
	dist(Con, Robot, D, S),
	D =< 30.

ensemble(con2rob, Con, Robot, S) :-
	dist(Con, Robot, D, S),
	D =< 20.
	
xpos(Rob, Pos, do2(A,S)) :- 
	xpos(Rob, POld, S),
	(
	 A = move_right(Rob), 
	 Pos is POld + 1, !
	;
	 A = move_left(Rob),
	 Pos is POld - 1, !
	;
	 Pos is POld, !
	).
	
ypos(Rob, Pos, do2(A,S)) :- 
	ypos(Rob, POld, S),
	(
	 A = move_down(Rob), 
	 Pos is POld + 1, !
	;
	 A = move_up(Rob),
	 Pos is POld - 1, !
	;
	 Pos is POld, !
	).
	
batteryLevel(Rob, Level, do2(A,S)) :-
	batteryLevel(Rob, OldLevel, S),
	(
		(A = move_up(Rob), ! ; A = move_right(Rob), ! ; 
			A = move_down(Rob), ! ; A = move_left(Rob), !), !,
			Level is max(OldLevel - 1, 0)
		;
		Level = OldLevel
	).
	


	
dist(A1, A2, Dist, S) :-
	xpos(A1, X1, S),
	ypos(A1, Y1, S),
	xpos(A2, X2, S),
	ypos(A2, Y2, S),
	Dist is sqrt((X2 - X1)^2 + (Y2 - Y1)^2).
		
init_domaindesc :- true.    