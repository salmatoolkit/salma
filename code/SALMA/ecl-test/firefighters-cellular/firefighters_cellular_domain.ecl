:- dynamic 
	knows/3,
	knowledgeValue/4,
	knowledgeTimeStamp/4,
	% firefighter real situation
	posX/3, % x and y 
	posY/3,
	vX/3,
	vY/3,
	cell/3, % c(Col, Row), starting with cell c(0,0)
	destinationCell/3, % destination term: c(Col,Row)
	% state fluents
	movingToDestination/2,
	extinguishing/2,
	conscious/2,
	stuck/2, % moving = not stuck
	gpsAvailable/2, 
	gpsWorking/2,
	% cell
	fireInCell/3,
	smokeInCell/3,
	cellIndoors/3,	
	%constants
	groupLeaderOf/2,
	cellWidth/1, cellHeight/1,
	cellColumnCount/1, cellRowCount/1,
	speedInCell/3.
	

sorts([firefighter, groupMember, groupLeader, ensemble, knowledgeItem,
		beliefItem, cellXPos, cellYPos]).
	
subsorts([groupMember, groupLeader], firefighter).


domain(knowledgeItem, [position, temperature, oxygenLevel,
	acceleration, gmsInDanger, nearbyGMInDanger]).
% gmsInDanger: list

fluent(knowledgeValue, [f:firefighter, ki:knowledgeItem], term).
fluent(knowledgeTimeStamp, [f:firefighter, ki:knowledgeItem], integer).

% fluents that describe the real situation of the firefighter
fluent(posX, [gm:groupMember], integer).
fluent(posY, [gm:groupMember], integer).
fluent(vX, [gm:groupMember], integer).
fluent(vY, [gm:groupMember], integer).
fluent(destinationCell, [gm:groupMember], term).
fluent(temperature, [gm:groupMember], integer).




% qualitative state fluents

fluent(conscious, [gm:groupMember], boolean).
fluent(stuck, [gm:groupMember], boolean).
fluent(gpsAvailable, [gm:groupMember], boolean).
fluent(gpsWorking, [gm:groupMember], boolean).


derived_fluent(knows, [f:firefighter, ki:knowledgeItem], boolean).
derived_fluent(cell, [gm:groupMember], term).

constant(groupLeaderOf, [gl:groupLeader, gm:groupMember], boolean).
constant(speedInCell, [x:cellXPos, y:cellYPos], integer).


primitive_action(setDestinationCell, [gm:groupMember, x:cellXPos,
											y:cellYPos]).
% sense actions
primitive_action(sense, [f:firefighter, ki:knowledgeItem]).

primitive_action(exchangeData, [gl:groupLeader, gm:groupMember]).

primitive_action(setGMsInDanger, [gl:groupLeader, gms:list]).

exogenous_action(getStuck, [gm:groupMember], []).
exogenous_action(breakFree, [gm:groupMember], []).
exogenous_action(passOut, [gm:groupMember], []).
exogenous_action(wakeUp, [gm:groupMember], []).
exogenous_action(loseGPSSignal, [gm:groupMember], []).
exogenous_action(regainGPSSignal, [gm:groupMember], []).
exogenous_action(breakGPS, [gm:groupMember], []).




% SUCCESSOR STATE AXIOMS 
% ======================

knows(F, KI, S) :-
	knowledgeTimeStamp(F, KI, T, S),
	T >= 0.

knowledgeValue(F, gmsInDanger, Val, do2(A,S)) :-
	(A = determineGMsInDanger(F, GMs) ->
		Val = GMs
	;
		knowledgeValue(F, gmsInDanger, Val, S)
	), !.

knowledgeValue(F, nearbyGMInDanger, Val, do2(A,S)) :-
	A = exchangeData(GL, F),
	groupLeaderOf(GL, F),
	knowledgeValue(F, gmsInDanger, GMsInDanger, S),
	(length(GMsInDanger) > 0 ->	Val = true ; Val = false), !
	;
	knowledgeValue(F, nearbyGMInDanger, Val, S), !.

knowledgeValue(F, KI, Val, do2(A,S)) :-
	(
		A = sense(F, KI) ->
			T =.. [KI, F, Val, S],
			call(T)
		;
			knowledgeValue(F, KI, Val, S)
	), !.
	
knowledgeTimeStamp(F, gmsInDanger, TimeStamp, do2(A,S)) :-
	(A = determineGMsInDanger(F, _) ->
		time(TimeStamp, S)
	;
		knowledgeTimeStamp(F, gmsInDanger, TimeStamp, S)
	), !.

knowledgeTimeStamp(F, nearbyGMInDanger, TimeStamp, do2(A,S)) :-
	A = exchangeData(GL, F),
	groupLeaderOf(GL, F),
	time(TimeStamp, S), !
	;
	knowledgeTimeStamp(F, nearbyGMInDanger, TimeStamp, S), !.

knowledgeTimeStamp(F, position, TimeStamp, do2(A,S)) :-
	A = sense(F, position),
	gpsAvailable(F, S),
	time(TimeStamp, S), !
	;
	knowledgeTimeStamp(F, position, TimeStamp, S), !.
	
knowledgeTimeStamp(F, KI, TimeStamp, do2(A,S)) :-
	(
		A = sense(F, KI) ->
			time(TimeStamp, S)
		;
			knowledgeTimeStamp(F, KI, TimeStamp, S)
	), !.	
	
temperature(GM, Temp, do2(A,S)) :-
	closeToFire(GM, do2(A,S)) ->
		Temp is 65
	;
		Temp is 25.

	
conscious(GM, do2(A,S)) :-
	A = wakeUp(GM), !
	;
	A \= passOut(GM),
	conscious(GM, S).

stuck(GM, do2(A, S)) :-
	A = getStuck(GM), !
	;
	A \= breakFree(GM),
	stuck(GM, S).

gpsAvailable(GM, do2(A, S)) :-
	A = regainGPSSignal(GM), !
	;
	A \= loseGPSSignal(GM),
	A \= breakGPS(GM),
	gpsAvailable(GM, S).

gpsWorking(GM, do2(A, S)) :-
	A \= breakGPS(GM),
	gpsWorking(GM, S).

posX(GM, X, do2(A, S)) :-
	posX(GM, OldX, S),
	(A = tick ->
		vX(GM, VX, S),
		X is OldX + VX
		;
		X is OldX
	).

posY(GM, Y, do2(A, S)) :-
	posY(GM, OldY, S),
	(A = tick ->
		vY(GM, VY, S),
		Y is OldY + VY
		;
		Y is OldY
	).	
	
vX(GM, VX, do2(A,S)) :-
	(not conscious(GM, do2(A,S)), ! 
		; 
	stuck(GM, do2(A,S))),
	VX is 0, !
	;
	
cell(GM, Cell, S) :-
	posX(GM, X, S),
	posY(GM, Y, S),
	ColNum is div(X, cellWidth),
	RowNum is div(Y, cellHeight),
	Cell = c(ColNum, RowNum).
	
destinationCell(GM, Cell, do2(A,S)) :-
	(A = setDestinationCell(GM, Col, Row) ->
		Cell = c(Col, Row)
		;
		destinationCell(GM, Cell, S)
	).


	
	
	
	


	



					
