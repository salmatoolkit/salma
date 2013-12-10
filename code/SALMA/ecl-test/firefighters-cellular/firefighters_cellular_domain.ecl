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
	temperature/3,
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
	groupLeader/3, % defines the groupLeader
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
fluent(destinationCell, [gm:groupMember], term).

% cell state

fluent(fireInCell, [x:cellXPos, y:cellYPos], boolean).
derived_fluent(smokeInCell, [x:cellXPos, y:cellYPos], boolean).
derived_fluent(fireInNeighbourCell, [x:cellXPos, y:cellYPos], boolean).

% qualitative state fluents

fluent(conscious, [gm:groupMember], boolean).
fluent(stuck, [gm:groupMember], boolean).
fluent(gpsWorking, [gm:groupMember], boolean).


derived_fluent(knows, [f:firefighter, ki:knowledgeItem], boolean).
derived_fluent(cell, [gm:groupMember], term).
derived_fluent(vX, [gm:groupMember], integer).
derived_fluent(vY, [gm:groupMember], integer).
derived_fluent(temperature, [gm:groupMember], integer).
derived_fluent(gpsAvailable, [gm:groupMember], boolean).



constant(groupLeader, [gm:groupMember], groupLeader).
constant(speedInCell, [x:cellXPos, y:cellYPos], integer).
constant(cellIndoors, [x:cellXPos, y:cellYPos], boolean).


primitive_action(setDestinationCell, [gm:groupMember, x:cellXPos,
											y:cellYPos]).
primitive_action(startExtinguishing, [gm:groupMember]).			
primitive_action(stopExtinguishing, [gm:groupMember]).									
											
% sense actions
primitive_action(sense, [f:firefighter, ki:knowledgeItem]).

primitive_action(exchangeData, [gl:groupLeader, gm:groupMember]).

primitive_action(setGMsInDanger, [gl:groupLeader, gms:list]).

exogenous_action(getStuck, [gm:groupMember], []).
exogenous_action(breakFree, [gm:groupMember], []).
exogenous_action(passOut, [gm:groupMember], []).
exogenous_action(wakeUp, [gm:groupMember], []).
exogenous_action(breakGPS, [gm:groupMember], []).

exogenous_action(fireStarts, [x:cellXPos, y:cellYPos], []).
exogenous_action(fireStops, [x:cellXPos, y:cellYPos], []).

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
	groupLeader(F, GL),
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
	groupLeader(F, GL),
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
	
temperature(GM, Temp, S) :-
	cell(GM, Cell, S),
	
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


gpsWorking(GM, do2(A, S)) :-
	A \= breakGPS(GM),
	gpsWorking(GM, S).

gpsAvailable(GM, S) :-
	gpsWorking(GM, S),
	cell(GM, 

	
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
	
vX(GM, VX, S) :-
	(not conscious(GM, S), ! 
		; 
	stuck(GM, S),
	VX is 0, !
	;
	cell(GM, Cell, S),
	Cell = c(ColNum, RowNum),
	speedInCell(ColNum, RowNum, VX).

	
vY(GM, VY, S) :-
	(not conscious(GM, S), ! 
		; 
	stuck(GM, S),
	VY is 0, !
	;
	cell(GM, Cell, S),
	Cell = c(ColNum, RowNum),
	speedInCell(ColNum, RowNum, VY).
	
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

% --------------
% CELL FLUENTS
% --------------

fireInCell(CellX, CellY, do2(A,S)) :-
	A = fireStarts(CellX, CellY), !
	;
	A \= fireStops(CellX, CellY),
	fireInCell(CellX, CellY, S).

fireInNeighbourCell(CellX, CellY, S) :-
	(foreach(DX, [-1,0,1]) do
		foreach(DY, [-1,0,1]), param(DX) do
	
smokeInCell(CellX, CellY, S) :-
	(
			
	

	
	
	


	



					
