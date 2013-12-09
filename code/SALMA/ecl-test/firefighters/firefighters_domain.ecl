:- dynamic 
	knows/3,
	knowledgeValue/4,
	knowledgeTimeStamp/4,
	% real world
	position/3, temperature/3, 
	oxygenLevel/3, 	
	acceleration/3, 
	distanceFromEntrance/3,		
	% state fluents
	atBuildingEntrance/2, insideBuilding/2, 
	exploringBuilding/2, leavingBuilding/2, conscious/2,
	stuck/2, closeToFire/2,
	gpsAvailable/2, 
	gpsWorking/2,
	%constants
	groupLeaderOf/2.

sorts([firefighter, groupMember, groupLeader, ensemble, knowledgeItem,
		beliefItem]).
	
subsorts([groupMember, groupLeader], firefighter).


domain(knowledgeItem, [position, temperature, oxygenLevel,
	acceleration, gmsInDanger, nearbyGMInDanger]).
% gmsInDanger: list

fluent(knowledgeValue, [f:firefighter, ki:knowledgeItem], term).
fluent(knowledgeTimeStamp, [f:firefighter, ki:knowledgeItem], integer).

% fluents that describe the real situation

fluent(temperature, [gm:groupMember], integer).

fluent(oxygenLevel, [gm:groupMember], integer).

fluent(acceleration, [gm:groupMember], integer).

% this grows when exploring, i.e. we assume that 
% the firefighter gradually moves deeper into the building
fluent(distanceFromEntrance, [gm:groupMember], integer).

% qualitative state fluents

fluent(exploringBuilding, [gm:groupMember], boolean).
fluent(leavingBuilding, [gm:groupMember], boolean).
fluent(conscious, [gm:groupMember], boolean).
fluent(closeToFire, [gm:groupMember], boolean).
fluent(stuck, [gm:groupMember], boolean).
fluent(gpsAvailable, [gm:groupMember], boolean).
fluent(usingBreathingApparatus, [gm:groupMember], boolean).

derived_fluent(knows, [f:firefighter, ki:knowledgeItem], boolean).
derived_fluent(insideBuilding, [gm:groupMember], boolean).
derived_fluent(atBuildingEntrance, [gm:groupMember], boolean).

constant(groupLeaderOf, [gl:groupLeader, gm:groupMember], boolean).

% GM mission actions
primitive_action(departFromBuilding, [gm:groupMember]).
primitive_action(startExploringBuilding, [gm:groupMember]).
primitive_action(startLeavingBuilding, [gm:groupMember]).

% sense actions
primitive_action(sense, [f:firefighter, ki:knowledgeItem]).

primitive_action(exchangeData, [gl:groupLeader, gm:groupMember]).

primitive_action(setGMsInDanger, [gl:groupLeader, gms:list]).

exogenous_action(arriveAtBuilding, [gm:groupMember], []).
exogenous_action(enterFireRange, [gm:groupMember], []).
exogenous_action(leaveFireRange, [gm:groupMember], []).
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

oxygenLevel(GM, Level, do2(A,S)) :-
	oxygenLevel(GM, OldLevel, S),
	(
		A = tick,
		usingBreathingApparatur(GM, S),
		Level is OldLevel - 1, !
		;
		Level is OldLevel
	), !.
		
acceleration(GM, Acc, do2(A,S)) :-
	A = tick ->
		(
			(stuck(GM, S), ! ; 
				not conscious(GM, S)) ->
				Acc is 0
			;
				Acc is (1-2*frandom) * 10.
		)
	;
	acceleration(GM, Acc, S).
		
distanceFromEntrance(GM, Dist, do2(A,S)) :-
	A = arriveAtBuilding(GM), Dist is 0, !
	;
	distanceFromEntrance(GM, OldDist, S),
	(
		A = tick,
		not stuck(GM, S),
		conscious(GM, S),		
		(			
			exploringBuilding(GM, S),
			Dist is OldDist + 1, !
			;
			leavingBuilding(GM, S),
			Dist is OldDist - 1, !
		), !
		;
		Dist is OldDist
	).
			
atBuildingEntrance(GM, S) :- 
	distanceFromEntrance(GM, Dist, S),
	Dist is 0.

insideBuilding(GM, S) :-
	distanceFromEntrance(GM, Dist, S),
	Dist > 0.

exploringBuilding(GM, do2(A,S)) :-
	A = startExploringBuilding(GM), !
	;
	

	
	


	






					
