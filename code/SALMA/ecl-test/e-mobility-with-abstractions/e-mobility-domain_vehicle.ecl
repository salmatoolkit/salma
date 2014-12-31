:- dynamic vehiclePosition/3,
	currentRoute/3, currentTargetPOI/3, currentRoad/3,
	currentLocation/3, currentTarget/3,
	nextRoad/3, currentPLCS/3, currentTargetPLCS/3,
	calendar/2, waitingForAssignment/2, waitingForReservation/2.
	
% VEHICLE

%% Stores the vehicle's schedule as list of POIs together with the time
%% interval this POI should be visited.
constant(calendar, [veh:vehicle], list).
doc(calendar : constant,[
	summary: "Stores the vehicle's schedule as list of POIs together with the time
				interval this POI should be visited.",
	desc: "list entry format:
			cal(poi, startTime, plannedDuration)"
	]).

%% either l(loc) for crossing, plcs, poi, or r(road)
fluent(vehiclePosition, [veh:vehicle], term).

% none when driving	
fluent(currentPLCS, [veh:vehicle], plcs).

fluent(currentTargetPOI, [veh:vehicle], poi).

fluent(currentTargetPLCS, [veh:vehicle], plcs).

fluent(waitingForAssignment, [veh:vehicle], boolean).
fluent(waitingForReservation, [veh:vehicle], boolean).

% route is given as the remaining list of roads
fluent(currentRoute, [veh:vehicle], list).
derived_fluent(currentRoad, [veh:vehicle], road).
derived_fluent(currentLocation, [veh:vehicle], location).
derived_fluent(currentTarget, [veh:vehicle], location).
derived_fluent(nextRoad, [veh:vehicle], road).

derived_fluent(hasTargetPLCS, [veh:vehicle], boolean).

primitive_action(setTargetPLCS, [veh:vehicle, target:plcs]).

poss(setTargetPLCS(_,_),_) :- true.

primitive_action(setRoute, [veh:vehicle, route:list]).
poss(setRoute(_,_), _) :- true.

primitive_action(setPOI, [veh:vehicle, p:poi]).
poss(setPOI(_,_), _) :- true.
	
	
% TODO: make 
% TODO: make % TODO: make 
exogenous_action(driverLeavesPLCS, [veh:vehicle], []).
schedulable(driverLeavesPLCS(Vehicle), S) :-
	currentPLCS(Vehicle, PLCS, S),
	PLCS \= none.

exogenous_action(driverParksAtPLCS, [veh:vehicle, p:plcs], []).
schedulable(driverParksAtPLCS(Vehicle), S) :-
	currentPLCS(Vehicle, CurrentPLCS, S),
	CurrentPLCS = none,
	vehiclePosition(Vehicle, Pos, S),
	Pos = l(Loc),
	domain(plcs, PLCS), 
	member(Loc, PLCS).


exogenous_action(arriveAtRoadEnd, [veh:vehicle], []).
schedulable(arriveAtRoadEnd(Vehicle), S) :-
	vehiclePosition(Vehicle, Pos, S),
	Pos = r(_).
		
exogenous_action(enterNextRoad, [veh:vehicle], []).
schedulable(enterNextRoad(Vehicle), S) :-
	vehiclePosition(Vehicle, Pos, S),
	Pos = l(_),
	nextRoad(Vehicle, NextRoad, S),
	NextRoad \= none.
	
	
% SUCCESSOR STATE AXIOMS



nextRoad(Vehicle, NextRoad, S) :-
	currentRoute(Vehicle, Route, S),
	(length(Route) =:= 0 -> 
		NextRoad = none
		;
		Route = [NextRoad | _]
	).

	
hasTargetPLCS(Vehicle, S) :-
	currentTargetPLCS(Vehicle, PLCS, S),
	not PLCS=none.

currentRoad(Vehicle, Road, S) :-
	vehiclePosition(Vehicle, Pos, S),
	(Pos = r(Road), !
		;
		Road = none
	).
	
currentTarget(Vehicle, Target, S) :-
	currentRoad(Vehicle, Road, S),
	(Road = none ->
		Target = none
		;
		roadEnds(Road, Ends),
		Ends = r(_, Target)
	).

effect(vehiclePosition(Vehicle), enterNextRoad(Vehicle), OldPos, Position, S) :-
	nextRoad(Vehicle, NextRoad, S),
	(NextRoad = none ->
		Position = OldPos
		;
		Position = r(NextRoad)
	).
	
effect(vehiclePosition(Vehicle), arriveAtRoadEnd(Vehicle), OldPos, Position, S) :-
	OldPos = r(Road),
	roadEnds(Road, Ends),
	Ends = r(_, Target),
	Position = l(Target).
	

effect(currentRoute(Vehicle), setRoute(Vehicle, NewRoute), _, NewRoute, _).
effect(currentRoute(Vehicle), enterNextRoad(Vehicle), OldRoute, NewRoute, S) :-
	(length(OldRoute, 0) ->
		NewRoute = []
		;
		OldRoute = [_ | NewRoute]
	).

effect(currentTargetPLCS(Vehicle), setTargetPLCS(Vehicle, NewTarget), _, NewTarget, _).


effect(currentPLCS(Vehicle), driverLeavesPLCS(Vehicle), _, none, _).
effect(currentPLCS(Vehicle), driverParksAtPLCS(Vehicle, PLCS), _, PLCS, _).

effect(currentTargetPOI(Vehicle), setPOI(Vehicle, NewPOI), _, NewPOI, _).


	

		
		

	
		