:- dynamic vehiclePosition/3,
	vehicleSpeed/3, currentRoute/3, currentTargetPOI/3, currentTarget/3,
	nextTarget/3, currentPLCS/3, currentTargetPLCS/3,
	calendar/2, waitingForAssignment/2, waitingForReservation/2.
	
% VEHICLE
constant(calendar, [veh:vehicle], list).
doc(calendar : constant,[
	summary: "Stores the vehicle's schedule as list of POIs together with the time
				interval this POI should be visited.",
	desc: "list entry format:
			cal(poi, startTime, plannedDuration)"
	]).

fluent(vehiclePosition, [veh:vehicle], term).
doc(vehiclePosition : fluent,[
	summary: "The vehicles's position.",
	desc: "The vehicle position is stored as aterm pos(p1, p2, pos_on_road) where
		pos_on_road is in the interval [0, roadlength(p1,p2)]"
	]).
	

fluent(vehicleSpeed, [veh:vehicle], integer).
doc(vehicleSpeed : fluent, [
	summary: "The vehicle's current speed.",
	desc: "This should be a stochastic fluent with a distribution
		that could depend on type of the current road but optionally also on the
		number of vehicles on the road."
	]).

% none when driving	
fluent(currentPLCS, [veh:vehicle], plcs).

fluent(currentTargetPOI, [veh:vehicle], poi).

fluent(currentTargetPLCS, [veh:vehicle], plcs).

fluent(waitingForAssignment, [veh:vehicle], boolean).
fluent(waitingForReservation, [veh:vehicle], boolean).

% route is given as the remaining list of locations
fluent(currentRoute, [veh:vehicle], list).

derived_fluent(currentTarget, [veh:vehicle], location).
derived_fluent(nextTarget, [veh:vehicle], location).
derived_fluent(hasTargetPLCS, [veh:vehicle], boolean).
derived_fluent(arrive_at_targetPLCS, [veh:vehicle], boolean).

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
schedulable(driverParksAtPLCS(vehicle, PLCS
	


exogenous_action(arriveAtRoadEnd, [veh:vehicle], []).
schedulable(arriveAtRoadEnd(Vehicle), S) :-
	vehiclePosition(Vehicle, Pos, S),
	Pos = pos(A, B),
	A \= B.
		
exogenous_action(enterNextRoad, [veh:vehicle], []).
schedulable(enterNextRoad(Vehicle), S) :-
	vehiclePosition(Vehicle, Pos, S),
	Pos = pos(A, A),
	currentTarget(Vehicle, Target, S),
	Target \= none.
	
% SUCCESSOR STATE AXIOMS

currentTarget(Vehicle, Target, S) :-
	currentRoute(Vehicle, Route, S),
	(length(Route) =:= 0 -> 
		Target = none
		;
		Route = [Target | _]
	).

nextTarget(Vehicle, Target, S) :-
	currentRoute(Vehicle, Route, S),
	(length(Route) < 2 -> 
		Target = none
		;
		Route = [_ | [Target | _]]
	).
	
hasTargetPLCS(Vehicle, S) :-
	currentTargetPLCS(Vehicle, PLCS, S),
	not PLCS=none.
	
	

effect(vehiclePosition(Vehicle), enterNextRoad(Vehicle), OldPos, Position, S) :-
	OldPos = pos(_, OldEnd),
	currentTarget(Vehicle, Target, S),
	(Target = none ->
		Position = OldPos
		;
		Position = pos(OldEnd, Target)
	).
	
effect(vehiclePosition(Vehicle), arriveAtRoadEnd(Vehicle), OldPos, Position, S) :-
	OldPos = pos(_, OldEnd),
	Position = pos(OldEnd, OldEnd).
	

effect(currentRoute(Vehicle), setRoute(Vehicle, NewRoute), _, NewRoute, _).
effect(currentRoute(Vehicle), arriveAtRoadEnd(Vehicle), OldRoute, NewRoute, S) :-
	(length(OldRoute, 0) ->
		NewRoute = []
		;
		OldRoute = [_ | NewRoute]
	).

effect(currentTargetPLCS(Vehicle), setTargetPLCS(Vehicle, NewTarget), _, NewTarget, _).

effect(currentPLCS(Vehicle), tick(Steps), _, PLCS, S) :-
	arrive_at_targetPLCS(Vehicle, do2(tick(Steps),S)),
	currentTargetPLCS(Vehicle, PLCS, S).

effect(currentPLCS(Vehicle), driverLeavesPLCS(Vehicle), _, none, _).
	
effect(currentTargetPOI(Vehicle), setPOI(Vehicle, NewPOI), _, NewPOI, _).


	

		
		

	
		