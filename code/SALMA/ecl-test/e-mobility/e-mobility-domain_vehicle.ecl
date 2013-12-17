
% VEHICLE
const(calendar, [veh:vehicle], list).
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
		pos_on_road is in the interval [0, roadlength(p1,p1)]"
	]).
	

fluent(vehicleSpeed, [veh:vehicle], integer).
doc(vehicleSpeed : fluent, [
	summary: "The vehicle's current speed.",
	desc: "This should be a stochastic fluent with a distribution
		that could depend on type of the current road but optionally also on the
		number of vehicles on the road."
	]).
		
fluent(currentPLCS, [veh:vehicle], plcs).

fluent(currentTargetPOI, [veh:vehicle], poi).
fluent(currentTargetPLCS, [veh:vehicle], plcs).

% route is given as the remaining list of locations
fluent(currentRoute, [veh:vehicle], list).
derived_fluent(currentTarget, [veh:vehicle], location).
derived_fluent(nextTarget, [veh:vehicle], location).


fluent(vehicle_plcs_reservationRequests, [veh:vehicle], list).

primitive_action(setTargetPLCS, [veh:vehicle, target:plcs]).
primitive_action(setRoute, [veh:vehicle, route:list]).

primitive_action(setPOI, [veh:vehicle, p:poi]).

	
	
exogenous_action(speedChanges, [veh:vehicle], [newSpeed:integer]).
exogenous_action(driverLeavesPLCS, [veh:vehicle], []).

% SUCCESSOR STATE AXIOMS


currentTarget(Vehicle, Target, S) :-
	currentRoute(Vehicle, Route, S),
	(length(Route) $= 0 -> 
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
	
	
get_next_target_start(Vehicle, ActPos, NextTargetStart, S) :-
	nextTarget(Vehicle, Target, S),
	(Target = none -> 
			NextTargetStart = pos(ActPos, ActPos, 0)
			;
			NextTargetStart = pos(ActPos, Target, 0)
	).
	

calculate_new_position(Vehicle, OldPos, Position, S) :-
	OldPos = pos(P1, P2, PosOnRoad),
	(P1 = P2 ->
		% we are at the end / beginning of a road
		get_next_target_start(Vehicle, P1, Position, S)		
		;
		% somewhere on the road
		roadlength(P1, P2, RoadLength),
		(PosOnRoad >= RoadLength ->
			% just arrived at target -> start next route segment
			get_next_target_start(Vehicle, P2, Position, S)
			;
			vehicleSpeed(Vehicle, Speed, S),
			NewPosOnRoad is PosOnRoad + Speed,
			Position = pos(P1, P2, NewPosOnRoad)
		)	
	).
	
vehiclePosition(Vehicle, Position, do2(A, S)) :-
	vehiclePosition(Vehicle, OldPos, S),
	(A = tick ->
		calculate_new_position(Vehicle, OldPos, Position, S)
		;
		Position = OldPos
	).
	

arrive_at_targetPLCS(Vehicle, S) :-
	vehiclePosition(Vehicle, Position, S),
	currentTargetPLCS(Vehicle, TargetPLCS, S),
	Position = pos(TargetPLCS, TargetPLCS, 0).
	
	
	
vehicleSpeed(Vehicle, Speed, do2(A, S)) :-
	A = speedChanges(Vehicle, NewSpeed),
	Speed is NewSpeed, !
	;
	arrive_at_targetPLCS(Vehicle, do2(A, S)),
	Speed is 0, !
	;
	vehicleSpeed(Vehicle, Speed, S).

currentRoute(Vehicle, Route, do2(A, S)) :-
	A = setRoute(Vehicle, NewRoute),
	Route = NewRoute, !
	;
	A = tick,
	currentRoute(Vehicle, OldRoute, S),
	vehiclePosition(Vehicle, OldPos, S),
	calculate_new_position(Vehicle, OldPos, NewPos, S),
	nextTarget(Vehicle, NextTarget, S),
	NewPos = pos(_, P2, _),
	% remove current target if next target was selected
	((P2 = NextTarget, NextTarget \= none) ->
		OldRoute = [_ | [Route | _]]
		;
		Route = OldRoute
	), !
	;
	currentRoute(Vehicle, Route, S).

currentTargetPLCS(Vehicle, Target, do2(A,S)) :-
	(A = setTargetPLCS(Vehicle, NewTarget) ->
		Target = NewTarget
	;
		currentTargetPLCS(Vehicle, Target, S)
	).

currentPLCS(Vehicle, PLCS, do2(A,S)) :-
	arrive_at_targetPLCS(Vehicle, do2(A,S)),
	currentTargetPLCS(Vehicle, PLCS, S), !
	;
	A = driverLeavesPLCS(Vehicle),
	PLCS = none, !.
	
currentTargetPOI(Vehicle, POI, do2(A,S)) :-
	A = setPOI(Vehicle, NewPOI) ->
		POI = NewPOI
	;
		currentTargetPOI(Vehicle, POI, S).

	
	
	
		
		
	
	
		