:- dynamic plcsReservations/3, vehiclePosition/3.

% notes: 
% - neglect the traffic situation and exclude RouteSAM for now.
%	Best route is simply calculated by shortest path.
% - PLCSSAM chooses best plcs and creates a reservation
%	PLCS may not reject a request.
% - should introduce "directly set fluents" that are
%   set with setFluentValue
% 	- examples could be state flags, plans, etc.

sorts([vehicle, plcs, plcssam, poi, crossing, location]).
subsorts([plcs, plcssam, poi, crossing], location).

constant(locX, [loc:location], integer).
constant(locY, [loc:location], integer).

% ROADS
constant(connected, [l1:location, l2:location], boolean).
constant(roadlength, [l1:location, l2:location], integer).

% PLCS
constant(maxCapacty, [p:plcs], integer).


constant(responsiblePLCSSAM, [loc:location], plcssam).
doc(responsiblePLCSSAM : constant, [
	summary: "The PLCSSAM that is respüonsible for the area that includes loc.",
	desc:	"Setup should be done in a way that makes sure that close plcs are always
			associated with the PLCSSAM in the area."
	]).


% PLCS


fluent(plcs_vehicle_reservationRequests, [p:plcs], list).

	
% format: res(vehicle, startTime, plannedDuration)
fluent(plcsReservations, [p:plcs], list).


derived_fluent(currentOccupacy, [p:plcs], integer).

% PLCSSAM


% reservation requests from vehicles
% list of req(vehicle, [plcsAlternatives], startTime, reservedDuration)
fluent(plcssam_vehicle_reservationRequests, [sam:plcssam], list).

% list of sugres(vehicle, plcs, startTime, reservedDuration)
fluent(plcssam_vehicle_reservationSuggestions, [sam:plcssam], list).



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
% route is given as the remaining list of locations
fluent(currentRoute, [veh:vehicle], list).


% -------------------------------------
% communication fluents
% -------------------------------------

% communication flow:
% 1. PLCSSAM reads all current vehicleReservationRequests
% 2. PLCSSAM selects best PLCS for each vehicle
% 3. PLCSSAM writes to PLCS::reservations
% 4. PLCSSAM sets Vehice::reservation

 
% The list of reservation requests that a vehicle intends to send to the PLCSSAM. Each request
% includes a list of alternatives for possible PLCSs. The PLCSSAM is supposed to select
% the optimal PLCS out of this list.

% list entry format: rreq([plcs1, plcs2,... ], startTime, plannedDuration)
fluent(vehicle_reservationRequests, [veh:vehicle], list).

% format: rresp(plcs, startTime, plannedDuration)
fluent(vehicle_reservationResponses, [veh:vehicle], list).





primitive_action(setRoute, [veh:vehicle, route:list]).
primitive_action(setPOI, [veh:vehicle, p:poi]).

primitive_action(createReservationRequest, 
	[veh:vehicle, alternatives:list, startTime:integer, plannedDuration]).

primitive_action(exchange_PLCSSAM_Vehicle, [veh:vehicle]).


get_next_target(Vehicle, Target, S) :-
	currentRoute(Vehicle, Route, S),
	(length(X) $= 0 -> 
		Target = none
		;
		Route = [Target | _]
	).
	
	
get_next_target_start(Vehicle, ActPos, NextTargetStart, S) :-
	get_next_target(Vehicle, Target, S),
	(Target = none -> 
			NextTargetStart = pos(ActPos, ActPos, 0)
			;
			Position = pos(ActPos, Target, 0)
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
	
	





