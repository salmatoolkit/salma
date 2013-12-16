% notes: 
% - assume that there is only one PLCSSAM for now
% - neglect the traffic situation and exclude RouteSAM for now.
%	Best route is simply calculated by shortest path.
% - PLCSSAM chooses best plcs and creates a reservation
%	PLCS may not reject a request.
% - should introduce "directly set fluents" that are
%   set with setFluentValue
% 	- examples could be state flags, plans, etc.

sorts([vehicle, plcs, poi, crossing, location]).
subsorts([plcs, poi, crossing], location).

constant(locX, [loc:location], integer).
constant(locY, [loc:location], integer).

% ROADS
constant(connected, [l1:location, l2:location], boolean).
constant(roaddistance, [l1:location, l2:location], integer).

% PLCS
constant(maxCapacty, [p:plcs], integer).

% format: res(vehicle, startTime, plannedDuration)
fluent(plcsReservations, [p:plcs], list).
% format of request: rresp(vehicle, startTime, plannedDuration)
derived_fluent(currentOccupacy, [p:plcs], integer).

% PLCSSAM




% VEHICLE
% format: cal(poi, startTime, plannedDuration)
constant(calendar, [veh:vehicle], list).
% vehicle position is stored as pos(p1, p2, pos_on_road) where
% pos on road is in the interval [0, roaddistance(p1,p1)]
fluent(vehiclePosition, [veh:vehicle], term).
% this should be a stochastic fluent with a normal distribution
% with mean depending on the street
fluent(vehicleSpeed, [veh:vehicle], integer).
fluent(currentPLCS, [veh:vehicle], plcs).
fluent(currentPOI, [veh:vehicle], poi).
% route is given as the remaining list of locations
fluent(currentRoute, [veh:vehicle], list).
% list of res(plcs, startTime, reservedDuration)
fluent(plcsReservationResponses, [p:plcs], list).

% -------------------------------------
% communication fluents
% -------------------------------------

% communication flow:
% 1. PLCSSAM reads all current vehicleReservationRequests
% 2. PLCSSAM selects best PLCS for each vehicle
% 3. PLCSSAM writes to PLCS::reservations
% 4. PLCSSAM sets Vehice::reservation

% the following two fluents store information for communication between PLCSSAM and vehicle
% 
% format: rreq([plcs1, plcs2,... ], startTime, plannedDuration)
fluent(vehicleReservationRequests, [veh:vehicle], list).
% format: rresp(plcs, startTime, plannedDuration)
fluent(vehicleReservationResponse, [veh:vehicle], plcs).



primitive_action(setRoute, [veh:vehicle, route:list]).
primitive_action(setPOI, [veh:vehicle, p:poi]).

primitive_action(createReservationRequest, 
	[veh:vehicle, alternatives:list, startTime:integer, plannedDuration]).

primitive_action(exchange_PLCSSAM_Vehicle, [veh:vehicle]).



		






