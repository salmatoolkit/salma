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

% format of request: rreq(vehicle, startTime, plannedDuration)
fluent(plcsReservationRequests, [p:plcs], list).
% format of request: rresp(vehicle, startTime, plannedDuration)
fluent(plcsReservationResponses, [p:plcs], list).
derived_fluent(currentOccupacy, [p:plcs], integer).

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
% format: rreq(plcs, startTime, plannedDuration)
fluent(vehicleReservationRequests, [veh:vehicle], list).
% format: rresp(plcs, startTime, plannedDuration)
fluent(vehicleReservationResponses, [veh:vehicle], list).

% route is given as the remaining list of locations
fluent(currentRoute, [veh:vehicle], list).

fluent(plcsAlternatives, [veh:vehicle], list).

primitive_action(setRoute, [veh:vehicle, route:list]).

primitive_action(createReservationRequest, 
	[veh:vehicle, p:plcs, startTime:integer, plannedDuration], 
		






