% VEHICLE --> PLCSSAM

% list entry format: rreq([plcs1, plcs2,... ], startTime, plannedDuration)
fluent(vehicle_plcssam_reservationRequests, [veh:vehicle], list).

% format: rresp(plcs, startTime, plannedDuration)
fluent(vehicle_plcssam_reservationResponses, [veh:vehicle], list).

% PLCSSAM --> VEHICLE

% reservation requests from vehicles
% list of req(vehicle, [plcsAlternatives], startTime, reservedDuration)
fluent(plcssam_vehicle_reservationRequests, [sam:plcssam], list).

% list of sugres(vehicle, plcs, startTime, reservedDuration)
fluent(plcssam_vehicle_reservationResponses, [sam:plcssam], list).

exogenous_action(exchange_PLCSSAM_Vehicle, [veh:vehicle, sam:plcssam],[]).
