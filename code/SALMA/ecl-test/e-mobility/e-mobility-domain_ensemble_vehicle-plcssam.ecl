% VEHICLE --> PLCSSAM

% list entry format: rreq([plcs1, plcs2,... ], startTime, plannedDuration)
fluent(vehicle_plcssam_reservationRequests, [veh:vehicle, sam:plcssam], list).

% format: rresp(startTime, plannedDuration, plcs, )
fluent(vehicle_plcssam_reservationResponses, [veh:vehicle, sam:plcssam], list).

% PLCSSAM --> VEHICLE

% reservation requests from vehicles
% list of rreq(vehicle, [plcsAlternatives], startTime, reservedDuration)
fluent(plcssam_vehicle_reservationRequests, [sam:plcssam], list).


% list of rresp(vehicle, startTime, reservedDuration, plcs)
fluent(plcssam_vehicle_reservationResponses, [sam:plcssam], list).

primitive_action(queryPLCSSAM, 
	[veh:vehicle, sam:plcssam, alternatives:list, startTime:integer, 
	plannedDuration:integer]).
doc(queryPLCSSAM : primitive_action, [
	summary: "Used by the vehicle to issue a reservation request to the PLCSSAM.",
	desc: "The request is placed in the outward channel to the PLCSSAM
		that is represented by fluent vehicle_plcssam_reservationRequests.
		Each request contains a list of possible alternatives for the 
		PLCS. These alternatives must have been determined beforehand."
	]).

	
primitive_action(set_plcssam_vehicle_reservationResponse,
	[sam:plcssam, veh:vehicle, 
	startTime:integer, plannedDuration:integer,
	bestPLCS:plcs]). 
doc(set_plcssam_vehicle_reservationResponse : primitive_action, [
	summary: "Used by the PLCSSAM to respond to a vehicle's reservation request.",
	desc: "The response is added to the list at fluent 
	plcssam_vehicle_reservationResponses and contains the PLCS that
	the PLCS has chosen from the received set of alternatives."
	]).
	
primitive_action(remove_all_plcssam_vehicle_reservationRequests, [sam:plcssam]).

	
	
exogenous_action(exchange_PLCSSAM_Vehicle, [veh:vehicle, sam:plcssam],[]).

vehicle_plcssam_reservationRequests(Vehicle, SAM, Requests, do2(A,S)) :-
	vehicle_plcssam_reservationRequests(Vehicle, SAM, OldRequests, S),
	(
		A = queryPLCSSAM(Vehicle, SAM, Alternatives, StartTime, PlannedDuration),
		Request = rreq(Alternatives, StartTime, PlannedDuration),
		append(OldRequests, [Request], Requests), !
		;
		% When data is exchanged, the outward channel is emptied
		A = exchange_PLCSSAM_Vehicle(Vehicle, SAM),
		Requests = [], !
		;
		Requests = OldRequests, !
	).
		
plcssam_vehicle_reservationRequests(SAM, Requests, do2(A,S)) :-
	A = remove_all_plcssam_vehicle_reservationRequests(SAM),
	Requests = [], !
	;
	plcssam_vehicle_reservationRequests(SAM, OldRequests, S),
	(
		A = exchange_PLCSSAM_Vehicle(Vehicle, SAM),
		vehicle_plcssam_reservationRequests(Vehicle, SAM, VehicleRequests, S),
		(foreach(VReq, VehicleRequests), foreach(Req, NewRequests),
			param(Vehicle) do
			VReq = rreq(Alternatives, StartTime, PlannedDuration),
			Req = rreq(Vehicle, Alternatives, StartTime, PlannedDuration)
		),
		append(OldRequests, NewRequests, Requests), !
		;
		Requests = OldRequests
	).

	
plcssam_vehicle_reservationResponses(SAM, Responses, do2(A,S)) :-
	plcssam_vehicle_reservationResponses(SAM, OldResponses, S),
	(
		A = set_plcssam_vehicle_reservationResponse(
				SAM, Vehicle, StartTime, PlannedDuration, BestPLCS),
		Resp = rresp(Vehicle, StartTime, PlannedDuration, BestPLCS),
		append(OldResponses, [Resp], Responses), !
		;
		% during a data exchange, remove all responses for a vehicle from
		% the outward channel of the PLCSSAM
		A = exchange_PLCSSAM_Vehicle(Vehicle, SAM),
		(foreach(SamResponse, OldResponses), fromto([], R1, R2, Responses), 
			param(Vehicle) do
			(
				SamResponse = rresp(Vehicle, _, _, _) ->
					% filter out message to vehicle
					R2 = R1
					;
					append(R1, [SamResponse], R2)
			)
		), !
		;
		Responses = OldResponses
	).
			
	