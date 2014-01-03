:- dynamic vehicle_plcssam_reservationRequests/4,
	vehicle_plcssam_reservationResponses/4,
	plcssam_vehicle_reservationRequests/3,
	plcssam_vehicle_reservationResponses/3,
	ongoing_exchange_PLCSSAM_Vehicle/3.
	
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
poss(queryPLCSSAM(_, _, _, _, _), _) :- true.
	
	
	
	
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
poss(set_plcssam_vehicle_reservationResponse(_,_,_,_,_), _) :- true.
	
primitive_action(remove_all_plcssam_vehicle_reservationRequests, [sam:plcssam]).
poss(remove_all_plcssam_vehicle_reservationRequests(_),_) :- true.


primitive_action(remove_all_vehicle_plcssam_reservationResponses,
	[veh:vehicle, sam:plcssam]).
poss(remove_all_vehicle_plcssam_reservationResponses(_, _), _) :- true.

primitive_action(start_exchange_PLCSSAM_Vehicle, [veh:vehicle, sam:plcssam]).
poss(start_exchange_PLCSSAM_Vehicle(_,_), _) :- true.

fluent(ongoing_exchange_PLCSSAM_Vehicle, [veh:vehicle, sam:plcssam], boolean).

exogenous_action(exchange_PLCSSAM_Vehicle, [veh:vehicle, sam:plcssam],[]).
poss(exchange_PLCSSAM_Vehicle(Vehicle, SAM), S) :-
	ongoing_exchange_PLCSSAM_Vehicle(Vehicle, SAM, S).

exogenous_action(fail_exchange_PLCSSAM_Vehicle, [veh:vehicle, sam:plcssam],[]).
poss(fail_exchange_PLCSSAM_Vehicle(Vehicle, SAM), S) :-
	ongoing_exchange_PLCSSAM_Vehicle(Vehicle, SAM, S).
	
	
vehicle_plcssam_reservationRequests(Vehicle, SAM, Requests, do2(A,S)) :-
	vehicle_plcssam_reservationRequests(Vehicle, SAM, OldRequests, S),
	(
		A = queryPLCSSAM(Vehicle, SAM, Alternatives, StartTime, PlannedDuration),
		Request = rreq(Alternatives, StartTime, PlannedDuration),
		append(OldRequests, [Request], Requests), !
		;
		% When data is exchanged, the outward channel is emptied
		(A = exchange_PLCSSAM_Vehicle(Vehicle, SAM) ;
		A = fail_exchange_PLCSSAM_Vehicle(Vehicle, SAM)),
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
		(A = exchange_PLCSSAM_Vehicle(Vehicle, SAM), 
		A = fail_exchange_PLCSSAM_Vehicle(Vehicle, SAM)),
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
			
vehicle_plcssam_reservationResponses(Vehicle, SAM, Responses, do2(A,S)) :-
	A = remove_all_vehicle_plcssam_reservationResponses(Vehicle, SAM),
	Responses = [], !
	;
	vehicle_plcssam_reservationResponses(Vehicle, SAM, OldResponses, S),
	(
		A = exchange_PLCSSAM_Vehicle(Vehicle, SAM),
		plcssam_vehicle_reservationResponses(SAM, SamResponses, S),
		(foreach(SResp, SamResponses), fromto([], R1, R2, NewResponses),
			param(Vehicle) do
			(
				SResp = rresp(Vehicle, StartTime, PlannedDuration, BestPLCS) ->
					NewResp = rresp(StartTime, PlannedDuration, BestPLCS),
					append(R1, [NewResp], R2)
					;
					% don't add response entry if Vehicle doesn't match
					R2 = R1
			)
		), 
		append(OldResponses, NewResponses, Responses), !
		;
		Responses = OldResponses, !
	).

ongoing_exchange_PLCSSAM_Vehicle(Vehicle, SAM, do2(A,S)) :-
	A = start_exchange_PLCSSAM_Vehicle(Vehicle, SAM), !
	;
	A \= exchange_PLCSSAM_Vehicle(Vehicle, SAM),
	A \= fail_exchange_PLCSSAM_Vehicle(Vehicle, SAM),
	ongoing_exchange_PLCSSAM_Vehicle(Vehicle, SAM, S), !.