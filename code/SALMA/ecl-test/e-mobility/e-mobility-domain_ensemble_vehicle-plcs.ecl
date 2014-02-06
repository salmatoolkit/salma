:- dynamic vehicle_plcs_reservationRequests/4,
		plcs_vehicle_reservationRequests/3,
		plcs_vehicle_reservationResponses/4,
		vehicle_plcs_reservationResponses/3,
		ongoing_exchange_PLCS_Vehicle/3.
		

% format rreq(startTime, plannedDuration)
fluent(vehicle_plcs_reservationRequests, [veh:vehicle, p:plcs], list).

%format rreq(vehicle, startTime, plannedDuration)
fluent(plcs_vehicle_reservationRequests, [p:plcs], list).

% format rresp(plcs, startTime, plannedDuration, ack:boolean)
fluent(vehicle_plcs_reservationResponses, [veh:vehicle], list).

% format rresp(startTime, plannedDuration, ack:boolean)
fluent(plcs_vehicle_reservationResponses, [p:plcs, veh:vehicle], list).

% adds request to vehicle_plcs_reservationRequests
primitive_action(requestReservation, 
	[veh:vehicle, p:plcs, startTime:integer, plannedDuration:integer]).
poss(requestReservation(_,_,_,_),_) :- true.
immediate_action(requestReservation).

primitive_action(remove_plcs_vehicle_reservationRequests, [p:plcs]).
poss(remove_plcs_vehicle_reservationRequests(_),_) :- true.
immediate_action(remove_plcs_vehicle_reservationRequests).

primitive_action(set_plcs_vehicle_reservationResponse, 
	[p:plcs, veh:vehicle, startTime:integer, plannedDuration:integer, 
	ack:boolean]).
poss(set_plcs_vehicle_reservationResponse(_,_,_,_,_),_) :- true.
immediate_action(set_plcs_vehicle_reservationResponse).

primitive_action(remove_vehicle_plcs_reservationResponses, 
	[veh:vehicle]).
poss(remove_vehicle_plcs_reservationResponses(_),_) :- true.	
immediate_action(remove_vehicle_plcs_reservationResponses).
	
primitive_action(start_exchange_PLCS_Vehicle, [veh:vehicle, p:plcs]).
poss(start_exchange_PLCS_Vehicle(_,_),_) :- true.


fluent(ongoing_exchange_PLCS_Vehicle, [veh:vehicle, p:plcs], boolean).
exogenous_action(exchange_PLCS_Vehicle, [veh:vehicle, p:plcs],[]).
poss(exchange_PLCS_Vehicle(Vehicle, PLCS), S) :-
	ongoing_exchange_PLCS_Vehicle(Vehicle, PLCS, S).

exogenous_action(fail_exchange_PLCS_Vehicle, [veh:vehicle, p:plcs],[]).
poss(fail_exchange_PLCS_Vehicle(Vehicle, PLCS), S) :-
	ongoing_exchange_PLCS_Vehicle(Vehicle, PLCS, S).

vehicle_plcs_reservationRequests(Vehicle, PLCS, Requests, do2(A,S)) :-
	vehicle_plcs_reservationRequests(Vehicle, PLCS, OldRequests, S),
	(A = requestReservation(Vehicle, PLCS, StartTime, PlannedDuration),
		Req = rreq(StartTime, PlannedDuration),
		append(OldRequests, [Req], Requests), !
		;
	A = exchange_PLCS_Vehicle(Vehicle, PLCS),
		Requests = [], !
		;
		Requests = OldRequests
	).

plcs_vehicle_reservationRequests(PLCS, Requests, do2(A,S)) :-
	plcs_vehicle_reservationRequests(PLCS, OldRequests, S),
	(A = exchange_PLCS_Vehicle(Vehicle, PLCS),
		vehicle_plcs_reservationRequests(Vehicle, PLCS, VRequests, S),
		(foreach(VR, VRequests), foreach(R, Requests), param(Vehicle) do
			VR = rreq(StartTime, PlannedDuration),
			R = rreq(Vehicle, StartTime, PlannedDuration)
		), !
		;
	A = remove_plcs_vehicle_reservationRequests(PLCS),
	Requests = [], !
	;
	Requests = OldRequests
	).

plcs_vehicle_reservationResponses(PLCS, Vehicle, Responses, do2(A,S)) :-
	plcs_vehicle_reservationResponses(PLCS, Vehicle, OldResponses, S),
	(A = set_plcs_vehicle_reservationResponse(PLCS, Vehicle,
		StartTime, PlannedDuration, Ack),
		Resp = rresp(StartTime, PlannedDuration, Ack),
		append(OldResponses, [Resp], Responses), !
		;
	A = exchange_PLCS_Vehicle(Vehicle, PLCS), 
	Responses = [], !
	;
	Responses = OldResponses
	).

vehicle_plcs_reservationResponses(Vehicle, Responses, do2(A,S)) :-
	vehicle_plcs_reservationResponses(Vehicle, OldResponses, S),
	(A = exchange_PLCS_Vehicle(Vehicle, PLCS),
		plcs_vehicle_reservationResponses(PLCS, Vehicle, PLCSResponses, S),
		(foreach(PLCSResp, PLCSResponses), foreach(VResp, Responses),
			param(PLCS) do
				PLCSResp = rresp(StartTime, PlannedDuration, Ack),
				VResp = rresp(PLCS, StartTime, PlannedDuration, Ack)
		), !
		;
	A = remove_vehicle_plcs_reservationResponses(Vehicle),
	Responses = [], !
	;
	Responses = OldResponses
	).

ongoing_exchange_PLCS_Vehicle(Vehicle, PLCS, do2(A,S)) :-
	A = start_exchange_PLCS_Vehicle(Vehicle, PLCS), !
	;
	A \= exchange_PLCS_Vehicle(Vehicle, PLCS),
	A \= fail_exchange_PLCS_Vehicle(Vehicle, PLCS),
	ongoing_exchange_PLCS_Vehicle(Vehicle, PLCS, S), !.
