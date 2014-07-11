:- ['../../ecl-src/agasmc'].
:- ['e-mobility-domain-with-abstractions'].


init :-
	init_agasmc,
	setDomain(crossing, [c1, c2, c3, c4]),
	setDomain(vehicle, [vehicle1, vehicle2]),
	setDomain(plcs, [plcs1, plcs2]),
	setDomain(poi, [poi1, poi2]),
	setDomain(plcssam, [sam1]),
	setDomain(channel, [assignment, reservation]),
	setDomain(sensor, [freeSlotsL]),

	init_sort_hierarchy(_),
	
	domain(plcs, PLs),
	(foreach(P, PLs) do 
		setConstant(maxCapacity, [P, 10]),
		set_current(freeSlotsL, [P], none)
	),	
	setConstant(connected, [c1,c2, true]),
	setConstant(connected, [c2,c3, true]),
	setConstant(connected, [c3,c4, true]),
	setConstant(connected, [c4,plcs1, true]),
	setConstant(connected, [c4,plcs2, true]),
	setConstant(roadlength, [c1, c2, 100]),
	setConstant(roadlength, [c2, c3, 100]),
	setConstant(roadlength, [c3, c4, 100]),
	setConstant(roadlength, [c4, plcs1, 100]),
	setConstant(roadlength, [c4, plcs2, 100]),
	set_current(vehiclePosition, [vehicle1], pos(c1,c1,0)),
	set_current(currentTargetPOI, [vehicle1], poi1),
	set_current(currentTargetPLCS, [vehicle1], plcs1),
	set_current(currentPLCS, [vehicle1], none),
	set_current(currentRoute, [vehicle1], [c1,c2,c3,c4,plcs1]),
	set_current(vehicleSpeed, [vehicle1], 10),
	place_vehicle_at_plcs(vehicle2, plcs2),
	set_current(time, [], 0).
	

place_vehicle_at_plcs(Vehicle, PLCS) :-
	set_current(currentPLCS, [Vehicle], PLCS),
	set_current(currentTargetPLCS, [Vehicle], PLCS),
	set_current(vehiclePosition, [Vehicle], pos(PLCS, PLCS, 0)),
	set_current(currentRoute, [Vehicle], []),
	set_current(vehicleSpeed, [Vehicle], 0).

	
start_sensing(Agent, Sensor, Msg) :-
	create_message(Sensor, Agent, [], Msg),
	progress_sequential([requestTransfer(Msg)], FailedActions),
	printf("%w\n", [FailedActions]).

print_message_state(Msg) :-
	
	message_spec(Msg, Spec),
	printf("Message # %d: %w\n", [Msg, Spec]),
	(awaitingTransfer(Msg, s0) -> printf("   awaitingTransfer\n",[]) ; true),
	(transferring(Msg, s0) -> printf("   transferring\n",[]) ; true),
	timestamp_S(Msg, Ts, s0),
	timestamp_T(Msg, Tt, s0),
	sensor_transmitted_value(Msg, TValue, s0),
	
	printf("   T_s = %d - T_t = %d - TValue = %w\n", [Ts, Tt, TValue]).
	
test_sensor :-
	init,
	freeSlotsL(plcs1, none, s0),
	freeSlotsL(plcs2, none, s0),
	start_sensing(plcs2, freeSlotsL, Msg),
	awaitingTransfer(Msg, s0),
	not transferring(Msg, s0),
	timestamp_S(Msg, 0, s0),
	timestamp_T(Msg, -1, s0),
	sensor_transmitted_value(Msg, none, s0),
	progress_sequential([tick, transferStarts(Msg, 0)], []),
	not awaitingTransfer(Msg, s0),
	transferring(Msg, s0),
	timestamp_S(Msg, 0, s0),
	timestamp_T(Msg, 1, s0),
	freeSlotsL(plcs1, none, s0),
	freeSlotsL(plcs2, none, s0),
	sensor_transmitted_value(Msg, 9, s0),
	
	progress_sequential([tick, transferEnds(Msg, -1)], []),
	freeSlotsL(plcs1, none, s0),
	freeSlotsL(plcs2, 8, s0),
	tstamp_freeSlotsL(plcs2, 2, s0),	
	domain(message, Messages),
	not member(Msg, Messages).
