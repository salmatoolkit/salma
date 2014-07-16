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
	setDomain(message, []),
	init_sort_hierarchy(_),
	
	domain(plcs, PLs),
	(foreach(P, PLs) do 
		setConstant(maxCapacity, [P, 10]),
		set_current(freeSlotsL, [P], none)
	),	
	domain(channel, Channels),
	(foreach(C, Channels) do
		set_current(channel_in_queue, [C], [])
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
	domain(message, []),
	start_sensing(plcs2, freeSlotsL, Msg),
	domain(message, [Msg]),
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
	domain(message, []).
	
message_exists(Msg) :-
	domain(message, M),
	member(Msg, M).
	
% channel: Params = [SrcRole, Dest, DestRole],
test_channel_transfer(Channel, Src, SrcRole, Dest, DestRole) :-
	run_eval,
	current_time(T1),
	create_message(Channel, Src, [SrcRole, Dest, DestRole], Msg),
	set_current(channel_out_content, [Msg], 42),
	progress_sequential([requestTransfer(Msg)], []),
	run_eval,
	messageSent(Src, Channel, SrcRole, Dest, DestRole, 42, s0),
	messageSent(Src, Channel, SrcRole, _, _, _, s0),
	not messageSent(Src, Channel, SrcRole, _, _, 43, s0),
	timestamp_S(Msg, T1, s0),
	timestamp_T(Msg, -1, s0),
	channel_transmission_content(Msg, none, s0),
	progress_sequential([tick, transferStarts(Msg, 1)], []),
	T2 is T1 + 1,
	timestamp_S(Msg, T1, s0),
	timestamp_T(Msg, T2, s0),
	channel_transmission_content(Msg, 43, s0),
	message_exists(Msg),	
	progress_sequential([tick, transferEnds(Msg, -2)], []),
	T3 is T1 + 2,
	not message_exists(Msg),
	% message format: src:agent, srcrole:term, dest:agent, destrole:term, timestamp:integer, content:term, 
	MsgTerm = msg(Src, SrcRole, Dest, DestRole, T3, 41),
	channel_in_queue(Channel, GlobalQueue, s0),
	
	member(MsgTerm, GlobalQueue),
	local_channel_in_queue(Dest, Channel, DestRole, LocalQueue, s0),
	member(MsgTerm, LocalQueue).
	
run_eval :-
	evaluation_step(ToplevelResults, ScheduledResults, PendingGoals, FailureStack),
	printf("ToplevelResults : %w \n ScheduledResults: %w \n PendingGoals: %w \n FailureStack: %w\n",
		[ToplevelResults, ScheduledResults, PendingGoals, FailureStack]).
		
test_channel :-
	init,
	%messageSent(v, assignment, veh, ?, ?, ?),
	F = forall([v,vehicle],
            implies(
                messageSent(v, assignment, veh, ?, ?, ?),
                until(10,
                    true,
                    currentPLCS(v) \= none
                )
            )
        ),
	
	register_property(f, F, _),
	evaluate_ad_hoc(F, Res, s0),
	printf("Result: %w\n", [Res]),
	
	channel_in_queue(assignment, [], s0),
	test_channel_transfer(assignment, vehicle1, veh, sam1, sam),
	test_channel_transfer(assignment, vehicle2, veh, sam1, sam),
	test_channel_transfer(assignment, sam1, sam, vehicle2, veh),
	test_channel_transfer(assignment, sam1, sam, vehicle1, veh),
	channel_in_queue(assignment, Q, s0),
	channel_in_queue(reservation, [], s0),
	printf("channel assignment: %w\n",[Q]),
	local_channel_in_queue(sam1, assignment, sam, Q2, s0),	
	evaluate_function(local_channel_in_queue, [sam1, assignment, sam, Q2, s0]),
	printf("local channel of SAM: %w\n", [Q2]),
	local_channel_in_queue(vehicle1, assignment, veh, Q3, s0),	
	printf("local channel of Vehicle 1: %w\n", [Q3]),
	local_channel_in_queue(vehicle2, assignment, veh, Q4, s0),	
	printf("local channel of Vehicle 2: %w\n", [Q4]),
	length(Q, 4),
	length(Q2, 2),
	length(Q3, 1),
	length(Q4, 1),
	progress_sequential([clean_queue(vehicle1, assignment, veh)],[]),
	
	channel_in_queue(assignment, Q5, s0),
	printf("After clean vehicle1: %w\n", [Q5]),
	length(Q5, 3),
	local_channel_in_queue(vehicle1, assignment, veh, [], s0),
	local_channel_in_queue(sam1, assignment, sam, Q2, s0),	
	local_channel_in_queue(vehicle2, assignment, veh, Q4, s0).