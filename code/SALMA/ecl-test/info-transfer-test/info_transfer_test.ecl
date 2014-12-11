:- ['../../ecl-src/agasmc'].
:- ['../../ecl-src/test_utils'].
:- [domaindesc_info_transfer_test].

setPosition(Agent, X, Y) :-
	set_current(xpos, [Agent], X),
	set_current(ypos, [Agent], Y).

init :- 
	init_agasmc,
	setDomain(robot, [rob1, rob2, rob3]),
	setDomain(controller, [con1, con2]),
	init_sort_hierarchy(_),
	setPosition(rob1, 20, 20),
	setPosition(rob2, 55, 30),
	setPosition(rob3, 60, 50),
	setPosition(con1, 30, 30),
	setPosition(con2, 50, 40),
	
	domain(robot, D1),
	(foreach(R, D1) do	
		set_current(batteryLevel, [R], 100)
	),	
	setDomain(channel, [rob2rob, con2rob]),
	setDomain(sensor, [batteryLevelL]),
	setDomain(remoteSensor, [batteryLevelR]),	
	setDomain(message, []),
	init_sort_hierarchy(_),

	domain(channel, Channels),
	(foreach(C, Channels) do
		set_current(channel_in_queue, [C], [])
	),	
	set_current(time, [], 0).

test_ensembles :-
	init,
	get_ensemble_participant_types(batteryLevelR, controller, robot),
	get_ensemble_participant_types(con2rob, controller, robot),
	get_ensemble_members(batteryLevelR, all:all, Members1, s0),
	Members1 =  [con1 : rob1, con1 : rob2, con2 : rob2, con2 : rob3],
	get_ensemble_members(batteryLevelR, con1:all, Members2, s0),
	Members2 =  [con1 : rob1, con1 : rob2],
	get_ensemble_members(batteryLevelR, con2:all, Members3, s0),
	Members3 =  [con2 : rob2, con2 : rob3],
	get_ensemble_members(batteryLevelR, all:rob1, Members4, s0),
	Members4 =  [con1 : rob1],
	get_ensemble_members(batteryLevelR, all:rob2, Members5, s0),
	Members5 =  [con1 : rob2, con2 : rob2].
	
	
print_message(Msg) :-
	message_spec(Msg, Spec),
	(awaitingTransfer(Msg, s0) -> Stat1 = "awaitingTransfer" ; Stat1 = "!awaitingTransfer"),
	(transferring(Msg, s0) -> Stat2 = "transferring" ; Stat2 = "!transferring"),
	
	channel_out_content(Msg, COutContent, s0),
	channel_transmission_content(Msg, CTransContent, s0),	
	printf("%d : %w - %s, %s - out: %w, trans: %w\n", [Msg, Spec, Stat1, Stat2, 
		COutContent, CTransContent]).
	
print_all_messages :-
	domain(message, Dom),
	(foreach(M, Dom) do
		print_message(M)
	).
	
print_channel(Channel) :-
	channel_in_queue(Channel, L, s0),
	printf("Channel %w:\n",[Channel]),
	(foreach(M, L) do
		printf("   %w\n",[M])
	).
	
test_unicast_channel :-
	init,
	create_message(rob2rob, rob1, unicast, [r1, rob2, r2], Msg),
	set_current(channel_out_content, [Msg], 42),
	print("Before:\n--------------\n"),
	print_all_messages,
	channel_in_queue(con2rob, [], s0),
	channel_in_queue(rob2rob, [], s0),
	assertEquals(length(domain(message)), 1, check_1),
	progress([requestTransfer(Msg)]),
	messageSent(rob1, rob2rob, _, _, _, _, s0),
	evaluate_ad_hoc(messageSent(rob1, rob2rob, ?, ?, ?, ?), R),
	printf("Result: %w\n",[R]),
	print("\nAfter requestTransfer:\n--------------\n"),
	print_all_messages,
	progress([tick(1)]),
	progress([transferStarts(Msg, 2)]),
	print("\nAfter transferStarts:\n--------------\n"),
	print_all_messages,
	assertEquals(length(domain(message)), 1, check_2),
	print_channel(rob2rob),
	channel_in_queue(con2rob, [], s0),
	channel_in_queue(rob2rob, [], s0),
	progress([tick(1)]),
	progress([transferEnds(Msg, 2)]),
	printf("\nAfter transferEnds(%d):\n--------------\n",[Msg]),
	print_all_messages,
	print_channel(rob2rob),
	progress([tick(1)]),
	assertEquals(length(domain(message)), 0, check_3),
	channel_in_queue(rob2rob, [msg(rob1, r1, rob2, r2, 2, 46)], s0),
	channel_in_queue(con2rob, [], s0).
	
test_multicast_channel :-
	init,
	create_message(con2rob, con2, multicastSrc, [con], Msg),
	set_current(channel_out_content, [Msg], 42),
	print("Before:\n--------------\n"),
	print_all_messages,
	channel_in_queue(con2rob, [], s0),
	channel_in_queue(rob2rob, [], s0),
	assertEquals(length(domain(message)), 1, check_1),
	progress([requestTransfer(Msg)]),
	print("\nAfter requestTransfer:\n--------------\n"),
	print_all_messages,
	progress([tick]),
	progress([transferStarts(Msg, 2)]),
	print("\nAfter transferStarts:\n--------------\n"),
	print_all_messages,
	assertEquals(length(domain(message)), 3, check_2),
	print_channel(con2rob),
	channel_in_queue(con2rob, [], s0),
	channel_in_queue(rob2rob, [], s0),
	progress([tick, tick]),
	domain(message, Messages1),
	get_dest_messages(Msg, Messages1, DestMessages1),
	(foreach(DMsg, DestMessages1) do
		progress([transferEnds(DMsg, 2)]),
		printf("\nAfter transferEnds(%d):\n--------------\n",[DMsg]),
		print_all_messages,
		print_channel(con2rob),
		progress([tick])
	),
	assertEquals(length(domain(message)), 0, check_3),
	channel_in_queue(con2rob, [msg(con2, con, rob2, r, 3, 46), msg(con2, con, rob3, r, 4, 46)], s0),
	channel_in_queue(rob2rob, [], s0).
	
test_local_sensor :-
	init,
	is_undefined(batteryLevelL, [rob1]),
	create_message(batteryLevelL, rob1, sensor, [], Msg),
	print("Before:\n--------------\n"),
	print_all_messages,
	progress([requestTransfer(Msg)]),
	print("\nAfter requestTransfer:\n--------------\n"),
	print_all_messages,
	progress([tick]),
	progress([transferStarts(Msg, 2)]),
	print("\nAfter transferStarts:\n--------------\n"),
	print_all_messages,
	progress([tick]),
	progress([transferEnds(Msg, 2)]),
	printf("\nAfter transferEnds(%d):\n--------------\n",[Msg]),
	print_all_messages,
	batteryLevelL(rob1, 104, s0),
	progress([tick]),
	%---
	set_current(batteryLevel, [rob1], 50),
	create_message(batteryLevelL, rob1, sensor, [], Msg2),
	progress([requestTransfer(Msg2)]),
	progress([tick]),
	progress([transferStarts(Msg2, 3)]),
	progress([tick]),
	batteryLevelL(rob1, 104, s0),
	progress([transferEnds(Msg2, 3)]),
	batteryLevelL(rob1, 56, s0).
	
test_remote_sensor :-
	init,
	% sense locally
	create_message(batteryLevelL, rob2, sensor, [], MsgLocal),
	progress([requestTransfer(MsgLocal)]),
	progress([tick]),	
	progress([transferStarts(MsgLocal, 0)]),
	progress([tick]),	
	progress([transferEnds(MsgLocal, 0)]),
	progress([tick]),	
	batteryLevelL(rob2, 100, s0),
	% send remote sensor src message
	create_message(batteryLevelR, rob2, remoteSensorSrc, 
		[], Msg),
	%set_current(channel_out_content, [Msg], 42),
	print("Before:\n--------------\n"),
	print_all_messages,
	progress([requestTransfer(Msg)]),
	print("\nAfter requestTransfer:\n--------------\n"),
	print_all_messages,
	progress([tick]),
	progress([transferStarts(Msg, 2)]),
	print("\nAfter transferStarts:\n--------------\n"),
	print_all_messages,
	print_channel(batteryLevelR),
	progress([tick, tick]),
	domain(message, Messages1),
	get_dest_messages(Msg, Messages1, DestMessages1),
	(foreach(DMsg, DestMessages1) do
		progress([transferEnds(DMsg, none)]),
		printf("\nAfter transferEnds(%d):\n--------------\n",[DMsg]),
		print_all_messages,
		print_channel(batteryLevelR),
		progress([tick])
	),
	progress([update_remote_sensor(con1, batteryLevelR)]),
	progress([update_remote_sensor(con2, batteryLevelR)]),
	batteryLevelR(con1, rob2, 102, s0),
	batteryLevelR(con2, rob2, 102, s0).

test_event_scheduling :-
	init,
	create_message(rob2rob, rob1, unicast, [r1, rob2, r2], Msg),
	set_current(channel_out_content, [Msg], 42),
	print("Before:\n--------------\n"),
	print_all_messages,
	progress([requestTransfer(Msg)]),
	get_next_schedulable_events(0, 100, [], [], Time, Events),
	printf("Time: %d, Events: %w\n", [Time, Events]),
	Time =:= 0,
	Events =  [transferStarts : [[Msg]], transferFails : [[Msg]]],
	Schedule = [ev(15, transferStarts, [Msg])],
	get_next_schedulable_events(0, 100, Schedule, [], Time2, Events2),
	printf("Time2: %d, Events2: %w\n", [Time2, Events2]),
	Time2 =:= -1,
	Events2 = [],
	progress([tick(15)]),
	time(15, s0),
	progress([transferStarts(Msg)]),
	get_next_schedulable_events(15, 100, [], [], Time3, Events3),
	printf("Time3: %d, Events3: %w\n", [Time3, Events3]).
	
	
test_all :-
	print("********************************************\n"),
	print("test_ensembles\n"),
	print("********************************************\n"),
	test_ensembles,
	
	print("\n\n********************************************\n"),
	print("test_unicast_channel\n"),
	print("********************************************\n"),
	test_unicast_channel,
	
	print("\n\n********************************************\n"),
	print("test_multicast_channel\n"),
	print("********************************************\n"),
	test_multicast_channel,
	
	print("\n\n********************************************\n"),
	print("test_remote_sensor\n"),
	print("********************************************\n"),
	test_remote_sensor,
	
	print("\n\n********************************************\n"),
	print("test_local_sensor\n"),
	print("********************************************\n"),
	test_local_sensor.
	
	
	
	
	
	
	
