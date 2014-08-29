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
	progress([tick]),
	progress([transferStarts(Msg, 2)]),
	print("\nAfter transferStarts:\n--------------\n"),
	print_all_messages,
	assertEquals(length(domain(message)), 1, check_2),
	print_channel(rob2rob),
	channel_in_queue(con2rob, [], s0),
	channel_in_queue(rob2rob, [], s0),
	progress([tick]),
	progress([transferEnds(Msg, 2)]),
	printf("\nAfter transferEnds(%d):\n--------------\n",[Msg]),
	print_all_messages,
	print_channel(rob2rob),
	progress([tick]),
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
		
test_remote_sensor :-
	init,
	create_message(batteryLevelR, rob2, multicastSrc, 
		[batteryLevelR], Msg),
	set_current(channel_out_content, [Msg], 42),
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
	).
		
	
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
	test_remote_sensor.
	
	
	
	
	
	
	
