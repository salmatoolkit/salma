:- lib(lists).
:- dynamic channel/4, sensor/3, remoteSensor/4, ensemble/4,
	awaitingTransfer/2, transferring/2, timestamp_S/3, timestamp_T/3,
	message_spec/2, 
	message_count_transmission/4, messageSent/7,
	channel_out_content/3, channel_transmission_content/3,
	channel_in_queue/3, local_channel_in_queue/5, message_available/4,
	sensor_transmitted_value/3,
	% function that adds error to original value
	% error_operator(con:Connector, OrigValue, Error, NewValue)
	error_operator/4.
	
:- local variable(nextMsg,1).

sort(message).
sort(connector).
sorts([channel, sensor, remoteSensor]).
subsorts([channel, sensor], connector).
subsort(remoteSensor, channel).

dynamic_sort(message).


% default error operator: add if both original value and error are numerical.
% Otherwise, error overwrites 
error_operator(_, OrigValue, Error, NewValue) :-
	number(OrigValue),
	number(Error),
	NewValue is OrigValue + Error, !
	;
	% for boolean values, an error of "true" flips the original value
	Error = true,
	(OrigValue = true, !, NewValue = false ; OrigValue = false, NewValue = true), !
	;
	NewValue = OrigValue.	



% STATE FLUENTS
fluent(awaitingTransfer, [m:message], boolean).
untracked_fluent(awaitingTransfer).

fluent(transferring, [m:message], boolean).
untracked_fluent(transferring).
	
	
% The constant message_spec stores the message's metadata.
% Format: msg(Con, MsgType, Agent, Params)
% convention: agent is sender for channels 
%    and receiver for (remote) sensors.
% Sensor: Params are directly used as params for the source fluent,
%			MsgType = sensor
% For channels and remote sensors, we distinguish 4 message types:
%    a) unicast: Params = [SrcRole, Dest, DestRole]
%    b) multicastSrc: Params = [SrcRole]
%	 c) remoteSensorSrc: Params = original params for local sensor
%    d) multicastDest: Params = [SrcMessageId, Dest, DestRole]

constant(message_spec, [m:message], term). 


fluent(timestamp_S, [m:message], integer).
untracked_fluent(timestamp_S).


fluent(timestamp_T, [m:message], integer).
untracked_fluent(timestamp_T).
	
derived_fluent(message_count_transmission, 
	[con:connector, a:agent], integer).
	

			   
% CHANNEL

% contains the message content before transferStarts
fluent(channel_out_content, [m:message], term).
untracked_fluent(channel_out_content).

% contains the message content after transferStarts
fluent(channel_transmission_content, [m:message], term).
untracked_fluent(channel_transmission_content).

% message format: src:agent, srcrole:term, destrole:term, content:term, timestamp:integer
fluent(channel_in_queue, [c:channel], list).
untracked_fluent(channel_in_queue).
%local_channel_in_queue(Agent, Channel, Role, Queue, S) :-

derived_fluent(local_channel_in_queue, [a:agent, c:channel, role:term], list).

% checks if a message that matches the given parameters has been sent in this timestep
derived_fluent(messageSent, [a:agent, c:channel, role:term, dest:agent, 
	destRole:term, content:term], boolean).

derived_fluent(message_available, [a:agent, c:channel, role:term], boolean).

% SENSOR

% the value after transferStarts
fluent(sensor_transmitted_value, [m:message], term).
untracked_fluent(sensor_transmitted_value).


% local fluent and corresponding timestamp fluent are installed manually
% using helper functions

%for now: remote sensor expressed by channel + sensor

% Actions and Events


primitive_action(requestTransfer, [m:message]).
immediate_action(requestTransfer).

primitive_action(clean_queue, [a:agent, c:channel, role:term]).
immediate_action(clean_queue).

% Updates the value map stored in the automatically introdcued 
% remote sensor fluent. This reads and removes relevant 
% messages in channel_in_queue
primitive_action(update_remote_sensor, [a:agent, r:remoteSensor]).
immediate_action(update_remote_sensor).

exogenous_action(transferStarts, [m:message], [error:term]).
exogenous_action(transferEnds, [m:message], [error:term]).
exogenous_action(transferFails, [m:message], []).

exogenous_action_choice(message_start, [m:message], [transferStarts, transferFails]).
exogenous_action_choice(message_end, [m:message], [transferEnds, transferFails]).


% convention: agent is sender for channels and receiver for (remote) sensors.
% for 
create_message(Con, Agent, MsgType, Params, Msg) :-
	getval(nextMsg, Msg),
	incval(nextMsg),
	get_current(domain, [message], Messages),
	append(Messages, [Msg], Messages2),
	set_current(domain, [message], Messages2),
	set_current(awaitingTransfer, [Msg], false),
	set_current(transferring, [Msg], false),
	set_current(timestamp_S, [Msg], -1),
	set_current(timestamp_T, [Msg], -1),
	setConstant(message_spec, [Msg, msg(Con, MsgType, Agent, Params)]).

	% SSAs
	
get_message_destinations(Msg, DestRole, Destinations, S) :-
	message_spec(Msg, Spec),
	Spec = msg(Con, MsgType, Agent, Params),
	(MsgType = multicastSrc,
		Params = [SrcRole],
		% determine direction of message
		(channel(Con, R1:_, R2:_, multicast),
			(SrcRole = R1,
				EnsembleSpec = Agent:all, DestRole = R2, !
			; SrcRole = R2,
				EnsembleSpec = all:Agent, DestRole = R1, !
			; throw(wrong_role_in_msg(Msg, Con, SrcRole))
			), !
		; throw(wrong_connector_type_for_multicastSrc(Msg, Con))
		), !			
	; MsgType = remoteSensorSrc,	
		(remoteSensor(Con, _, _, _),
			% The message was sent by the sensing (remote) agent. 
			% The ensemble is defined from the perspective 
			% of the receiving agent.
			EnsembleSpec = all:Agent, DestRole = Con, !
			; throw(undefined_remote_sensor(Msg, Con))
		), !			
	; MsgType = multicastDest,
		Params = [_, Dest, DestRole],
		EnsembleSpec = none,
		Destinations = [Dest], !
	; MsgType = unicast,
		Params = [_, Dest, DestRole],
		EnsembleSpec = none,
		Destinations = [Dest], !
	; MsgType = sensor,
		DestRole = Con,
		EnsembleSpec = none,
		Destinations = [Agent], !
	; throw(unsupported_msg_type(Msg, MsgType))
	),
	(EnsembleSpec \= none ->
		get_ensemble_members(Con, EnsembleSpec, EnsemblePairs, S),
		% select parts of ensemble pairs according to query direction
		(foreach(Pair, EnsemblePairs), foreach(D, Destinations), param(EnsembleSpec) do
			Pair = D1:D2,
			(EnsembleSpec = _:all -> D = D2 ; D = D1)
		)		
		;
		true
	).
		
			
get_message_type(Msg, MsgType) :-
	message_spec(Msg, Spec),
	Spec = msg(_, MsgType, _, _).
	
get_src_message(Msg, SrcMessage) :- 
	message_spec(Msg, Spec),
	Spec = msg(_, MsgType, _, Params),
	(MsgType = multicastDest ->
		Params = [SrcMessage, _, _]
		;
		SrcMessage = Msg
	).

get_dest_messages(Msg, AllMessages, DestMessages) :-
	message_spec(Msg, Spec),
	Spec = msg(_, MsgType, _, _),
	((MsgType = multicastSrc, ! ; MsgType = remoteSensorSrc) ->
		(foreach(M, AllMessages), fromto([], In, Out, DestMessages),
			param(Msg) do
				message_spec(M, Spec2),
				Spec2 = msg(_, MsgType2, _, Params2),
				(MsgType2 = multicastDest,
					Params2 = [Msg, _, _],
					append(In, [M], Out), !
					; 
					Out = In
				)
		)
		;
		DestMessages = [Msg]
	).
					
		
	
% for multicast or remoteSensorSrc, a new message might be created by transferStarts.
% there message_spec 
domain(message, D, do2(A, S)) :-
	domain(message, OldD, S),
	(
		((A = transferEnds(Msg, _), ! ; A = transferFails(Msg), !),		
			delete(Msg, OldD, DTemp), !,
			get_message_type(Msg, MsgType),
			% Delete orphaned multicast sorce-half messages, i.e.
			% source-half messages of which all dest-half messageSent
			% have already been deleted.
			(MsgType = multicastDest,
				get_src_message(Msg, SrcMessage),
				get_dest_messages(SrcMessage, DTemp, DestMessages),
				length(DestMessages, 0),
				delete(SrcMessage, DTemp, D), !
				;
				D = DTemp
			)
		), !			
		; A = transferStarts(Msg, _), 
			message_spec(Msg, Spec),
			Spec = msg(Con, MsgType, Agent, Params),
			(MsgType = multicastSrc, ! ; MsgType = remoteSensorSrc), !,
			% DE-MULTIPLEX multicast message
			get_message_destinations(Msg, DestRole, Destinations, S),		
			getval(nextMsg, NextMsg),		
			(foreach(Dest, Destinations), count(NewMsg, NextMsg, NextMsg2),
				fromto(OldD, In, Out, D), 
				param(Agent, Msg, Con, DestRole) do
					append(In, [NewMsg], Out),
					setConstant(message_spec, [NewMsg, 
						msg(Con, multicastDest, Agent, 
							[Msg, Dest, DestRole])])				
			), 
			NextMsg3 is NextMsg2 + 1,
			setval(nextMsg, NextMsg3)
		; D = OldD
	).

	
% performs garbage collection for message specifications, i.e. 
% retracts all message specifications for messages that are not
% in the current domain
clean_message_specs :-
	domain(message, Messages),
	not (
		clause(message_spec(M,_), _),
		(not member(M, Messages) -> 
			retract_constant(message_spec, [M])
			;
			true
		),
		fail
	).
	
awaitingTransfer(Message, do2(A,S)) :-
	A = requestTransfer(Message), !
	;
	A \= transferStarts(Message, _),
	A \= transferFails(Message),
	awaitingTransfer(Message, S).

transferring(Message, do2(A,S)) :-
	A = transferStarts(Message, _), !
	;
	A \= transferEnds(Message, _),
	A \= transferFails(Message),
	transferring(Message, S).

transferring(Message, s0) :-
	get_current(transferring, [Message], Val), !,
	Val = true
	;
	% the default for multicast destination-half messages is transferring since
	% they are created in response to a transfer start
	get_message_type(Message, multicastDest).
	
transferring(Message, slast) :-
	get_last(transferring, [Message], Val), !,
	Val = true.
	
timestamp_S(Message, T, do2(A,S)) :-
	A = requestTransfer(Message), !,
	time(T, S) 
	;
	timestamp_S(Message, T, S).
	
timestamp_S(Message, T, s0) :- 
	get_current(timestamp_S, [Message], T), !
	;
	(
		% for multicast destination-half messages, the default is the transmission start (sic!)
		% timestamp of the source-half message
		message_spec(Message, Spec), 
		Spec = msg(_, MsgType, _, Params),
		MsgType = multicastDest, 
		Params = [SrcMsg, _, _],
		get_current(timestamp_T, [SrcMsg], T), !
		;
		T = -1
	).

timestamp_S(Message, T, slast) :- 
	get_last(timestamp_S, [Message], T), !
	;
	(
		% for multicast destination-half messages, the default is the timestamp of the 
		% source-half message
		message_spec(Message, Spec), 
		Spec = msg(_, MsgType, _, Params),
		MsgType = multicastDest, 
		Params = [SrcMsg, _, _],
		get_last(timestamp_T, [SrcMsg], T), !
		;
		T = -1
	).	


timestamp_T(Message, T, do2(A,S)) :-
	A = transferStarts(Message, _), !,
	time(T, S) 
	;
	timestamp_T(Message, T, S).
	
timestamp_T(Message, T, s0) :- 
	get_current(timestamp_T, [Message], T), !
	;
	(
		% for multicast destination-half messages, the default is the timestamp of the 
		% source-half message
		message_spec(Message, Spec), 
		Spec = msg(_, MsgType, _, Params),
		MsgType = multicastDest, 
		Params = [SrcMsg, _, _],
		get_current(timestamp_T, [SrcMsg], T), !
		;
		T = -1
	).

timestamp_T(Message, T, slast) :- 
	get_last(timestamp_T, [Message], T), !
	;
	(
		% for multicast destination-half messages, the default is the timestamp of the 
		% source-half message
		message_spec(Message, Spec), 
		Spec = msg(_, MsgType, _, Params),
		MsgType = multicastDest, 
		Params = [SrcMsg, _, _],
		get_last(timestamp_T, [SrcMsg], T), !
		;
		T = -1
	).

message_connector(Message, Connector) :-
	(message_spec(Message, Spec) ->
		Spec =  msg(Connector, _, _, _)
		;
		Connector = none
	).
		
% this fluent is set directly by the process and doesn't change
channel_out_content(Message, Content, do2(A, S)) :-
	channel_out_content(Message, Content, S).
	
channel_out_content(Message, Content, s0) :-
	get_current(channel_out_content, [Message], Content), !
	;
	Content = none.

channel_out_content(Message, Content, slast) :-
	get_last(channel_out_content, [Message], Content), !
	;
	Content = none.

channel_transmission_content(Message, Content, do2(A,S)) :-
	A = transferStarts(Message, Error), !,	
	message_spec(Message, Spec),
	Spec = msg(Connector, MsgType, Agent, Params),
	(MsgType = remoteSensorSrc ->
		remoteSensor(Connector, _, LocalSensor, _),
		append([Agent], Params, Params2),
		get_fluent_value(LocalSensor, Params2, Out, S)
		;
		% channel_out_content is used both for unicast and 
		% multicast intentionally sent messages
		channel_out_content(Message, Out, S)
	),		
	error_operator(Connector, Out, Error, Content)
	;
	channel_transmission_content(Message, Content, S).
	
	
channel_transmission_content(Message, Content, s0) :-
	get_current(channel_transmission_content, [Message], Content), !
	;
	% for multicast destination-half messages, the transmitted content
	% is the same as that of the source-half
	(
		message_spec(Message, Spec), 
		Spec = msg(_, MsgType, _, Params),
		MsgType = multicastDest, 
		Params = [SrcMsg, _, _],
		get_current(channel_transmission_content, [SrcMsg], Content), !
		;
		Content = none
	).

channel_transmission_content(Message, Content, slast) :-
	get_last(channel_transmission_content, [Message], Content), !
	;
	Content = none.
	
% message format: src:agent, srcrole:term, dest:agent, destrole:term, timestamp:integer, content:term, 
channel_in_queue(Channel, L, do2(A, S)) :-
	(channel_in_queue(Channel, OldL, S), ! ; OldL = []),
	(A = transferEnds(Msg, Error),
		message_spec(Msg, Spec),
		Spec = msg(Channel, MsgType, Sender, Params),
		channel_transmission_content(Msg, Content, S),
		error_operator(Channel, Content, Error, Content2),
		time(Time, S),
		(MsgType = multicastDest ->
			Params = [SrcMsg, Dest, DestRole],
			message_spec(SrcMsg, SrcSpec),
			SrcSpec = msg(_, SrcMsgType, _, SrcParams),
			(SrcMsgType = remoteSensorSrc ->
				% for remote sensor messages, also store the original parameters
				% used for the local (direct) sensor
				M = msg(Sender, SrcParams, Dest, Channel, Time, Content2)
				;
				SrcParams = [SrcRole],
				M = msg(Sender, SrcRole, Dest, DestRole, Time, Content2)
			)			
			;			
			Params = [SrcRole, Dest, DestRole],
			M = msg(Sender, SrcRole, Dest, DestRole, Time, Content2)
		),
		append(OldL, [M], L), !
		
	; A = clean_queue(Agent, Channel, Role),
		(foreach(M, OldL), fromto([], In,  Out, L), param(Agent, Role) do
			(M \= msg(_, _, Agent, Role, _, _) ->
			append(In, [M], Out)
			;
			Out = In
			)
		), !
	; L = OldL
	).

local_channel_in_queue(Agent, Channel, Role, Queue, S) :-
	channel_in_queue(Channel, QAll, S),
	(foreach(M, QAll), fromto([], In,  Out, Queue), param(Agent, Role) do
		(M = msg(_, _, Agent, Role, _, _) ->
			append(In, [M], Out)
			;
			Out = In
		)
	).

message_available(Agent, Channel, Role, S) :-
	channel_in_queue(Channel, QAll, S),
	M = msg(_, _, Agent, Role, _, _),
	member(M, QAll), !.
	
messageSent(Agent, Channel, Role, Dest, DestRole, Content, S) :-
	domain(message, Msgs),
	member(M, Msgs),
	time(CurrentTime, S),
	timestamp_S(M, CurrentTime, S),
	message_spec(M, Spec),
	Spec = msg(Channel, MsgType, Agent, Params),
	channel_out_content(M, Content, S),
	(MsgType = unicast,
		Params = [Role, Dest, DestRole], !
	; MsgType = multicastSrc,
		Dest = all,
		Params = [Role], !
	; MsgType = multicastDest,
		Params = [SrcMsg, Dest, DestRole],
		message_spec(SrcMsg, SrcSpec),
		SrcSpec = msg(_, _, _, SrcParams),
		SrcParams = [Role]
	).
	

	
% the value that is transmitted for a local (direct) sensor member
sensor_transmitted_value(Message, Value, do2(A, S)) :-
	A = transferStarts(Message, Error),
	message_spec(Message, Spec),
	Spec = msg(Sensor, _, Agent, Params),
	sensor(Sensor, _, SrcFluent),
	(fluent(SrcFluent, _, Type), ! ; derived_fluent(SrcFluent, _, Type), ! ;  throw(fluent_undefined(SrcFluent))),
	L1 = [SrcFluent, Agent | Params],
	(Type = boolean ->
		append(L1, S, L2),
		T =.. L2,
		(call(T) -> SrcValue = true ; SrcValue = false)		
		;
		append(L1, [SrcValue, S], L2),
		T =.. L2,
		call(T)
	),
	error_operator(Sensor, SrcValue, Error, Value), !
	;
	sensor_transmitted_value(Message, Value, S), !
	;
	Value = none.

new_sensor_timestamp(Sensor, Agent, Params, Action, Situation, Timestamp) :-
	Action = transferEnds(Message, _),
	message_spec(Message, Spec),
	Spec = msg(Sensor, _, Agent, Params),
	time(Timestamp, Situation).

	
 
 
message_transmitted_by_agent(Msg, Connector, Agent, S) :-
	transferring(Msg, S),
	message_spec(Msg, Spec),
	Spec = msg(Connector, _, Agent, _).
	
message_count_transmission(Connector, Agent, Count, S) :-
	findall(M, message_transmitted_by_agent(M, Connector, Agent, S), Messages),
	length(Messages, Count).
 
% can be used in the target fluent
new_sensor_value_received(Sensor, Agent, Params, Action ,Situation, Value) :-
	Action = transferEnds(Message, Error),
	message_spec(Message, Spec),
	Spec = msg(Sensor, _, Agent, Params),
	sensor_transmitted_value(Message, TransmittedValue, Situation),
	error_operator(Sensor, TransmittedValue, Error, Value).

poss(requestTransfer(M), S) :-
	not awaitingTransfer(M, S),
	not transferring(M, S).

schedulable(message_start(M), S) :-
	awaitingTransfer(M, S).
	
schedulable(message_end(M), S) :-
	transferring(M, S),
	% a multicast source-half message may not end directly
	get_message_type(M, MsgType),
	MsgType \= multicastSrc,
	MsgType \= remoteSensorSrc.
	

poss(clean_queue(_, _, _), _) :- true.	

poss(update_remote_sensor(_,_), _) :- true.

get_ensemble_participant_types(EnsembleName, PType1, PType2) :-
	channel(EnsembleName, P1, P2, _),
	P1 = _ : PType1,
	P2 = _ : PType2, !
	;
	remoteSensor(EnsembleName, PType1, _, PType2), !.

	
get_ensemble_members(EnsembleName, Spec, Members, S) :-
	get_ensemble_participant_types(EnsembleName, PType1, PType2),
	domain(PType1, D1, S),
	domain(PType2, D2, S),
	Spec = Source:Dest,
	(Source = all, Dest = all, !,
		findall(P1:P2, (member(P1, D1), member(P2, D2), 
			ensemble(EnsembleName, P1, P2, S)), Members)
	; Source = all, Dest \= all, !,	
		findall(P1:Dest, (member(P1, D1), 
			ensemble(EnsembleName, P1, Dest, S)), Members)
	; Source \= all, Dest = all, !,
		findall(Source:P2, (member(P2, D2), 
			ensemble(EnsembleName, Source, P2, S)), Members)
	; 
		findall(Source:Dest, 
			ensemble(EnsembleName, Source, Dest, S), Members)
	).

% The following section contains functions that are used by the simulation engine
% for automatic initialization of the domain.

get_declared_channels(Channels) :-
	findall(c(ChannelName, End1, End2, Mode), channel(ChannelName, End1, End2, Mode), Channels).
	
get_declared_sensors(Sensors) :-
	findall(s(SensorName, OwnerType, SourceFluent), sensor(SensorName, OwnerType, SourceFluent),
		Sensors).

get_declared_remote_sensors(RemoteSensors) :-
	findall(
		rs(RemoteSensorName, RemoteSensorOwner, LocalSensorName, LocalSensorOwner), 
		remoteSensor(RemoteSensorName, RemoteSensorOwner, LocalSensorName, LocalSensorOwner),
		RemoteSensors).
		
add_direct_sensor_fluents :-
	get_declared_sensors(Sensors),
	(foreach(Sensor, Sensors) do
		Sensor = s(SensorName, _, SourceFluent),
		(
			fluent(SourceFluent, Params, Type), !
			;
			derived_fluent(SourceFluent, Params, Type), !
			;
			throw(undeclared_source_fluent(SourceFluent))
		),
		(not fluent(SensorName, _, _) ->
			assert(fluent(SensorName, Params, Type))
			;
			true
		),
		(not untracked_fluent(SensorName) ->
			assert(untracked_fluent(SensorName))
			;
			true
		),
		length(Params, NArgs),
		length(NewParams, NArgs),
		NewParams = [Agent | MsgParams],
		Sit = do2(Action, OldSit), 
		(Type = boolean ->
			append(NewParams, [Sit], NewParams2),
			append(NewParams, [OldSit], NewParamsOldQuery)
			;
			append(NewParams, [Val, Sit], NewParams2),
			append(NewParams, [Val, OldSit], NewParamsOldQuery)
		),			
		Head =.. [SensorName | NewParams2],
		(not clause(Head, _) ->
			OldQuery =.. [SensorName | NewParamsOldQuery],
			
			Query = (
				new_sensor_value_received(SensorName, Agent, MsgParams, 
					Action, OldSit, Val), !
				;
				OldQuery
				),
			assert((Head :- Query))
			;
			true
		)
	).
	
	
get_latest_remote_sensor_value(RemoteSensorName, Agent, SrcAgent, LocalParams, Value, S) :-
	channel_in_queue(RemoteSensorName, QAll, S),
	(foreach(M, QAll), fromto(none: -1, In, Out, V), 
		param(Agent, RemoteSensorName, SrcAgent, LocalParams) do
			(M = msg(SrcAgent, LocalParams, Agent, RemoteSensorName, MsgTime, Content),
				In = _:Latest,
				MsgTime >= Latest,
				Out = Content:MsgTime, !
				;
				Out = In
			)
	),
	V = Value:_,
	Value \= none.
				
				
add_remote_sensor_fluents :-
	get_declared_remote_sensors(RemoteSensors),
	(foreach(RemoteSensor, RemoteSensors) do
		RemoteSensor = rs(RemoteSensorName, RemoteSensorOwner, LocalSensorName, LocalSensorOwner),
		(
			fluent(LocalSensorName, LocalParams, Type), !
			;
			throw(undeclared_local_fluent(LocalSensorName))
		),
		% ignore source agent for local sensor
		LocalParams = [_ | LocalParams2],
		Params = [dest:RemoteSensorOwner, src:LocalSensorOwner],
		append(Params, LocalParams2, Params2),		
		
		(not fluent(RemoteSensorName, _, _) ->
			assert(fluent(RemoteSensorName, Params2, Type))
			;
			true
		),
		(not untracked_fluent(RemoteSensorName) ->
			assert(untracked_fluent(RemoteSensorName))
			;
			true
		),
		length(Params2, NArgs),
		length(NewParamsTemp, NArgs),
		NewParamsTemp = [AgentVar | [SrcAgentVar | LocalParamsVar]],
		append(NewParamsTemp, [Value], NewParams),
		append(NewParams, [do2(Action, OldSit)], NewParams2), 
		append(NewParams, [OldSit], NewParamsOldQuery),		
		Head =.. [RemoteSensorName | NewParams2],
		OldQuery =.. [RemoteSensorName | NewParamsOldQuery],
		(not clause(Head, _) ->
			assert((Head :- 
				Action = update_remote_sensor(AgentVar, RemoteSensorName),
				get_latest_remote_sensor_value(RemoteSensorName, AgentVar, SrcAgentVar, LocalParamsVar, 
					Value, OldSit), !
				;
				OldQuery))
			;
			true
		)
	).
	

init_info_transfer :-
	setval(nextMsg, 1),
	add_direct_sensor_fluents,
	add_remote_sensor_fluents,
	retract_constant(message_spec, [_]).