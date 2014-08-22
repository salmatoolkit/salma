:- lib(lists).
:- dynamic channel/4, sensor/3, remoteSensor/4, ensemble/4,
	awaitingTransfer/2, transferring/2, timestamp_S/3, timestamp_T/3,
	message_spec/2, 
	message_count_transmission/4, messageSent/7,
	channel_out_content/3, channel_transmission_content/3,
	channel_in_queue/3, local_channel_in_queue/5, 
	sensor_transmitted_value/3,
	
	% function that adds error to original value
	% error_operator(con:Connector, OrigValue, Error, NewValue)
	error_operator/4.
	
:- local variable(nextMsg,1).

sort(message).
sort(connector).
sorts([channel, sensor, remoteSensor]).
subsorts([channel, sensor, remoteSensor], connector).

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
derived_fluent(messageSent, [a:agent, c:channel, role:term, dest:channel, 
	destRole:term, content:term], boolean).

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

exogenous_action(transferStarts, [m:message], [error:term]).
exogenous_action(transferEnds, [m:message], [error:term]).
exogenous_action(transferFails, [m:message], []).



% convention: agent is sender for channels and receiver for (remote) sensors
% unicast channel: Params = [SrcRole, Dest, DestRole],
% multicast channel / remote sensor: Partams = [SrcRole]
create_message(Con, Agent, Params, Msg) :-
	getval(nextMsg, Msg),
	incval(nextMsg),
	get_current(domain, [message], Messages),
	append(Messages, [Msg], Messages2),
	set_current(domain, [message], Messages2),
	set_current(awaitingTransfer, [Msg], false),
	set_current(transferring, [Msg], false),
	set_current(timestamp_S, [Msg], -1),
	set_current(timestamp_T, [Msg], -1),
	setConstant(message_spec, [Msg, msg(Con, Agent, Params)]).

	% SSAs
	
domain(message, D, do2(A, S)) :-
	domain(message, OldD, S),
	((A = transferEnds(Msg, _), ! ; A = transferFails(Msg), !) ->		
			delete(Msg, OldD, D)
			;
			D = OldD
	), !
	;
	D = [].
	
awaitingTransfer(Message, do2(A,S)) :-
	A = requestTransfer(Message), !
	;
	A \= transferStarts(Message, _),
	A \= transferFails(Message),
	awaitingTransfer(Message, S), !.

transferring(Message, do2(A,S)) :-
	A = transferStarts(Message, _), !
	;
	A \= transferEnds(Message, _),
	A \= transferFails(Message),
	transferring(Message, S), !.	
	
timestamp_S(Message, T, do2(A,S)) :-
	A = requestTransfer(Message), !,
	time(T, S) 
	;
	timestamp_S(Message, T, S), !
	;
	T = -1.

timestamp_T(Message, T, do2(A,S)) :-
	A = transferStarts(Message, _), !,
	time(T, S) 
	;
	timestamp_T(Message, T, S), !
	;
	T = -1.

message_connector(Message, Connector) :-
	(message_spec(Message, Spec) ->
		Spec =  msg(Connector, _, _)
		;
		Connector = none
	).
		
% this fluent is set directly by the process and doesn't change
channel_out_content(Message, Content, do2(A, S)) :-
	channel_out_content(Message, Content, S), !
	;
	Content = none.
	
channel_transmission_content(Message, Content, do2(A,S)) :-
	A = transferStarts(M, Error), !,
	channel_out_content(Message, Out, S),
	message_connector(Message, Connector),
	error_operator(Connector, Out, Error, Content)
	;
	channel_transmission_content(Message, Content, S), !
	;
	Content = none.

% message format: src:agent, srcrole:term, dest:agent, destrole:term, timestamp:integer, content:term, 
channel_in_queue(Channel, L, do2(A, S)) :-
	(channel_in_queue(Channel, OldL, S), ! ; OldL = []),
	(A = transferEnds(Msg, Error),
		message_spec(Msg, Spec),
		Spec = msg(Channel, Sender, Params),
		Params = [SrcRole, Dest, DestRole],
		channel_transmission_content(Msg, Content, S),
		error_operator(Channel, Content, Error, Content2),
		time(Time, S),
		M = msg(Sender, SrcRole, Dest, DestRole, Time, Content2),
		append(OldL, [M], L), !
		;
	A = clean_queue(Agent, Channel, Role),
		(foreach(M, OldL), fromto([], In,  Out, L), param(Agent, Role) do
			(M \= msg(_, _, Agent, Role, _, _) ->
			append(In, [M], Out)
			;
			Out = In
			)
		), !
		;
		L = OldL
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


	
messageSent(Agent, Channel, Role, Dest, DestRole, Content, S) :-
	domain(message, Msgs),
	member(M, Msgs),
	time(CurrentTime, S),
	timestamp_S(M, CurrentTime, S),
	message_spec(M, Spec),
	Spec = msg(Channel, Agent, Params),
	Params = [Role, Dest, DestRole],
	channel_out_content(M, Content, S), !.
	


sensor_transmitted_value(Message, Value, do2(A, S)) :-
	A = transferStarts(Message, Error),
	message_spec(Message, Spec),
	Spec = msg(Sensor, Agent, Params),
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
	Spec = msg(Sensor, Agent, Params),
	time(Timestamp, Situation).

	
 
 
message_transmitted_by_agent(Msg, Connector, Agent, S) :-
	transferring(Msg, S),
	message_spec(Msg, Spec),
	Spec = msg(Connector, Agent, _).
	
message_count_transmission(Connector, Agent, Count, S) :-
	findall(M, message_transmitted_by_agent(M, Connector, Agent, S), Messages),
	length(Messages, Count).
 
% can be used in the target fluent
new_sensor_value_received(Sensor, Agent, Params, Action ,Situation, Value) :-
	Action = transferEnds(Message, Error),
	message_spec(Message, Spec),
	Spec = msg(Sensor, Agent, Params),
	sensor_transmitted_value(Message, TransmittedValue, Situation),
	error_operator(Sensor, TransmittedValue, Error, Value).

poss(requestTransfer(M), S) :-
	not awaitingTransfer(M, S),
	not transferring(M, S).

poss(transferStarts(M, _), S) :-
	awaitingTransfer(M, S).

poss(transferEnds(M, _), S) :-
	transferring(M, S).

poss(transferFails(M), S) :-
	awaitingTransfer(M, S), !
	;
	transferring(M,S).

poss(clean_queue(_, _, _), S) :- true.	


get_ensemble_participant_types(EnsembleName, PType1, PType2) :-
	channel(EnsembleName, P1, P2, _)
	P1 = _ : PType1,
	P2 = _ : PType2, !
	;
	remoteSensor(EnsembleName, PType1, _, PType2), !.




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



