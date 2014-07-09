:- lib(lists).
:- dynamic channel/4, sensor/3, remoteSensor/4, ensemble/4,
	awaitingTransfer/2, transferring/2, timestamp_S/3, timestamp_T/3,
	message_spec/2,
	% function that adds error to original value
	% error_operator(con:Connector, OrigValue, Error, NewValue)
	error_operator/4.
	
:- local variable(nextMsg,1).

sort(message).
sort(connector).
sorts([channel, sensor, remoteSensor]).
subsorts([channel, sensor, remoteSensor], connector).

dynamic_sort(message).

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
	
derived_fluent(message_count, [con:connector, a:agent, phase:term],
               integer).


			   
% CHANNEL

% contains the message content before transferStarts
fluent(channel_out_content, [m:message], term).
untracked_fluent(channel_out_content).

% contains the message content after transferStarts
fluent(channel_transmission_content, [m:message], term).
untracked_fluent(channel_transmission_content).

% message format: src:agent, srcrole:term, destrole:term, content:term, timestamp:integer
fluent(channel_in_queue, [c:channel, dest:agent], list).
untracked_fluent(channel_in_queue).


% SENSOR

% the value after transferStarts
fluent(sensor_transmitted_value, [m:message], term).
untracked_fluent(sensor_transmitted_value).

% local fluent is installed manually

%for now: remote sensor as abbreviation

% Actions and Events


primitive_action(requestTransfer, [m:message]).


exogenous_action(transferStarts, [m:message], [error:term]).
exogenous_action(transferEnds, [m:message], [error:term]).
exogenous_action(transferFails, [m:message], []).



% convention: agent is sender for channels and receiver for (remote) sensors
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
	setConstant(message_spec, [Msg], msg(Con, Agent, Params)).

	% SSAs
	
domain(message, D, do2(A, S)) :-
	domain(message, OldD, S),
	((A = transferEnds(Msg, _), ! ; A = transferFails(Msg), !) ->
			
			delete(Msg, oldD, D) =
			;
			D = OldD
	).
	
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
	A = requestTransfer(_, _, _, Message), !,
	current_time(T) 
	;
	timestamp_S(Message, T, S), !
	;
	T = -1.

timestamp_T(Message, T, do2(A,S)) :-
	A = transferStarts(Message, _), !,
	current_time(T) 
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

% message format: src:agent, srcrole:term, destrole:term, timestamp:integer, content:term, 
channel_in_queue(Channel, Dest, L, do2(A, S)) :-
	(channel_in_queue(Channel, Dest, OldL, S), ! ; OldL = []),
	(A = transferEnds(Msg, Error),
		message_spec(Msg, Spec),
		Spec = msg(_, Sender, Params),
		Params = [SrcRole, Dest, DestRole],
		channel_transmission_content(Msg, Content, S),
		error_operator(Channel, Content, Error, Content2),
		time(Time, S),
		M = msg(Sender, SrcRole, DestRole, Time, Content2),
		append(OldL, [M], L), !
		;
		L = OldL
	).
	
			
			