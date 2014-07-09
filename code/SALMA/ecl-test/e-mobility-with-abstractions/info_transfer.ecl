:- dynamic channel/4, sensor/3, remoteSensor/4, ensemble/4,
	awaitingTransfer/2, transferring/2, timestamp_S/3, timestamp_T/3,
	connector_timestamps_R/3, timestamp_R/5, nextMsg/5,
   message_receiving_agent/3, message_params/3.

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
	
fluent(message_receiving_agent, [m:message], agent). 
untracked_fluent(message_receiving_agent).


fluent(message_params, [m:message], list).
untracked_fluent(message_params).


fluent(timestamp_S, [m:message], integer).
untracked_fluent(timestamp_S).


fluent(timestamp_T, [m:message], integer).
untracked_fluent(timestamp_T).

% list content: (agent, params, tstamp)
fluent(connector_timestamps_R, [con:connector], list).
untracked_fluent(connector_timestamps_R).
                
derived_fluent(timestamp_R, [con:connector, agent:agent, params:list], integer).
	

derived_fluent(nextMsg, [], integer).

derived_fluent(message_count, [con:connector, a:agent, phase:term],
               integer).

% CHANNEL

% message format: role:term, agent:agent, dest:[*:agent] 
fluent(channelOut, [con:connector], list).
untracked_fluent(channelOut).


% Actions and Events

primitive_action(requestTransfer, [id:term, agent:agent, params:list, m:integer]).

exogenous_action(transferStarts, [id:term, agent:agent, params:list, m:integer], [error:term]).
exogenous_action(transferEnds, [id:term, agent:agent, params:list, m:integer], [error:term]).
exogenous_action(transferFails, [id:term, agent:agent, params:list, m:integer], []).

% SSAs

awaitingTransfer(Id, Agent, Params, M, do2(A,S)) :-
	A = requestTransfer(Id, Agent, Params, M), !
	;
	A \= transferStarts(Id, Agent, Params, M, _),
	A \= transferFails(Id, Agent, Params, M),
	awaitingTransfer(Id, Agent, Params, M, S), !.

transferring(Id, Agent, Params, M, do2(A,S)) :-
	A = transferStarts(Id, Agent, Params, M, _), !
	;
	A \= transferEnds(Id, Agent, Params, M, _),
	A \= transferFails(Id, Agent, Params, M),
	transferring(Id, Agent, Params, M, S), !.


