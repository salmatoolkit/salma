:- dynamic channel/4, sensor/3, remoteSensor/4, ensemble/4,
	awaitingTransfer/5, transferring/5, timestamp_S/6, timestamp_T/6,
	timestamp_R/5, nextMsg/5.

	
sort(message).
sort(connector).

% STATE FLUENTS
fluent(awaitingTransfer, [m:message], boolean).
untracked_fluent(awaitingTransfer).

fluent(transferring, [m:message], boolean).
untracked_fluent(transferring).
	
fluent(timestamp_S, [m:message], integer).
untracked_fluent(timestamp_S).


fluent(timestamp_T, [m:message], integer).
untracked_fluent(timestamp_T).

% list content: (agent, params, tstamp)
fluent(connector_timestamps_R, [con:connector], list).

derived_fluent(timestamp_R, [con:connector, agent:agent, params:list], integer).

	
	
fluent(channelOut, [channelName:term, role:term, agent:agent, 
						dest:term, 
	
derived_fluent(nextMsg, [id:term, agent:agent, params:list], integer).

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


