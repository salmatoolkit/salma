:- dynamic channel/4, sensor/3, remoteSensor/4, ensemble/4,
	awaitingTransfer/5, transferring/5, timestamp_S/6, timestamp_T/6,
	timestamp_R/5, msgs/5, nextMsg/5.
	
% STATE FLUENTS
fluent(awaitingTransfer, [id:term, agent:agent, params:list, m:integer], 
	boolean).

fluent(transferring, [id:term, agent:agent, params:list, m:integer], 
	boolean).
	
fluent(timestamp_S, [id:term, agent:agent, params:list, m:integer],
	integer).

fluent(timestamp_T, [id:term, agent:agent, params:list, m:integer],
	integer).
fluent(timestamp_R, [id:term, agent:agent, params:list],
	integer).
	
fluent(msgs, [id:term, agent:agent, params:list], list).
	
	
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


