:- dynamic xpos/3, ypos/3, carrying/3, active/2, painted/2,
	worst_time/2,
	robot_radius/2,
	gravity/1,
	wcet/2.

sorts([component, knowledge_item, ensemble, process,
		runnable]).


		
fluent(local_knowledge, [component, knowledge_item], value).
fluent(belief, [component, component, knowledge_item], value).
% [sender, receiver] - flib for receive
fluent(queue, [component, component], list).
fluent(running, [runnable], boolean).

primitive_action(update_knowledge,[component, knowledge_item, term]).

% adds the knowledge item to the queue
% [sender, receiver, update= [item_1:value, ..., item_n:value] ]
primitive_action(send, [component, component, knowledge_update]).
% [receiver, sender]
primitive_action(receive, [component, component]).


% assume unlimited queues for now
poss(send(_,_,_), _) :- true.
poss(receive(Receiver, Sender), S) :- 
	queue(Sender, Receiver, Q, S),
	length(Q) > 0.	

queue(Sender, Receiver, QueueContent, do2(A,S)) :-
	queue(Sender, Receiver, OldContent, S),
	(A = send(Sender, Receiver, Update),
		current_time(CurrentTime),
		append(OldQueue, [u(CurrentTime, Update)], QueueContent), !
		;
	A = receive(Receiver, Sender),
		% remove first element. [] case is blocked by poss.
		OldContent = [_ | QueueContent], !		
		;
	% TODO: handle expiration in tick
	QueueContent = OldContent, !
	).
		
belief(C1, C2, KItem, Val, do2(A,S)) :-
	belief(C1, C2, KItem, OldVal, S),
	(A = receive(C1, C2), 
		queue(C2, C1, QueueContent, S),
		QueueContent = [Msg | _],
		% ignore time for now
		Msg = u(_, Update),
		member(KItem : Val, Update), !
	;
		Val = OldVal
	).

local_knowledge(Component, KnowledgeItem, Value, do2(A,S)) :-
	A = update_knowledge(Component, KnowledgeItem, Value), !
	;
	local_knowledge(Component, KnowledgeItem, Value, S).
	