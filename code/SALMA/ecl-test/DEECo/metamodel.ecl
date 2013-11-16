sorts([component, knowledge_item, ensemble, process,
		runnable]).

		
fluent(local_knowledge, [component, knowledge_item], value).
fluent(belief, [component, component, knowledge_item], value).
% [sender, receiver] - flib for receive
fluent(queue, [component, component], list).
fluent(running, [runnable], boolean).

primitive_action(update_knowledge,[component, knowledge_item, term]).
primitive_action(update_belief,[component, component, knowledge_item, term]).

% adds the knowledge item to the queue
% [source, dest, item]
primitive_action(send, [component, component, knowledge_item]).

primitive_action(retrieve, [component, component, knowledge_item]).


