:- dynamic lock_held_by/3.

sort(lock).

fluent(lock_held_by, [l:lock], term).

primitive_action(acquire_lock, [a:agent, proc:term, l:lock]).
atomic_action(acquire_lock).

primitive_action(release_lock, [a:agent, proc:term, l:lock]).
atomic_action(release_lock).

effect(lock_held_by(Lock), acquire_lock(Agent, Proc, Lock), _, l(Agent, Proc), _).
effect(lock_held_by(Lock), release_lock(_, _, Lock), _, none, _).

poss(acquire_lock(Agent, Proc, Lock), S) :-
	lock_held_by(Lock, none, S).
	
poss(release_lock(Agent, Proc, Lock), S) :-
	lock_held_by(Lock, LockedBy, S),
	LockedBy = l(Agent, Proc).

