compile_persistent_fluents :-
	findall((N - F),persistent_fluent(N,F), L),
	(foreach(Entry, L) do
		Entry = Name - Formula,
		compile_formula(Formula, CompiledFormula),
		store_set(persistent_fluents, Name, CompiledFormula)
	).
	
% TODO: rename to persistent_property
query_persistent_fluent(Name, CurrentState, LastChanged) :-
	(state_dirty -> update_persistent_fluents ; true),
	internal_query_persistent_fluent(Name, CurrentState, LastChanged).
	
internal_query_persistent_fluent(Name, CurrentState, LastChanged) :-
	(store_get(persistent_fluent_states, Name, S) ->
		S = CurrentState : LastChanged 
		;
		CurrentState = nondet,
		LastChanged = -1
	).
update_persistent_fluents :-
	stored_keys_and_values(persistent_fluents, L),
	current_time(CurrentTime),		
	(foreach(Entry, L), param(CurrentTime) do
		Entry = Name - Formula,
		internal_query_persistent_fluent(Name, CurrentState, _),
		evaluate_formula(null, [0], 0,
			CurrentTime, CurrentTime, Formula, 0, Result, _, _, _),
		(Result \= CurrentState ->
			store_set(persistent_fluent_states, Name, Result : CurrentTime)
			;
			true
		)
	),
	set_state_dirty(false).

print_persistent_fluents :-
	findall((N - F),persistent_fluent(N,F), L),

	(foreach(Entry, L) do
		Entry = Name - Formula,
		query_persistent_fluent(Name, CurrentState, LastChanged),
		store_get(persistent_fluents, Name, CF),
		printf("%s = %8s (%10d)\nFormula: %w\n Compiled: %w\n\n",[Name, CurrentState, LastChanged, Formula, CF])
	).
