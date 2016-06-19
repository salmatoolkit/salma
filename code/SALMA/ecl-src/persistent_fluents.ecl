compile_persistent_fluents :-
        /* $D$ */ 
        findall((N - F),persistent_fluent(N,F), L),
        (foreach(Entry, L) do
            /* $D(IGNORE)$ */ 
            Entry = Name - Formula,
            compile_formula(Formula, CompiledFormula),
            store_set(persistent_fluents, Name, CompiledFormula)
        ).
	
% TODO: rename to persistent_property
query_persistent_fluent(Name, CurrentState, LastChanged) :-
        /* $D(IGNORE)$ */ 
        (state_dirty -> /* $D(IGNORE)$ */ update_persistent_fluents ; /* $D(IGNORE)$ */ true),
        internal_query_persistent_fluent(Name, CurrentState, LastChanged).

internal_query_persistent_fluent(Name, CurrentState, LastChanged) :-
        /* $D(IGNORE)$ */ 
        (store_get(persistent_fluent_states, Name, S) ->
            /* $D(IGNORE)$ */ 
            S = CurrentState : LastChanged 
        ;
            /* $D(IGNORE)$ */ 
            CurrentState = nondet,
            LastChanged = -1
        ).

update_persistent_fluents :-
        /* $D(IGNORE)$ */ 
        stored_keys_and_values(persistent_fluents, L),
        current_time(CurrentTime),		
        (foreach(Entry, L), param(CurrentTime) do
            /* $D(IGNORE)$ */ 
            Entry = Name - Formula,
            internal_query_persistent_fluent(Name, CurrentState, _),
            evaluate_formula(null, [0], 0,
                             CurrentTime, CurrentTime, Formula, 0, Result, _, _, _),
            (Result \= CurrentState ->
                /* $D(IGNORE)$ */ 
                store_set(persistent_fluent_states, Name, Result : CurrentTime)
            ;
                /* $D(IGNORE)$ */ 
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
