% Adds the given formula to formula_cache. Before actually adding a new entry to formula_cache,
% we check if we've already have the same formula cached. To speed up search, we keep an extra 
% storage formula_cache_candidates that stores a list of cache ids for each formula position (path).
cache_formula(ToplevelFormula, FormulaPath, F, Id) :-
        /* $D$ */ 
        (not ToplevelFormula = null ->
            /* $D$ */ 
            Key =.. [ToplevelFormula | FormulaPath],
            (store_get(formula_cache_candidates, Key, Candidates), /* $D$ */ ! ; /* $D$ */ Candidates = []),
            (fromto(Candidates, In, Out1, []), fromto(-1, _, Out2, MatchingKey), param(F) do
                /* $D$ */ 
                In = [CId | Rest],
                get_cached_formula(CId, CachedF),
                (CachedF = F -> 
                    /* $D$ */ 
                    Out1 = [], 
                    Out2 is CId
                ;
                    /* $D$ */ 
                    Out1 = Rest,
                    Out2 is -1
                )
            )
        ;
            /* $D$ */ 
            MatchingKey is -1
        ),
        
        (MatchingKey is -1 ->				
            /* $D$ */ 
            incval(next_formula_cache_id),
            getval(next_formula_cache_id, Id),
            store_set(formula_cache, Id, F),
            (not ToplevelFormula = null ->
                /* $D$ */ 
                Candidates2 = [Id | Candidates],
                store_set(formula_cache_candidates, Key, Candidates2)
            ; /* $D$ */ true
            )
        ;
            /* $D$ */ 
            Id is MatchingKey
        ).

get_cached_formula(Id, F) :-
        /* $D$ */ 
        store_get(formula_cache, Id, F).
		
print_formula_cache(Stream) :-
        stored_keys_and_values(formula_cache, L),
        (foreach(Entry, L), param(Stream) do
            Entry = Id - F,
            printf(Stream, "%10d %w\n",[Id, F])
        ).

print_cache_candidates(Stream):-
        stored_keys_and_values(formula_cache_candidates, L),
        (foreach(Entry, L), param(Stream) do
            Entry = Key - IdList,
            printf(Stream, "%w %w\n",[Key, IdList])
        ).
