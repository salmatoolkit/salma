close_successor_state_axioms :- 
	add_default_domain_ssas,
	findall(fluent(Name,Args,Type),fluent(Name,Args,Type), Fluents),
	(foreach(F, Fluents) do
		F = fluent(Name,Args,Type),
		add_storage_query_to_ssa(Name,Args,Type, s0, get_current),
		add_storage_query_to_ssa(Name,Args,Type, slast, get_last)
	).
	
add_default_domain_ssas :-
	get_all_sorts(Sorts),
	(foreach(Sort, Sorts) do
		(not clause(domain(Sort, _, do2(_,_)), _) ->
			assert(domain(Sort, D, do2(_,S)) :- domain(Sort, D, S)) 
			;
			true
		)
	).
		
add_storage_query_to_ssa(Name, Args, Type, Sit, QueryName) :-
	length(Args, NArgs),
	length(NewArgs, NArgs),
	var(Val),
	QueryArgs = [Name, NewArgs, Val],
	Query =.. [QueryName | QueryArgs],
	(Type = boolean ->
		append(NewArgs,[Sit], NewArgsWithSit),
		Head =.. [Name | NewArgsWithSit],
		(not clause(Head, _) ->
			assert((Head :- Query, Val = true))
			; 
			true
		)
		;
		append(NewArgs,[Val,Sit], NewArgsWithSit),
		Head =.. [Name | NewArgsWithSit],
		(not clause(Head, _) ->
			%assert((Head :- Query, ! ; Val = none))
			assert((Head :- Query))
			;
			true
		)
	).	
	
construct_boolean_fluent_ssa(FluentName, Params, Head, Body) :-
	Len is length(Params),
	length(Args1, Len),
	Qualifier =.. [FluentName | Args1],
	append(Args1, [do2(A,S)], Args),
	append(Args1, [S], OldArgs),
	Head =.. [FluentName | Args],
	OldQuery =.. [FluentName | OldArgs],
	Body = (
				(OldQuery -> OldValue = true ; OldValue = false),
				(
					effect(Qualifier, A, OldValue, NewValue, S), !
					;
					NewValue = OldValue
				),
				NewValue = true
	).						
									
construct_nonboolean_fluent_ssa(FluentName, Params, Head, Body) :-
	Len is length(Params),
	length(Args1, Len),
	Qualifier =.. [FluentName | Args1],
	append(Args1, [NewValue, do2(A,S)], Args),
	append(Args1, [OldValue, S], OldArgs),
	Head =.. [FluentName | Args],
	OldQuery =.. [FluentName | OldArgs],
	Body = (
				OldQuery,
				(
					effect(Qualifier, A, OldValue, NewValue, S), !
					;
					NewValue = OldValue
				)
	).	

	
% Constructs successor state axioms (SSAs) from effect axioms.
% Checks for every fluent whether a SSA clause is defined. 
% If not, a clause is generated that uses defined effect clauses
% of the form effect(fluent-term, action-term, old-value, new-value, situation) to
% calculate the new value. The generated clause uses the old value for actions
% that don't match any effect clause. This default fall-back to the old value of
% the last situation is also set up for fluents without any SSA or effect clause.
construct_ssas :-
	get_declared_fluents(Fluents),
	(foreach(F, Fluents), fromto([], HandledIn, HandledOut, _) do
		F = f(FluentName, Params, Type),
		(member(FluentName, HandledIn) ->
			HandledOut = HandledIn
			;
			(Type = boolean ->
				construct_boolean_fluent_ssa(FluentName, Params, Head, Body)
				;
				construct_nonboolean_fluent_ssa(FluentName, Params, Head, Body)
			),	
			% if there's already a fluent clause then back off!
			(not clause(Head, _) ->
				retractall(Head),
				asserta((Head :- Body))
				;
				true
			),
			append(HandledIn, [FluentName], HandledOut)
		)
	).