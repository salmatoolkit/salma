print_fluents :-
	get_declared_fluents(Fluents),
	(foreach(F, Fluents) do
		F = f(FName,Params,Type),
		printf("%s(%w) -> %s\n",[FName, Params, Type]),
		((doc(FName : fluent, Content), member(summary:Summary, Content)) ->
			printf("\t%s\n", Summary)
			;
			true
		)
	).
	
check_fluent(FName, Params, Type, Check) :-
	(Type = boolean ->
		SigLen is length(Params) + 1
		;
		SigLen is length(Params) + 2
	),
	(not is_dynamic(FName/SigLen) -> 
		Check = not_dynamic
		;
		length(L, SigLen),
		C =.. [FName | L],
		(clause(C, _) ->
			Check = ok
			;
			Check = no_axiom		
		)
	).
	
	
check_fluents :-
	get_declared_fluents(Fluents),
	(foreach(F, Fluents), fromto(0, In, Out, Positives) do
		F = f(FName,Params,Type),
		printf("%s(%w) -> %s\n",[FName, Params, Type]),
		catch(check_fluent(FName, Params, Type, Check), T, 
			Check = T),
		printf("\t%s\n",[Check]),
		(Check = ok -> Out is In + 1 ; Out is In)
	),
	Total is length(Fluents),
	printf("Total: %d, OK: %d\n",[Total, Positives]).
