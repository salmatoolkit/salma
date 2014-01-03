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


check_constant(CName, Params, Check) :-
	SigLen is length(Params) + 1,
	(not is_dynamic(CName/SigLen) -> 
		Check = not_dynamic
		;
		Check = ok
	).	
	
check_fluents :-
	get_declared_fluents(DirectFluents),
	get_declared_derived_fluents(DerivedFluents),
	append(DirectFluents, DerivedFluents, Fluents),
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

check_constants :-
	get_declared_constants(Constants),
	(foreach(C, Constants), fromto(0, In, Out, Positives) do
		C = c(CName,Params,Type),
		printf("%s(%w) -> %s\n",[CName, Params, Type]),
		catch(check_constant(CName, Params, Check), T, 
			Check = T),
		printf("\t%s\n",[Check]),
		(Check = ok -> Out is In + 1 ; Out is In)
	),
	Total is length(Constants),
	printf("Total: %d, OK: %d\n",[Total, Positives]).
	

check_action_poss(ActionName, Params, Check) :- 
	SigLen is length(Params), 
	length(L, SigLen),
	Act =.. [ActionName | L],
	(clause(poss(Act, _), _) ->
		Check = ok
		;
		Check = no_poss_axiom
	).
	
check_actions :-
	get_declared_primitive_actions(PrimitiveActions),
	get_declared_stochastic_actions(StochasticActions),
	get_declared_exogenous_actions(ExogenousActions),
	printf("Primitive Actions\n----------------\n",[]),
	(foreach(Act, PrimitiveActions), fromto(0, In, Out, OkPrimitiveActions) do
		Act = pa(Name, Params),
		printf("%s(%w)\n",[Name, Params]),
		catch(check_action_poss(Name, Params, Check), T, 
			Check = T),
		printf("\t%s\n",[Check]),
		(Check = ok -> Out is In + 1 ; Out is In)
	),
	L1 is length(PrimitiveActions),
	printf("PrimitiveActions: %d, OK: %d\n\n",[L1, OkPrimitiveActions]),
	
	printf("Stochastic Actions\n----------------\n",[]),
	(foreach(Act, StochasticActions), fromto(0, In, Out, OkStochasticActions) do
		Act = pa(Name, Params, Outcomes),
		printf("%s(%w) -> [%w]\n",[Name, Params, Outcomes]),
		catch(check_action_poss(Name, Params, Check), T, 
			Check = T),
		printf("\t%s\n",[Check]),
		(Check = ok -> Out is In + 1 ; Out is In)
	),
	L2 is length(StochasticActions),
	printf("StochasticActions: %d, OK: %d\n\n",[L2, OkStochasticActions]),
	
	printf("Exogenous Actions\n----------------\n",[]),
	(foreach(Act, ExogenousActions), fromto(0, In, Out, OkExogenousActions) do
		Act = ea(Name, Params1, Params2),
		printf("%s(%w, %w)\n",[Name, Params1, Params2]),
		append(Params1, Params2, Params),
		catch(check_action_poss(Name, Params, Check), T, 
			Check = T),
		printf("\t%s\n",[Check]),
		(Check = ok -> Out is In + 1 ; Out is In)
	),
	L3 is length(ExogenousActions),	
	printf("ExogenousActions: %d, OK: %d\n\n",[L3, OkExogenousActions]),
	Total is L1 + L2 + L3,
	TotalOK is OkPrimitiveActions + OkStochasticActions + OkExogenousActions,
	printf("All actions: %d, OK: %d\n",[Total, TotalOK]).
	
	