:- lib(ic).
:- ['sort_system'].
:- ['agasmc_progression'].
:- ['property_compiler'].
:- ['termutils'].
:- ['property_evaluator'].
:- ['gologinterpreter'].




% Compiles and registers a property to check
register_property(Name, P, P2) :-
	compile_formula(P, P2),
	add_toplevel_goal(Name, P2).

	
register_property_str(Name, PStr, P2) :-
	term_string(P, PStr),
	register_property(Name, P, P2).

register_persistent_fluent(Name, Formula, CompiledFormula) :-
	compile_formula(Formula, CompiledFormula),
	add_persistent_fluent(Name, CompiledFormula).
	
	
	
evaluation_step(OverallResult, ToplevelResults, ScheduledResults) :-
	% TODO: check for events and set clocks
	update_persistent_fluents,
	evaluate_all_scheduled(ScheduledResults, OverallScheduledResults),
	evaluate_toplevel(ToplevelResults, OverallToplevelResults),
	result_and(OverallToplevelResults, OverallScheduledResults, OverallResult),
	progress([tick]).

	

init_agasmc :-
	init_progression,
	init_smc.

	
reset_agasmc :-
	init_progression,
	reset_smc.

convert_args(Args, Vars) :-
	(foreach(Arg, Args), foreach(Var,Vars) do
		(Arg = zero -> Var = 0 ; Var = Arg)
	).


% Creates domain membership constraints for the given parameters.
% 
% Params: list of (variable : type) pairs or ground values
% ConstrainedParams: list of variables.
create_param_constraints(Params, ConstrainedParams) :-
	(foreach(P, Params), foreach(PTerm, ConstrainedParams) do
		(P = (Var : Type), !,
			domain(Type, D),
			member(Var, D),
			PTerm = Var
		; P = zero, !,
			PTerm = 0
		; 
			PTerm = P
		)
	).
	
% Calls Predicate with the given parameters. The given parameter list can contain ground
% values together with typed variables. For each typed variable, a domain constraint is 
% established before the predicate call.
% Params: list of either ground values or (Variable : Type) tuples.
select_entities(Predicate, Params) :-
	create_param_constraints(Params, ConstrainedParams),
	T =.. [Predicate | ConstrainedParams],
	call(T).

% Attempts to create a valid plan for a procedure call with the
% given parameters. The paramer list can contain ground values or
% typed finite domain variables given as (Var : Sort). 
create_procedure_plan(Procedure, Params, Plan) :-
	create_param_constraints(Params, ConstrainedParams),
	T =.. [Procedure | ConstrainedParams],
	do2(T, s0, S),
	situation2action_list(S,Plan).
	
setDomain(Domain, Entities) :-
	convert_args(Entities, Entities2),
	assert(domain(Domain, D) :- D = Entities2).
	
setConstant(ConstantName, Params) :-
	convert_args(Params, Params2),
	T =.. [ConstantName | Params2],
	assert(T).