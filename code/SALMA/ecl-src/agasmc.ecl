:- lib(ic).
:- lib(hash).
:- dynamic fluent/3.
:- dynamic effect/5.
:- dynamic constant/3.
:- dynamic doc/2.
:- dynamic derived_fluent/3.
:- dynamic poss/2.
:- dynamic caused/3.
:- dynamic schedulable/2. 
:- dynamic event_alternatives/1.
:- dynamic untracked_fluent/1.
:- ['termutils'].
:- ['sort_system'].
:- ['agasmc_progression'].
:- ['locking'].
:- ['property_compiler'].
:- ['property_evaluator'].
:- ['info_transfer'].
:- ['gologinterpreter'].
:- ['devtools'].



evaluation_step(ToplevelResults, ScheduledResults, PendingGoals, FailureStack) :-
	current_time(CurrentTime),
	evaluation_step(CurrentTime, ToplevelResults, ScheduledResults, PendingGoals, FailureStack).	


evaluation_step(EndTime, ToplevelResults, ScheduledResults, PendingGoals, FailureStack) :-
	% TODO: check for events and set clocks
	(properties_unsynced -> recompile_all ; true),
	erase_failure_stack,
	update_persistent_fluents,
	evaluate_all_scheduled(EndTime, ScheduledResults), 
	get_pending_toplevel_goals(PendingGoals),
	evaluate_toplevel(EndTime, ToplevelResults),
	get_merged_failures(FailureStack).
	%,clean_formula_cache.

init_agasmc :-
	init_info_transfer,
	init_progression,
	init_smc.

	
reset_agasmc :-
	init_info_transfer,
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
	set_current(domain, [Domain], Entities2).

% sets value for the given constant instance. A boolean constant must have a value parameter,
% too.
setConstant(ConstantName, Params) :-
	convert_args(Params, Params2),
	T =.. [ConstantName | Params2],
	asserta((T :- true, !)).

% TODO: implement
%get_all_constant_instances(ConstantInstances) :-
%	true.