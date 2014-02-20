erase_forall(Var, Type, Formula, Out) :-
		domain(Type, D) -> 
			(foreach(Entity, D), foreach(F, Conjunction), param(Var, Formula) do
				subst_in_term(Var, Entity, Formula, F2),
				remove_quantifiers_term(F2, F)
			),
			Out =.. [all, Conjunction]
		;
		throw(unregistered_domain(Type)).

erase_exists(Var, Type, Formula, Out) :-
		domain(Type, D) ->		
			% TODO: think of a way to use proper constraint programming
			(foreach(Entity, D), foreach(F, Disjunction), param(Var, Formula) do
				subst_in_term(Var, Entity, Formula, F2),
				remove_quantifiers_term(F2, F)
			),
			Out =.. [one, Disjunction]
		;
		throw(unregistered_domain(Type)).
		
		
remove_quantifiers_list([H], Tl2) :-
		remove_quantifiers_term(H, T1),
		Tl2 = [T1].
		
remove_quantifiers_list([H | Tl], Tl2) :-
		remove_quantifiers_term(H, T1),
		remove_quantifiers_list(Tl, Tl3),
		Tl2 = [T1 | Tl3].
		

remove_quantifiers_term(T, Out) :-
	
		T = forall([Var,Type],Formula),
		erase_forall(Var, Type, Formula, Out), !
		;
		T = exists([Var,Type],Formula),
		erase_exists(Var, Type, Formula, Out), !
		;
		arity(T, N),
		(N is 0 ->
			Out = T
			;
			T =.. Tl,
			remove_quantifiers_list(Tl, Tl2),
			Out =.. Tl2
		), !.
		
		


compile_constraints_list([H], Tl2, Situation) :-
		compile_constraints_term(H, T1, Situation),
		Tl2 = [T1].

compile_constraints_list([H | Tl], Tl2, Situation) :-
		compile_constraints_term(H, T1, Situation),
		compile_constraints_list(Tl, Tl3, Situation), 
		Tl2 = [T1 | Tl3].
		

gather_evaluations_list([H], Tl2, InList, OutList, Situation) :-
		gather_evaluations_term(H, T1, InList, OutList, Situation),
		Tl2 = [T1].
		
gather_evaluations_list([H | Tl], Tl2, InList, OutList, Situation) :-
		gather_evaluations_term(H, T1, InList, OutList2, Situation),
		gather_evaluations_list(Tl, Tl3, OutList2, OutList, Situation),
		Tl2 = [T1 | Tl3].
		
% OutList  

isFluent(Functor, Type) :-
	fluent(Functor, _, Type),!
	;
	derived_fluent(Functor, _, Type).

	

gather_evaluations_term(T, Out, InList, OutList, Situation):-
		is_list(T),
		gather_evaluations_list(T, Subterms, InList, OutList, Situation), 
		Out = Subterms, !
		;
		functor(T, Functor, N),	
		(N > 0 ->
			T =.. [_ | Subterms],
			(
				isFluent(Functor, _),
				% - it must be a functional fluent since we already handled relational ones in 
				%   compile_constraints_term
				% - we don't need to handle arguments of the fluent
				gather_evaluations_list(Subterms, Subterms2, InList, OutList2, Situation),
				% 1. add result variable and s0
				var(NewVar),
				append(Subterms2, [NewVar, Situation], Subterms3),
				T2 =.. [Functor | Subterms3],
				append(OutList2, [T2], OutList),
				Out = NewVar, !
				;
				% we have a function now so no s0 but handle arguments first
				gather_evaluations_list(Subterms, Subterms2, InList, OutList2, Situation),
				% add result var
				var(NewVar),
				append(Subterms2, [NewVar], Subterms3),
				T2 =.. [Functor | Subterms3],
				append(OutList2, [T2], OutList),
				Out = NewVar, !				
			)
			;
			% atom
			OutList = InList,
			Out = T
		).
		
create_constraint(Op, Subterms, Out, Situation) :-
		gather_evaluations_list(Subterms, HandledSubterms, [], InitGoals, Situation),
		T =.. [Op | HandledSubterms],
		append(InitGoals, [T], Subterms2),
		Out =.. [all, Subterms2].

		
		
		
	% idea: 
	% * gather function and functional fluent terms in X and Y. functions could also be +,-,*,etc.
	% * append fresh variable as result argument and s0 for fluents
	% * replace function/fluent term with the variable that was created in the last step.
	% * collect the restored goals in a combined list with the proper
	%   order
	% * Create output term: eval_all(fluent/function goals, 

	
	
	
compile_constraints_term(T, Out, Situation) :-
		% - When this goal is called we assume that quantifiers have been removed already
		%   this also means that we might encounter a variable that has been introduced during quantifier elimination
		% - The passed situation argument is important to handle change tests. In all cases except "change", it has to be 
		%   passed down to the recursion.
		T = ?,
		Out = _, !
		;
		var(T),
		Out = T, !
		;
		T = possible(GologProg),
		Out = possible(GologProg, Situation), !
		;		
		functor(T, Functor, N),		
		(
			% for fluents we assume that the arguments are either atoms or variables
			% boolean fluents can just be handled directly without introducing a variable
			isFluent(Functor, boolean),
			T =.. TempTl ,
			append(TempTl, [Situation], Tl),
			Out =.. Tl, !			
			;			
			% handle comparison
			
			member(Functor, [>,<,>=,=<,==, =\=, \=, 
				$>, $>=, $<, $=<, $=, $\=]),
			% we assume that there are two subterms	
			T =.. [_ | Subterms],
			create_constraint(Functor, Subterms, Out, Situation), ! 
			;
			% for all other cases we need to handle the subterms
			(N > 0 ->	
				% for a change test, create a variable to use as situation for all enclosed fluents
				T =.. [_ | Subterms],
				( 
					(
						(Functor = start, ! ; Functor = end),
						not((Subterms = [PFName], persistent_fluent(PFName, _)))
					) -> 
					compile_constraints_list(Subterms, HandledSubterms, s0),
					compile_constraints_list(Subterms, HandledSubtermsLast, slast),
					IsChangeEvent = true
					;
					compile_constraints_list(Subterms, HandledSubterms, Situation),
					IsChangeEvent = false
				),
				(
					Functor = and,
					Out =.. [all, HandledSubterms], !
					;
					Functor = or,
					Out =.. [one, HandledSubterms], !
					;
					Functor = not,
					HandledSubterms = [P],
					Out =.. [neg, P], !
					;
					Functor = implies,
					% TODO: exception if > 2 params
					HandledSubterms = [P,Q],
					Out = one([neg(P), Q]), !
					;						
					Functor = until,
					% TODO: exception if != 3 params
					HandledSubterms = [MaxTime, P,Q],
					Out =.. [until, MaxTime, P, Q], !						
					;
					% occur means action/exogenous action occured
					Functor = occur,
					HandledSubterms = [Act],
					Out =.. [occur, Act], !
					;
					Functor = start,
					(IsChangeEvent ->
						HandledSubtermsLast = [P],
						HandledSubterms = [Q],
						Out =.. [changed, P, Q, ok]
						;
						HandledSubterms = [PFName],
						Out =.. [pfswitch, PFName, ok]
					), !
					;
					Functor = end,
					(IsChangeEvent ->
						HandledSubtermsLast = [P],
						HandledSubterms = [Q],
						Out =.. [changed, P, Q, not_ok]
						;
						HandledSubterms = [PFName],
						Out =.. [pfswitch, PFName, not_ok]
					), !
					;						
					% otherwise just take what we have now
					Out =.. [Functor | HandledSubterms], !
				)
				;
				% now we basically have only numbers, true or false left
				Out = T, !
			), !
		).
	


compile_formula(FIn, FOut) :-
	compile_formula(FIn, FOut, s0).
	
compile_formula(FIn, FOut, Situation) :-
	remove_quantifiers_term(FIn,F2), 
	compile_constraints_term(F2, FOut, Situation).
	