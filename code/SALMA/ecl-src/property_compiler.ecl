% constraint_op(op, complement) 
constraint_op(>, =<).
constraint_op(=<, >).

constraint_op(<, >=).
constraint_op(>=, <).

constraint_op(==, \==).
constraint_op(\==, ==).

constraint_op(=:=, =\=).
constraint_op(=\=, =:=).

constraint_op(\=, =).
constraint_op(=, \=).

constraint_op($>, $=<).
constraint_op($=<, $>).

constraint_op($<, $>=).   
constraint_op($>=, $<).

constraint_op($=, $\=).
constraint_op($\=, $=).

reifiable_op(Op) :- 
	member(Op, [>, =<, <, >=, =:=, =\=, $>, $=<, $<, $>=, $=, $\=]).

erase_forall(Var, Type, Formula, Out) :-
		domain(Type, D) -> 
			(foreach(Entity, D), foreach(F, Conjunction), param(Var, Formula) do
				subst_in_term(Var, Entity, Formula, F2),
				remove_quantifiers_term(F2, F)
			),
			Out =.. [and | Conjunction]
		;
		throw(unregistered_domain(Type)).

erase_exists(Var, Type, Formula, Out) :-
		domain(Type, D) ->		
			% TODO: think of a way to use proper constraint programming
			(foreach(Entity, D), foreach(F, Disjunction), param(Var, Formula) do
				subst_in_term(Var, Entity, Formula, F2),
				remove_quantifiers_term(F2, F)
			),
			Out =.. [or | Disjunction]
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
	
		(T = forall([Var,Type],Formula) ; T = forall(Var : Type, Formula)),
		erase_forall(Var, Type, Formula, Out), !
		;
		(T = exists([Var,Type],Formula) ; T = exists(Var : Type, Formula)),
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
		Tl2 = [T1], !.
		
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
		var(T),
		OutList = InList,
		Out = T, !
		;
		functor(T, Functor, N),	
		(N > 0 ->
			T =.. [_ | Subterms],
			gather_evaluations_list(Subterms, Subterms2, InList, OutList2, Situation)
			;
			Subterms2 = [],
			OutList2 = InList
		),
		(isFluent(Functor, _) ->
			% - it must be a functional fluent since we already handled relational ones in 
			%   compile_constraints_term
			% 1. add result variable and s0
			var(NewVar),
			append(Subterms2, [NewVar, Situation], Subterms3),
			T2 =.. [Functor | Subterms3],
			append(OutList2, [T2], OutList),
			Out = NewVar, !
			;
			% we might have a function, a constant, or an atom
			% if arity > 0 then it can't be an atom
			((N > 0, ! ; constant(Functor, _, _)) ->		
				var(NewVar),
				append(Subterms2, [NewVar], Subterms3),
				T2 =.. [Functor | Subterms3],
				append(OutList2, [T2], OutList),
				Out = NewVar
				;
				% it has to be an atom
				OutList = InList,
				Out = T
			)	
		).
		
create_constraint(Op, Subterms, Out, Situation) :-
		gather_evaluations_list(Subterms, HandledSubterms, [], InitGoals, Situation),
		T =.. [Op | HandledSubterms],
		append(InitGoals, [T], Subterms2),
		Out = c_(Subterms2).

		
		
		
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
		T = let(Var : Def, Body),		
		var(NewVar),
		create_constraint(=, [NewVar, Def], NewDef, Situation),
		% FreshVar should be bound now
		compile_constraints_term(Body, NewBody, Situation),
		Out = let(Var : NewVar, NewDef, NewBody), !
		;
		T = match(VarSpec : Def, Body),
		(is_list(VarSpec) -> Vars = VarSpec ; Vars = [VarSpec]),
		compile_constraints_term(Def, Def2, Situation),
		compile_constraints_term(Body, NewBody, Situation),
		(foreach(Var, Vars), foreach(NV, NewVarSpecs), fromto(Def2, InDef, OutDef, NewDef) do
				var(NewVar),
				subst_in_term(Var, NewVar, InDef, OutDef),
				NV = Var : NewVar
		),		
		Out = match(NewVarSpecs, NewDef, NewBody), !
		;
		functor(T, Functor, N),		
		(
			% handle comparison
			constraint_op(Functor, _),
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
				)
				;
				HandledSubterms = []
			),
			(
				isFluent(Functor, boolean),
				append(HandledSubterms, [Situation], HandledSubterms2),
				Out =.. [Functor | HandledSubterms2], !
				;				
				Functor = and,
				Out =.. [all, HandledSubterms], !
				;
				Functor = or,
				Out =.. [one, HandledSubterms], !
				;
				Functor = not,
				HandledSubterms = [P],
				Out =.. [not2, P], !
				;						
				Functor = until,
				% TODO: exception if != 3 params
				HandledSubterms = [MaxTime, P,Q],
				Out =.. [until, MaxTime, P, Q], !						
				;
				% occur means action/exogenous action occurred
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
		
		).
	


compile_formula(FIn, FOut) :-
	compile_formula(FIn, FOut, s0).
	
compile_formula(FIn, FOut, Situation) :-
	remove_quantifiers_term(FIn,F2), 
	simplify(F2, F3),
	compile_constraints_term(F3, FOut, Situation).
	


	
simplify_list(FInList, FOutList) :-
	(foreach(FIn, FInList), foreach(FOut, FOutList) do
		simplify(FIn, FOut)
	).
	
simplify(FIn, FOut) :-
	FIn = implies(P, Q), !,
		simplify(P, P2),
		simplify(Q, Q2),
		FOut = or(not(P2), Q2)
		;
	FIn = complement(P), !,
		simplify(P, P2),
		build_complement(P2, FOut)
		;
	FIn =.. [Functor | Subterms], !,
		simplify_list(Subterms, Subterms2),
		FOut =.. [Functor | Subterms2]
		;
	% atom
		FOut = FIn.

build_complements(PList, ComplList) :-
	(foreach(P, PList), foreach(C, ComplList) do
		build_complement(P, C)
	).
		
	
build_complement(P, Compl) :-
	P =.. [Functor | Subterms],
	(constraint_op(Functor, ComplFunctor),
		Compl =.. [ComplFunctor | Subterms], !
		;
	member(Functor, [not, neg]),
		Subterms = [Compl], !
		;
	Functor = or,
		build_complements(Subterms, ComplList),
		Compl =.. [and | ComplList], !
		;
	Functor = and,
		build_complements(Subterms, ComplList),
		Compl =.. [or | ComplList], !
		;
	Compl = not(P)
	), !
	;
	Compl = not(P).
	
	
	
		
		