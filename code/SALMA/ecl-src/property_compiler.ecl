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
        /* $D$ */ 
        domain(Type, D) ->
            /* $D$ */ 
            (foreach(Entity, D), foreach(F, Conjunction), param(Var, Formula) do
                /* $D$ */ 
                subst_in_term(Var, Entity, Formula, F2),
                remove_quantifiers_term(F2, F)
            ),
            Out =.. [and | Conjunction]
        ;
            /* $D(IGNORED)$ */ 
            throw(unregistered_domain(Type)).

erase_exists(Var, Type, Formula, Out) :-
        /* $D$ */ 
        domain(Type, D) ->		
            /* $D$ */ 
            (foreach(Entity, D), foreach(F, Disjunction), param(Var, Formula) do
                /* $D$ */ 
                subst_in_term(Var, Entity, Formula, F2),
                remove_quantifiers_term(F2, F)
            ),
            Out =.. [or | Disjunction]
        ;
            /* $D(IGNORED)$ */ 
            throw(unregistered_domain(Type)).

		
remove_quantifiers_list([H], Tl2) :-
        /* $D$ */ 
        remove_quantifiers_term(H, T1),
        Tl2 = [T1].

remove_quantifiers_list([H | Tl], Tl2) :-
        /* $D$ */ 
        remove_quantifiers_term(H, T1),
        remove_quantifiers_list(Tl, Tl3),
        Tl2 = [T1 | Tl3].
		

remove_quantifiers_term(T, Out) :-
        (T = forall([Var,Type],Formula) /* $DS$ */ ; T = forall(Var : Type, Formula) /* $DS$ */ ),
        /* $D$ */ 
        erase_forall(Var, Type, Formula, Out), !
        ;
        (T = exists([Var,Type],Formula) /* $DS$ */ ; T = exists(Var : Type, Formula) /* $DS$ */ ),
        /* $D$ */ 
        erase_exists(Var, Type, Formula, Out), !
        ;
        /* $D$ */ 
        arity(T, N),
        (N is 0 ->
            /* $D$ */ 
            Out = T
        ;
            /* $D$ */ 
            T =.. Tl,
            remove_quantifiers_list(Tl, Tl2),
            Out =.. Tl2
        ), !.
			


compile_constraints_list([H], Tl2, Situation) :-
        /* $D$ */ 
        compile_constraints_term(H, T1, Situation),
        Tl2 = [T1].

compile_constraints_list([H | Tl], Tl2, Situation) :-
        /* $D$ */ 
        compile_constraints_term(H, T1, Situation),
        compile_constraints_list(Tl, Tl3, Situation), 
        Tl2 = [T1 | Tl3].
		

gather_evaluations_list([H], Tl2, InList, OutList, Situation) :-
        /* $D$ */ 
        gather_evaluations_term(H, T1, InList, OutList, Situation),
        Tl2 = [T1], !.
		
gather_evaluations_list([H | Tl], Tl2, InList, OutList, Situation) :-
        /* $D$ */ 
        gather_evaluations_term(H, T1, InList, OutList2, Situation),
        gather_evaluations_list(Tl, Tl3, OutList2, OutList, Situation),
        Tl2 = [T1 | Tl3].
		
% OutList  

isFluent(Functor, Type) :-
        /* $D$ */ 
        fluent(Functor, _, Type), /* $D$ */ !
	;
        /* $D$ */ 
	derived_fluent(Functor, _, Type) /* $DS$ */ .

	

gather_evaluations_term(T, Out, InList, OutList, Situation):-
        is_list(T),
        /* $D$ */ 
        gather_evaluations_list(T, Subterms, InList, OutList, Situation), 
        Out = Subterms, !
        ;
        var(T),
        /* $D$ */ 
        OutList = InList,
        Out = T, !
        ;
        functor(T, Functor, N),	
        /* $D$ */ 
        (N > 0 ->
            /* $D$ */ 
            T =.. [_ | Subterms],
            gather_evaluations_list(Subterms, Subterms2, InList, OutList2, Situation)
        ;
            /* $D$ */ 
            Subterms2 = [],
            OutList2 = InList
        ),
        
        (isFluent(Functor, _) ->
            /* $D$ */ 
            
            % - it must be a functional fluent since we already handled relational ones in 
            %   compile_constraints_term
            % 1. add result variable and s0
            var(NewVar),
            append(Subterms2, [NewVar, Situation], Subterms3),
            T2 =.. [Functor | Subterms3],
            append(OutList2, [T2], OutList),
            Out = NewVar, !
        ;
            /* $D$ */ 
            % we might have a function, a constant, or an atom
            % if arity > 0 then it can't be an atom
            ((N > 0, /* $D$ */ ! ; 
              % we could have a predicate that has only on one "return" argument
              atom(Functor), is_predicate(Functor/1), /* $D$ */ !
             ; constant(Functor, _, _) /* $DS$ */ ) ->
                /* $D$ */ 
                var(NewVar),
                append(Subterms2, [NewVar], Subterms3),
                T2 =.. [Functor | Subterms3],
                append(OutList2, [T2], OutList),
                Out = NewVar
            ;
                /* $D$ */ 
                % it has to be an atom
                OutList = InList,
                Out = T
            )	
        ).

create_constraint(Op, Subterms, Out, Situation) :-
        /* $D$ */ 
        gather_evaluations_list(Subterms, HandledSubterms, [], InitGoals, Situation),
        T =.. [Op | HandledSubterms],
        append(InitGoals, [T], Subterms2),
        Out = c_(Subterms2).

create_relational_fluent_use(FluentName, Subterms, Out, Situation) :-
        /* $D$ */ 
        gather_evaluations_list(Subterms, HandledSubterms, [], InitGoals, Situation),
        append(HandledSubterms, [Situation], HandledSubterms2),
        T =.. [FluentName | HandledSubterms2],
        append(InitGoals, [T], Subterms2),
        Out = c_(Subterms2).

create_predicate_use(PredicateName, Subterms, Out, Situation) :-
        /* $D$ */ 
        gather_evaluations_list(Subterms, HandledSubterms, [], InitGoals, Situation),
        T =.. [PredicateName | HandledSubterms],
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
        T = ?, /* $D$ */ 
        Out = _, !
        ;
        var(T), /* $D$ */ 
        Out = T, !
        ;
        T = possible(GologProg), /* $D$ */ 
        Out = possible(GologProg, Situation), !	
        ;		
        T = let(Var : Def, Body), /* $D$ */ 		
        var(NewVar),
        create_constraint(=, [NewVar, Def], NewDef, Situation),
        % FreshVar should be bound now
        compile_constraints_term(Body, NewBody, Situation),
        Out = let(Var : NewVar, NewDef, NewBody), !
        ;
        T = match(VarSpec : Def, Body), /* $D$ */ 
        (is_list(VarSpec) -> Vars = VarSpec ; Vars = [VarSpec]),
        compile_constraints_term(Def, Def2, Situation),
        compile_constraints_term(Body, NewBody, Situation),
        (foreach(Var, Vars), foreach(NV, NewVarSpecs), fromto(Def2, InDef, OutDef, NewDef) do
            /* $D$ */ 
            var(NewVar),
            subst_in_term(Var, NewVar, InDef, OutDef),
            NV = Var : NewVar
        ),		
        Out = match(NewVarSpecs, NewDef, NewBody), !
        ;
        /* $D$ */ 
        functor(T, Functor, N),		
        (
            % handle comparison
            constraint_op(Functor, _), /* $D$ */ 
            % we assume that there are two subterms	
            T =.. [_ | Subterms],
            create_constraint(Functor, Subterms, Out, Situation), ! 
        ;
            atom(Functor), isFluent(Functor, boolean), /* $D$ */ 
            T =.. [_ | Subterms],
            create_relational_fluent_use(Functor, Subterms, Out, Situation),	!
        ;
            atom(Functor), is_predicate(Functor/N), not member(Functor, [and, or, not]), /* $D$ */ 
            T =.. [_ | Subterms],
            create_predicate_use(Functor, Subterms, Out, Situation), !
        ;			
            % for all other cases we need to handle the subterms
            /* $D$ */ 
            (N > 0 ->	
                /* $D$ */ 
                % for a change test, create a variable to use as situation for all enclosed fluents
                T =.. [_ | Subterms],
                ( 
                    ((Functor = start, /* $D$ */ ! ; Functor = end /* $DS$ */ ),
                     not((Subterms = [PFName], persistent_fluent(PFName, _)))
                    ) -> 
                    /* $D$ */ 
                    compile_constraints_list(Subterms, HandledSubterms, s0),
                    compile_constraints_list(Subterms, HandledSubtermsLast, slast),
                    IsChangeEvent = true
                ;
                    /* $D$ */ 
                    compile_constraints_list(Subterms, HandledSubterms, Situation),
                    IsChangeEvent = false
                )
            ;
                /* $D$ */ 
                HandledSubterms = []
            ),
            (
                Functor = and, /* $D$ */ 
                Out =.. [all, HandledSubterms], !
            ;
                Functor = or, /* $D$ */ 
                Out =.. [one, HandledSubterms], !
            ;
                Functor = not, /* $D$ */ 
                HandledSubterms = [P],
                Out =.. [not2, P], !
            ;						
                Functor = until, /* $D$ */ 
                % TODO: exception if != 3 params
                HandledSubterms = [MaxTime, P,Q],
                Out =.. [until, MaxTime, P, Q], !						
            ;
                % occur means action/exogenous action occurred
                Functor = occur, /* $D$ */ 
                HandledSubterms = [Act],
                Out =.. [occur, Act], !
            ;
                Functor = start, /* $D$ */ 
                (IsChangeEvent ->
                    /* $D$ */ 
                    HandledSubtermsLast = [P],
                    HandledSubterms = [Q],
                    Out =.. [changed, P, Q, ok]
                ;
                    /* $D$ */ 
                    HandledSubterms = [PFName],
                    Out =.. [pfswitch, PFName, ok]
                ), !
            ;
                Functor = end, /* $D$ */ 
                (IsChangeEvent ->
                    /* $D$ */ 
                    HandledSubtermsLast = [P],
                    HandledSubterms = [Q],
                    Out =.. [changed, P, Q, not_ok]
                ;
                    /* $D$ */ 
                    HandledSubterms = [PFName],
                    Out =.. [pfswitch, PFName, not_ok]
                ), !
            ;						
                % otherwise just take what we have now
                /* $D$ */ 
                Out =.. [Functor | HandledSubterms], !
            )
        
        ).



compile_formula(FIn, FOut) :-
	/* $D$ */ 
        compile_formula(FIn, FOut, s0).
	
compile_formula(FIn, FOut, Situation) :-
	/* $D$ */ 
        remove_quantifiers_term(FIn,F2), 
	simplify(F2, F3),
	compile_constraints_term(F3, FOut, Situation).
	


	
simplify_list(FInList, FOutList) :-
        /* $D$ */ 
        (foreach(FIn, FInList), foreach(FOut, FOutList) do
            /* $D$ */ 
            simplify(FIn, FOut)
        ).
	
simplify(FIn, FOut) :-
        FIn = implies(P, Q), !, /* $D$ */ 
        simplify(P, P2),
        simplify(Q, Q2),
        FOut = or(not(P2), Q2)
        ;
        FIn = complement(P), !, /* $D$ */ 
        simplify(P, P2),
        build_complement(P2, FOut)
        ;
        FIn =.. [Functor | Subterms], !, /* $D$ */ 
        simplify_list(Subterms, Subterms2),
        FOut =.. [Functor | Subterms2]
        ;
        % atom
        FOut = FIn.

build_complements(PList, ComplList) :-
        /* $D$ */ 
        (foreach(P, PList), foreach(C, ComplList) do
            /* $D$ */ 
            build_complement(P, C)
        ).

	
build_complement(P, Compl) :-
        /* $D$ */ 
        P =.. [Functor | Subterms],
        (constraint_op(Functor, ComplFunctor),
         Compl =.. [ComplFunctor | Subterms], !
        ;
         member(Functor, [not, neg]),
         /* $D$ */ 
         Subterms = [Compl], !
        ;
         Functor = or,
         /* $D$ */ 
         build_complements(Subterms, ComplList),
         Compl =.. [and | ComplList], !
        ;
         Functor = and,
         /* $D$ */ 
         build_complements(Subterms, ComplList),
         Compl =.. [or | ComplList], !
        ; 
         /* $D$ */ 
         Compl = not(P)
        ), !
        ;
        /* $D$ */ 
	Compl = not(P).
	
	
	
		
		