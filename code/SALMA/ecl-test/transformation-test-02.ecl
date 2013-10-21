:- ['../ecl-src/agasmc_progression'].
:- ['../ecl-src/property_evaluator'].
:- [domaindesc].

domain(robot,D) :-
        D=[rob1,rob2,rob3].

domain(item, D) :- D=[item1, item2, item3].

init :-
	init_progression,
	set_current(xpos, [rob1], 20),
	set_current(xpos, [rob2], 30),
	set_current(xpos, [rob3], 40),
	set_current(carrying,[rob1, item1], true),
	set_current(carrying,[rob2, item2], true),
	set_current(carrying,[rob3, item3], true).


test1(T2) :- remove_quantifiers_term(forall([x,robot], xpos(x) > 10),T2).
test2(T2) :- remove_quantifiers_term(forall([x,robot], 
				exists([i,item], carrying(x,i))),T2).


test3(T2) :- remove_quantifiers_term(forall([x,robot], xpos(x) > 10),T), 
				compile_constraints_term(T, T2, s0).
				

test4(T3) :- T = forall([x,robot], 
					and(
						exists([i,item], 
							carrying(x,i)),
						xpos(x) > 10
						)
					),
					remove_quantifiers_term(T,T2), 
					compile_constraints_term(T2, T3, s0).

test5(T3) :- T = and(xpos(rob1) > 10, xpos(rob2) > 20),
				remove_quantifiers_term(T,T2), 
				compile_constraints_term(T2, T3, s0).
				
test6(N, T3) :- T = exists([x,robot], xpos(x) > N),
				remove_quantifiers_term(T,T2), 
				compile_constraints_term(T2, T3, s0).			
				
			
test7(T3) :- T = (xpos(rob1) > xpos(rob2)),
				remove_quantifiers_term(T,T2), 
				compile_constraints_term(T2, T3, s0).	

test8(T3) :- T = or(xpos(rob1) > xpos(rob2), xpos(rob1) < xpos(rob2)),
				remove_quantifiers_term(T,T2), 
				compile_constraints_term(T2, T3, s0).	
				
test9(T3) :- T = (xpos(rob2) > xpos(rob1) + 9),
				remove_quantifiers_term(T,T2), 
				compile_constraints_term(T2, T3, s0).	
				
test10(T3) :- T = occur(xpos(rob2) > xpos(rob1) + 9),
				remove_quantifiers_term(T,T2), 
				compile_constraints_term(T2, T3, s0).	
