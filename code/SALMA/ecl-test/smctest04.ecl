:- ['../ecl-src/agasmc_progression'].
:- ['../ecl-src/property_compiler'].
:- ['../ecl-src/property_evaluator'].
:- [domaindesc2].


domain(robot,D) :-
        D=[rob1,rob2].

domain(item, D) :- D=[item1, item2].

init :-
	init_progression,
	init_smc,
	set_current(xpos, [rob1], 20),
	set_current(xpos, [rob2], 30),
	set_current(ypos, [rob1], 20),
	set_current(ypos, [rob2], 30),
	
	set_current(velocity_x, [rob1], 0),
	set_current(velocity_x, [rob2], 0),
	
	set_current(velocity_y, [rob1], 0),
	set_current(velocity_y, [rob2], 0),

	set_current(carrying,[rob1, item1], false),
	set_current(carrying,[rob2, item2], false),
	set_current(time, [], 0).


% eval_op(Operator, T1, T2, v(Result)) :-
		% T  =.. [Operator, T1, T2, Result],
		% call(T).
		
% renew_vars_list([H | Tl], Out) :-
		
% renew_vars_term(T, Out) :-
		% (T = v(X) ->
			% var(NewVar),
			% Out = v(NewVar),
				
		% T =.. L,
		
		




% should register x=v in hash when v(x,v) is evaluated.
				
% test2(T2) :- T = all([
					%rob1
					% implies(
						% eval_fluent(
					% one([
						%item1
						
						
						% eval_fluent(carrying,v(r,rob1),v(i,item1))
					


					
% Bsp.:  r = rob1

test1(Result, L) :- F = all([
				xpos(rob1, P1, s0), P1 > 20,
				until(
					all([xpos(rob1, P2, s0), P2 > 20]),
					all([ypos(rob1, Y, s0), Y > 20])
				)]),
		evaluate_toplevel(F, Result),
		stored_keys_and_values(scheduled_goals, L).

		
		
		
test(F, Result, L) :- T = forall([r,robot],
						implies(
							xpos(r) > 50,
							exists([i,item], 
								until(
									carrying(r, i), 
									% xpos(r) > xpos(r)@start + 100)
									xpos(r) > 100
								)
							)
						)
				),
			remove_quantifiers_term(T,T2), 
			compile_constraints_term(T2, F, s0),
			evaluate_toplevel(F, Result),
			stored_keys_and_values(scheduled_goals, L).
			
% L = [f(0) - 
	% all([
		% one([
			% one([
				% until(
					% carrying(rob1, item1, s0), 
					% all([xpos(rob1, _3207, s0), _3207 > 100])
				% )
			% ])
		% ])
	% ])
% ]


test_nested(F, Result, L) :- 
				T = forall([r,robot],
					implies(
						xpos(r) > 50,
						until(
							xpos(r) > 50,
							until(
								xpos(r) > 100,
								ypos(r) > 100
							)
						)
					)
				),
				remove_quantifiers_term(T,T2), 
				compile_constraints_term(T2, F, s0),
				evaluate_toplevel(F, Result),
				stored_keys_and_values(scheduled_goals, L).


