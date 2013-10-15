%:- use_module('../ecl-src/agasmc').
:- ['../ecl-src/agasmc'].
:- [domaindesc2].

assertEquals(Condition, Expected, Actual, Message) :-
	call(Condition),
		Expected \= Actual,
		throw(test_failed(notEqual(Expected,Actual), Message))
	;
	true.
		
	


domain(robot,D) :-
        D=[rob1,rob2,rob3].

domain(item, D) :- D=[item1, item2, item3].

init :-
	set_current(xpos, [rob1], 20),
	set_current(xpos, [rob2], 30),
	set_current(xpos, [rob3], 40),
	set_current(ypos, [rob1], 20),
	set_current(ypos, [rob2], 30),
	set_current(ypos, [rob3], 40),
	set_current(velocity_x, [rob1], 0),
	set_current(velocity_x, [rob2], 0),
	set_current(velocity_x, [rob3], 0),
	set_current(velocity_y, [rob1], 0),
	set_current(velocity_y, [rob2], 0),
	set_current(velocity_y, [rob3], 0),
	set_current(carrying,[rob1, item1], true),
	set_current(carrying,[rob2, item2], true),
	set_current(time, [], 0).


	
	
test_simple_progression :-
			init_agasmc,
			init,
			progress([move_right(rob1)]),
			get_current(velocity_x,[rob1],V),
			assertEquals(true, 1, V, v_wrong),
			progress([tick]),
			get_current(time,[],T1),
			assertEquals(true, 1, T1, t_wrong),
			get_current(xpos,[rob1],X1), 
			assertEquals(true, 21, X1, x_wrong),
			progress([tick]),
			get_current(time,[],T2), 
			assertEquals(true, 2, T2, t_wrong), 			
			get_current(xpos,[rob1],X2), 
			assertEquals(true, 22, X2, x_wrong).
			
			
			
			


test_until_fail :-
			init_agasmc,
			init,
			set_current(velocity_x, [rob1], 1),
			T = until(5, xpos(rob1) > 19, xpos(rob1) > 40),
			register_property(f, T, _),
			(count(I,0,5) do	
				evaluation_step(R, R1, R2),
				printf("%d : %w %w %w\n",[I,R,R1,R2]),
				assertEquals(true, nondet, R, r_wrong)
			),
			evaluation_step(R, _, _),
			assertEquals(true, not_ok, R, r_wrong).
			
test_until_ok :-
			init_agasmc,
			init,
			set_current(velocity_x, [rob1], 1),
			T = until(12, xpos(rob1) > 19, xpos(rob1) > 25),
			register_property(f, T, _),
			(count(I,0,5) do
				
				evaluation_step(R, R1, R2),
				printf("%d : %w %w %w\n",[I,R,R1,R2]),
				assertEquals(true, nondet, R, r_wrong)
			),
			evaluation_step(R, _, _),
			assertEquals(true, ok, R, r_wrong).	
			
test_nested_until(MaxTimeOuter, MaxTimeP, XTarget, PeriodLength, Repetitions):-		
			init_agasmc,
			init,
			set_current(velocity_x, [rob1], 1),		
			set_current(carrying,[rob1, item1], false),
			T = until(MaxTimeOuter, 
						implies(velocity_y(rob1) > 0, 
								until(MaxTimeP, true, velocity_y(rob1) == 0)
								), 
						xpos(rob1) >= XTarget),
			register_property(f, T, _),
			set_current(velocity_y, [rob1], 1),
			(count(I,0,Repetitions), param(PeriodLength, XTarget) do
				(I mod PeriodLength $= 0 -> set_current(velocity_y, [rob1], 0) ; true),
				evaluation_step(R, _, _),
				xpos(rob1, X, s0),
				velocity_y(rob1, Vy, s0),
				time(Time, s0),
				printf("time = %d, vy = %d, x = %d, R = %w\n",[Time,Vy,X, R]),
				set_current(velocity_y, [rob1], 1),
				(X < XTarget, R \= nondet,
					throw(test_failed(notEqual(R,nondet), error1))
				;
				X >= XTarget, Vy $= 0, R \= ok,
					throw(test_failed(notEqual(R,ok)), error2)
				;
				X >= XTarget, Vy > 0, R \= nondet,
					throw(test_failed(notEqual(R,nondet)), error3)
				;
				true
				)		
			).
			
test_nested_until_OK :-
		test_nested_until(20, 4,40,4,40).
test_nested_until_too_less_outer_time :-
		test_nested_until(12, 4,40,4,40).


estep :-
		evaluation_step(R1, R2, R3),		
		time(T, s0),
		xpos(rob1, X, s0),
		printf("%5d %5d %w\nR2: %w\nR3: %w\n\n",[T, X, R1, R2, R3]).
		


			
test_until_with_change_event(LeftBound, RightBound, MaxTime, Repetitions) :-
			init_agasmc,
			init,
			set_current(velocity_x, [rob1], 1),
			F = implies(
					start(xpos(rob1) > LeftBound),
					until(MaxTime,
						true, 
						xpos(rob1) == RightBound
					)
				),
			register_property(f, F, _),
			(count(I,0,Repetitions), param(LeftBound, RightBound) do
				xpos(rob1,X,s0),
				printf("i = %d, x = %d",[I, X]),
				evaluation_step(R,_,_),
				printf(" --> %w\n",[R]),
				
				(X =< LeftBound, R \= ok,
					throw(test_failed(notEqual(R,ok), error1))
				;
				X > LeftBound, X < RightBound, R \= nondet,
					write("ok"),
					throw(test_failed(notEqual(R,nondet), error2))
				;
				X >= 30, R \= ok,
					throw(test_failed(notEqual(R,ok), error3))
				;
				true
				)		
			).
			
			
test_FO_until_with_change_event(LeftBound, MaxTime, Distance, XRob3) :-
			init_agasmc,
			init,
			set_current(velocity_x, [rob1], 1),
			set_current(velocity_x, [rob2], 2),
			set_current(velocity_x, [rob3], 0),
			set_current(xpos,[rob1],0),
			set_current(xpos,[rob2],0),
			set_current(xpos,[rob3],XRob3),
			F = forall([r,robot], 
					implies(
						start(xpos(r) > LeftBound),
						until(MaxTime,
							true, 
							exists([r2,robot],
								xpos(r2) == xpos(r) + Distance
							)
						)
					)
				),
			register_property(f, F, _),
			(count(I, 0, 50), param(LeftBound, Distance) do
				time(T,s0),
				xpos(rob1,X1,s0),
				xpos(rob2,X2,s0),
				xpos(rob3,X3,s0),
				printf("%2d: t = %3d, x1 = %3d, x2 = %3d, x3 = %3d",[I, T, X1, X2, X3]),
				evaluation_step(R,_,_),
				printf(" --> %w\n",[R]),
				(X2 =< LeftBound, R \= ok,
					throw(test_failed(notEqual(R,ok), error1))
				;
				X2 > LeftBound, X2 < X1 + Distance, R \= nondet,
					throw(test_failed(notEqual(R,nondet), error2))
				;
				X2 >= X1 + Distance, R \= ok,
					throw(test_failed(notEqual(R,ok), error3))
				;
				true
				)
			).
				
test_FO_until_with_change_event_OK :-
	test_FO_until_with_change_event(4, 10, 14, 38).

test_FO_until_with_change_event_Fail2 :-
	test_FO_until_with_change_event(4, 10, 100, 38).

	
test_FO_until_with_persistent_fluent(F2) :-
			init_agasmc,
			init,
			set_current(velocity_x, [rob1], 1),
			F =	implies(
					start(all_carry_something),
					until(10,
						true, 
						exists([r2,robot],
							xpos(r2) == 50
						)
					)
				),
			register_property(f, F, F2),
			(count(I,0,20) do
				time(T,s0),
				xpos(rob1,X1,s0),
				query_persistent_fluent(all_carry_something, Curr, LastChanged),
				printf("%2d: t = %3d, x1 = %3d, all_carry_something=%w : %d ",
						[I, T, X1, Curr, LastChanged]),
				evaluation_step(R,_,_),
				printf(" --> %w\n",[R]),
				(R \= ok ->
					throw(test_failed(notEqual(R,ok), error1))
					;
					true
				)
			),
			printf("\n\nGRAB!\n\n",[]),
			set_current(carrying, [rob3, item3], true),
			(count(I,0,20) do
				time(T,s0),
				xpos(rob1,X1,s0),
				query_persistent_fluent(all_carry_something, Curr, LastChanged),
				printf("%2d: t = %3d, x1 = %3d, all_carry_something=%w : %d ",
						[I, T, X1, Curr, LastChanged]),
				evaluation_step(R,_,_),
				printf(" --> %w\n",[R]),
				(X1 < 50, R \= nondet,
					throw(test_failed(notEqual(R,nondet), error2))
					;
				X1 >= 50, R \= ok,
					throw(test_failed(notEqual(R,ok), error3))
					;
					true
				)
			).
				
				
			

test_FO_until_with_action_occurrence(F2) :-
			init_agasmc,
			init,
			set_current(velocity_x, [rob1], 1),
			F = forall([r,robot], 
					implies(
						occur(grab(r, ?)),
						until(10,
							true, 
							xpos(r) > 50
						)
					)
					
				),
			register_property(f, F, F2).			

test_FO_until_with_action_occurrence2(F2) :-
			init_agasmc,
			init,
			set_current(velocity_x, [rob1], 1),
			F = forall([r,robot], 
					forall([i,item],
						implies(							
							occur(grab(r, i)),
							until(10,
								carrying(r,i), 
								% TODO: make evaluate_until exclude last timepoint
								occur(drop(r,i))
							)
						)
					)
				),
			register_property(f, F, F2).	