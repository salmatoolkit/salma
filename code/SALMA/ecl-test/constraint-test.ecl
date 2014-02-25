:- ['../ecl-src/agasmc'].
:- dynamic xpos/3, xpos2/3.
:- local store(vars).
:- dynamic constrained_var/2.
sorts([robot]).

fluent(xpos, [r:robot], integer).
fluent(xpos2, [r:robot], integer).

primitive_action(move_right,[r:robot]).
primitive_action(move_left, [r:robot]).

poss(move_right(_), _) :- true.
poss(move_left(_), _) :-true.

intro_var(Name, Range, Confidence, Sit, Var) :-
	store_get(vars, vk(Name, Sit), VarDesc), 
	VarDesc = vd(Var, _, _),
	!
	;
	Var::Range,
	store_set(vars, vk(Name, Sit), vd(Var, Range, Confidence)),
	!.

domain(robot,D) :-
        D=[rob1,rob2].

constrained_var(deltax2, uniform(1,5)).

xpos2(R, X, do2(A,S)) :-
	xpos(R, OldX, S),
	intro_var(delta, [1..5], 0.95, S, Delta),
	(A = move_right(R), !,
		X $= OldX + Delta
		;
	A = move_left(R), !,
		X $= OldX - Delta
		;
	X $=OldX).

xpos(Rob, Pos, do2(A,S)) :- 
	xpos(Rob, POld, S),
	(
	 A = move_right(Rob), 
	 Pos is POld + 1, !
	;
	 A = move_left(Rob),
	 Pos is POld - 1, !
	;
	 Pos is POld, !
	).
	
proc(moveToX, [r:robot, targetX : integer],
	while(xpos(r) =\= targetX, 
		if(xpos(r) < targetX, 
			move_right(r),
			move_left(r)
		)
	) 
).
	
proc(moveToX2, [r:robot, targetX : integer],
	while(not(xpos2(r) $>= targetX), 
		move_right(r)
	) 
).

init :-
	init_agasmc,
	set_current(xpos, [rob1], 10), 
	set_current(xpos, [rob2], 20),
	set_current(xpos2, [rob1], 10), 
	set_current(xpos2, [rob2], 20),
	set_current(time, [], 0).

%test1(s) :- do2(if( xpos(rob1) < 24, move_right(rob1), move_left(rob1) ),s0,s).

%test2(S) :- do2(while(xpos(rob1) < 24, move_right(rob1),s0,S).
test3(S) :- do2(while(xpos(rob1) < 24, move_right(rob1)),s0,S).
	
test4(X, S) :- store_erase(vars), do2(moveToX2(rob1, X), s0, S).

get_prob_bound(VarName, Range, P) :-
	constrained_var(VarName, Distrib),
	Distrib = uniform(L,U),
	
	
get_range(VarName, ConfidenceLevel, Range) :-
	constrained_var(VarName, Distrib),
	Distrib = uniform(L,U),
	Mid $= L + (U-L)/2,
	NewSize $= (U-L)*ConfidenceLevel,
	NewL $= Mid - NewSize/2,
	NewU $= Mid + NewSize/2,
	Range = [L..U].
	

calc_p(P) :-
	stored_keys_and_values(vars, L),
	(foreach(V, L), fromto(1, P0, P1, P) do
		V = vk(_, _) - vd(_, _, Confidence),
		
		
print_vars :-
	stored_keys_and_values(vars, L),
	(foreach(V, L) do
		V = vk(Name, Sit) - vd(Var, _, Confidence),
		get_domain(Var, Range), 
		printf("%10s @ %f [%w] : %w\n",[Name, Confidence, Sit, Range])
	).
	
		



