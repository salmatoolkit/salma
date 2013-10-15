:- dynamic time/2, velocity_x/3, velocity_y/3, xpos/3, ypos/3, carrying/3.
:- lib(lists).

% action declaration
primitive_action(tick,[]).

primitive_action(send, [robot, channel, term]).
% given term for receive is a pattern that is matched
primitive_action(receive, [robot, channel]).

primitive_action(move_down,[robot]).
primitive_action(move_up,[robot]).
primitive_action(move_left,[robot]).
primitive_action(move_right,[robot]).

primitive_action(grab,[robot,item]).
primitive_action(drop,[robot,item]).

% fluent declaration: fluent_name, [arg_domains], value_domain

fluent(time,[], integer).

fluent(xpos, [robot], integer).
fluent(ypos, [robot], integer).
fluent(velocity_x, [robot], integer).
fluent(velocity_y, [robot], integer).

fluent(carrying, [robot, item], boolean).

fluent(global_msg_queue, [], termlist).
fluent(msg_in_buf, [robot], term).


% successor state axioms

persistent_fluent(someone_out_of_range, 
					exists([r,robot],
								xpos(r) > 50)
					).

persistent_fluent(all_carry_something, 
						forall([r,robot],
							exists([i, item],
								carrying(r,i)
							)
						)
					).

xpos(Rob, Pos, do2(A,S)):-
        xpos(Rob, POld, S),
        (
            A = tick -> 
            velocity_x(Rob, Vx, S),
            Pos is POld + Vx  
        ;
            Pos is POld
        ).
xpos(Rob, Pos, s0) :- get_current(xpos, [Rob], Pos).
xpos(Rob, Pos, slast) :- get_last(xpos, [Rob], Pos).

ypos(Rob, Pos, do2(A,S)):-
        ypos(Rob, POld, S),
        (
            A = tick -> 
            velocity_y(Rob, Vy, S),
            Pos is POld + Vy 
        ;
            Pos is POld
        ).

		


ypos(Rob, Pos, s0) :- get_current(ypos, [Rob], Pos).
ypos(Rob, Pos, slast) :- get_last(ypos, [Rob], Pos).

carrying(Rob, Item, do2(A,S)) :-
    A = grab(Rob, Item) ;
    (not A = drop(Rob, Item), carrying(Rob, Item, S)).
	
carrying(Rob, Item, s0) :- 
    get_current(carrying, [Rob, Item], X), X = true.

carrying(Rob, Item, slast) :- 
    get_last(carrying, [Rob, Item], X), X = true.

% very simple movement model: robots can only go straight left, right, up, down
velocity_x(Rob, Vx, do2(A,S)) :-
		A = move_left(Rob),
		Vx is -1, !
		;
		A = move_right(Rob),
		Vx is 1, !
		;
		velocity_x(Rob, Vx, S),!.
velocity_x(Rob, Vx, s0) :- get_current(velocity_x, [Rob], Vx).
velocity_x(Rob, Vx, slast) :- get_last(velocity_x, [Rob], Vx).

velocity_y(Rob, Vy, do2(A,S)) :-
		A = move_up(Rob),
		Vy is -1, !
		;
		A = move_down(Rob),
		Vy is 1, !
		;
		velocity_y(Rob, Vy, S),!.		
		
velocity_y(Rob, Vy, s0) :- get_current(velocity_y, [Rob], Vy).		
velocity_y(Rob, Vy, slast) :- get_last(velocity_y, [Rob], Vy).

time(T,do2(A,S)) :-
		time(TOld, S),
		(A = tick -> 
			T is TOld + 1
		;
			T  is TOld
		).
			
time(T, s0) :- get_current(time, [], T).		
time(T, slast) :- get_last(time, [], T).			
   
  
    
global_msg_queue(Q, do2(A,S)) :-
		global_msg_queue(OldQueue, S),
		(A = send(Robot, Channel, Term),
			current_time(CurrentTime),
			append(OldQueue, [m(Robot, Channel, CurrentTime, Term)], Q), !
			;
		A = receive(Robot, Channel),
			lists:select(m(Robot, Channel, _, _), OldQueue, Q), !
			;
		% TODO: handle expiration in tick
		Q = OldQueue, !
		).
			
			
global_msg_queue(Q, s0) :- get_current(global_msg_queue, [], Q).			
global_msg_queue(Q, slast) :- get_last(global_msg_queue, [], Q).		
			
msg_in_buf(Robot, Msg, do2(A,S)) :-
		A = receive(Robot, Channel),
			global_msg_queue(Q, S),
			member(m(Robot, Channel, Time, Term), Q), 
			Msg = m(Robot, Channel, Time, Term), !
		;
			msg_in_buf(Robot, OldMsg, S),
			Msg = OldMsg, !.

msg_in_buf(Robot, Msg, s0) :- get_current(msg_in_buf, [Robot], Msg).

msg_in_buf(Robot, Msg, slast) :- get_last(msg_in_buf, [Robot], Msg).


			
robotLeftFrom(Rob, Pos, S) :-
        xpos(Rob, P, S),
        P < Pos.
        
robotRightFrom(Rob, Pos, S) :-
        xpos(Rob, P, S),
        P > Pos. 
        
		


