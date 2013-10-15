:- ['../ecl-src/agasmc_progression'].
:- dynamic channelContent/3, sending/4.

domain(robot,D) :-
        D=[rob1,rob2,rob3].

domain(channel, D) :-
		D=[channel1, channel2].

fluent(channelContent, [channel], list).
fluent(sending,[robot,channel],integer).



channelContent(Channel, Content, do2(A, S)) :-
	A = send(Robot, Channel, Data, Size) ->
		channelContent(Channel, OldContent, S),
		append(OldContent, [msg(Robot, Data, Size)], Content)
	;
		channelContent(Channel, Content, S).

channelContent(Channel, Content, s0) :-
	get_current(channelContent,[Channel], Content).

sending(Robot, Channel, Size, do2(A, S)) :-
	A =  send(Robot, Channel, _, X) -> Size = X 
		;
		Size = 0.

sending(Robot, Channel, Size, s0) :-
	get_current(sending, [Robot, Channel], Size).
	

init :-
	init_progression,
	set_current(channelContent, [channel1], []).

	

%bandwidthConsumption(BWC, S) :- 
%	findall(Size, transmission(_, _, Size, S), Sizes),
%	sum(Sizes, BWC).
	
	