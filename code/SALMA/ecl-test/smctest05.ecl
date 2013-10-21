:- ['../ecl-src/agasmc'].
:- [domaindesc].

domain(robot,D) :-
        D=[rob1].

domain(item, D) :- D = [coffee, chocolate].


init :-
	init_agasmc,
	set_current(xpos, [rob1], 20),
	set_current(ypos, [rob1], 20),
	set_current(carrying,[rob1, coffee], true),
	set_current(carrying,[rob2, chocolate], false),
	set_current(time,[],0).