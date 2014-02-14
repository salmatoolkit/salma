:- ['../ecl-src/agasmc'].
:- [domaindesc_extended].


domain(robot,D) :-
        D=[rob1, rob2].

domain(item, D) :- D = [coffee, chocolate].


init :-
	init_agasmc,
	set_current(xpos, [rob1], 20),
	set_current(ypos, [rob1], 20),
	set_current(carrying,[rob1, coffee], true),
	set_current(carrying,[rob1, chocolate], false),
	
	set_current(xpos, [rob2], 20),
	set_current(ypos, [rob2], 30),
	set_current(carrying,[rob2, coffee], false),
	set_current(carrying,[rob2, chocolate], false),
	set_current(time,[],0).