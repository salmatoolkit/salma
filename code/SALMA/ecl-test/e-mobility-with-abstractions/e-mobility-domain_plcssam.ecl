:- dynamic freeSlotsR/4.

fluent(freeSlotsR, [sam:plcssam, plcs:plcs], integer).

%this fluent is set directly for now
freeSlotsR(Sam, PLCS, FreeSlots, do2(A,S)) :-
	freeSlotsR(Sam, PLCS, FreeSlots, S).