:- dynamic plcsReservations/3, currentOccupancy/3, maxCapacity/2, freeSlots/3, 
	freeSlotsL/3, availableSlots/3, tstamp_freeSlotsL/3, plcsChargeRate/2.
% PLCS
constant(maxCapacity, [p:plcs], integer).
constant(plcsChargeRate, [p:plcs], float).


% format: res(vehicle, startTime, plannedDuration)
fluent(plcsReservations, [p:plcs], list).
primitive_action(add_reservation, 
	[p:plcs, veh:vehicle, startTime:integer, plannedDuration:integer]).

poss(add_reservation(_,_,_,_),_) :- true.
	
% Performed periodically to clean up reservation list.
primitive_action(update_reservations, [p:plcs]).
poss(update_reservations(_),_) :- true.

derived_fluent(currentOccupancy, [p:plcs], integer).
derived_fluent(freeSlots, [p:plcs], integer).
derived_fluent(availableSlots, [p:plcs], integer).

% Declare the free slots sensor and its associated local fluent.
sensor(freeSlotsL, plcs, freeSlots).

% Declare a dedicated time stamp fluent for freeSlotsL.
fluent(tstamp_freeSlotsL, [p:plcs], integer).
untracked_fluent(tstamp_freeSlotsL).



%derived_fluent(expectedOccupancy, [p:plcs, intervalStart:integer,
%	intervalEnd:integer], integer).

plcsReservations(PLCS, Reservations, do2(A,S)) :-
	plcsReservations(PLCS, OldReservations, S),
	(
		A = add_reservation(PLCS, Vehicle, StartTime, PlannedDuration),
		Res = res(Vehicle, StartTime, PLannedDuration),
		append(OldReservations, [Res], Reservations), !
		;
		A = update_reservations(PLCS),
		% sort out all deprecated reservations
		time(CurrentTime, S),
		(foreach(Res, OldReservations), fromto([], R1, R2, Reservations),
			param(CurrentTime, Vehicle) do
				Res = res(Vehicle, StartTime, PlannedDuration),
				(CurrentTime =< StartTime + PlannedDuration ->
					append(R1, [Res], R2)
					;
					R2 = R1
				)
		), !
		;
		Reservations = OldReservations
	).
	
availableSlots(PLCS, AvailableSlots, S) :-
	plcsReservations(PLCS, Reservations, S),
	AvailableSlots is maxCapacity(PLCS) - length(Reservations).

currentOccupancy(PLCS, Occupancy, S) :-
	domain(vehicle, Vehicles),
	findall(Vehicle, (member(Vehicle, Vehicles), currentPLCS(Vehicle, PLCS, S)), Vehicles2),
	length(Vehicles2, Occupancy).
	
freeSlots(PLCS, FreeSlots, S) :-
	currentOccupancy(PLCS, Occupancy, S),
	FreeSlots is maxCapacity(PLCS) - Occupancy.


	
tstamp_freeSlotsL(PLCS, TStamp, do2(A, S)) :-
	new_sensor_timestamp(freeSlotsL, PLCS, [], A, S, TStamp), !
	;
	tstamp_freeSlotsL(PLCS, TStamp, S), !
	; 
	TStamp = none.
	
					