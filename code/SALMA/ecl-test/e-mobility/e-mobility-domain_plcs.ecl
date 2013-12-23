% PLCS
constant(maxCapacty, [p:plcs], integer).

% format: res(vehicle, startTime, plannedDuration)
fluent(plcsReservations, [p:plcs], list).
primitive_action(add_reservation, 
	[p:plcs, veh:vehicle, startTime:integer, plannedDuration:integer]).

% Performed periodically to clean up reservation list.
primitive_action(update_reservations,	[p:plcs])


derived_fluent(currentOccupancy, [p:plcs], integer).
derived_fluent(expectedOccupancy, [p:plcs, intervalStart:integer,
	intervalEnd:integer], integer).

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
			param(CurrentTime) do
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

currentOccupancy(PLCS, Occupancy, S) :-
	findall(Vehicle, currentPLCS(Vehicle, PLCS, S), Vehicles),
	length(Vehicles, Occupancy).
	
		
					