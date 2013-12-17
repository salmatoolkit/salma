% PLCS
constant(maxCapacty, [p:plcs], integer).

fluent(plcs_vehicle_reservationRequests, [p:plcs], list).

% format: res(vehicle, startTime, plannedDuration)
fluent(plcsReservations, [p:plcs], list).


derived_fluent(currentOccupacy, [p:plcs], integer).
