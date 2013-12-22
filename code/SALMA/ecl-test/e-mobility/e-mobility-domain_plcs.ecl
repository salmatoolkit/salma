% PLCS
constant(maxCapacty, [p:plcs], integer).

% format: res(vehicle, startTime, plannedDuration)
fluent(plcsReservations, [p:plcs], list).

derived_fluent(currentOccupacy, [p:plcs], integer).
