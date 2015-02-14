:- dynamic locX/2, locY/2, roadEnds/2, roadlength/2, roadBaseSpeed/2,
	numVehiclesOnRoad/3, responsiblePLCSSAM/2.
	
:- ['e-mobility-domain_vehicle'].
:- ['e-mobility-domain_plcs'].
:- ['e-mobility-domain_plcssam'].


% notes: 
% - neglect the traffic situation and exclude RouteSAM for now.
%	Best route is simply calculated by shortest path.
% - PLCSSAM chooses best plcs and creates a reservation
%	PLCS may not reject a request.
% - should introduce "directly set fluents" that are
%   set with setFluentValue
% 	- examples could be state flags, plans, etc.

sorts([vehicle, plcs, plcssam, poi, crossing, location, road]).
subsort(location, object).
subsorts([plcs, plcssam, poi, crossing], location).
subsorts([vehicle, plcs, plcssam], agent).

constant(locX, [loc:location], integer).
constant(locY, [loc:location], integer).

% ROADS
%% given as r(Start, End) 
constant(roadEnds, [r:road], term).

constant(roadlength, [r:road], float).
constant(roadBaseSpeed, [r:road], float).

derived_fluent(numVehiclesOnRoad, [r:road], integer).

numVehiclesOnRoad(Road, Count, S) :-
	findall(V, vehiclePosition(V, r(Road), S), Vehicles),
	length(Vehicles, Count).
	

constant(responsiblePLCSSAM, [loc:location], plcssam).
doc(responsiblePLCSSAM : constant, [
	summary: "The PLCSSAM that is responsible for the area that includes loc.",
	desc:	"Setup should be done in a way that makes sure that close plcs are always
			associated with the PLCSSAM in the area."
	]).

channel(assignment, veh:vehicle, sam:plcssam, unicast).
channel(reservation, veh:vehicle, plcs:plcs, unicast).
remoteSensor(freeSlotsR, plcssam, freeSlotsL, plcs).

% TODO: define cool ensembles
ensemble(freeSlotsR, SAM, PLCS, S) :- true.

init_domaindesc :- true.