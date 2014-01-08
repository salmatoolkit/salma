:- dynamic locX/2, locY/2, connected/3, roadlength/3,
	responsiblePLCSSAM/2.

:- ['e-mobility-domain_vehicle'].
:- ['e-mobility-domain_plcs'].
:- ['e-mobility-domain_plcssam'].
:- ['e-mobility-domain_ensemble_vehicle-plcs'].
:- ['e-mobility-domain_ensemble_vehicle-plcssam'].

% notes: 
% - neglect the traffic situation and exclude RouteSAM for now.
%	Best route is simply calculated by shortest path.
% - PLCSSAM chooses best plcs and creates a reservation
%	PLCS may not reject a request.
% - should introduce "directly set fluents" that are
%   set with setFluentValue
% 	- examples could be state flags, plans, etc.

sorts([vehicle, plcs, plcssam, poi, crossing, location]).
subsorts([plcs, plcssam, poi, crossing], location).

constant(locX, [loc:location], integer).
constant(locY, [loc:location], integer).

% ROADS
constant(connected, [l1:location, l2:location], boolean).
constant(roadlength, [l1:location, l2:location], integer).

constant(responsiblePLCSSAM, [loc:location], plcssam).
doc(responsiblePLCSSAM : constant, [
	summary: "The PLCSSAM that is responsible for the area that includes loc.",
	desc:	"Setup should be done in a way that makes sure that close plcs are always
			associated with the PLCSSAM in the area."
	]).



% -------------------------------------
% communication fluents
% -------------------------------------

% communication flow:
% 1. PLCSSAM reads all current vehicleReservationRequests
% 2. PLCSSAM selects best PLCS for each vehicle
% 3. PLCSSAM writes to PLCS::reservations
% 4. PLCSSAM sets Vehice::reservation

 
% The list of reservation requests that a vehicle intends to send to the PLCSSAM. Each request
% includes a list of alternatives for possible PLCSs. The PLCSSAM is supposed to select
% the optimal PLCS out of this list.

%stochastic_action(bla, [v:vehicle], [explode]).

init_domaindesc :- true.





