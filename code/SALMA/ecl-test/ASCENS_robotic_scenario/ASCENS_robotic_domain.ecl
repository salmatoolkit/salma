:- ['geom_utils.ecl'].
:- ['ASCENS_robotic_domain_declarations.ecl'].
:- ['movement_control.ecl'].
:- ['robot_fluents.ecl'].

poss(update_direction(_,_,_),_) :- true.
poss(update_light_sensor(_,_,_,_),_) :- true.
poss(update_distance_sensor(_,_,_),_) :- true.
poss(update_velocity(_,_,_),_) :- true.
poss(sense_for_item(_),_) :- true.
poss(set_target_wavelength(_,_),_) :- true.

poss(activate_light(R),S) :- not light_active(R,S).
poss(deactivate_light(R),S) :- light_active(R,S).



poss(grab(R,I), S) :- test_ad_hoc(
						and(
							not(exists([i, item], carrying(R, i))),
							not(exists([r, robot], carrying(r, I))),
							object_distance(R,I) =< item_pick_range,
							not(too_heavy(R,I))
						),
						S
					).

poss(drop(R,I), S) :- carrying(R,I,S).



% functions

too_heavy(Robot, Item) :- item_weight(Item) > robot_strength(Robot).
	

	
exogenous_action(_,_,_) :- false.	

	
init_domaindesc :- true.


