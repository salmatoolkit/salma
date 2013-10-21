xpos(Object, X, do2(A,S)) :-
	(A = tick ->		
		domain(robot, DRobot),
		(
			member(Object, DRobot), !,
			new_xpos_robot(Object, X, S)
			;			
			member(Robot, DRobot),
			attached(Robot, Object,S), !,
			new_xpos_robot(Robot, RobX, S),
			X is RobX
			;
			xpos(Object, X, S), !
		)
		; % else: not tick
		xpos(Object, X, S)
	).


ypos(Object, Y, do2(A,S)) :-
	(A = tick ->		
		domain(robot, DRobot),
		(
			member(Object, DRobot), !,
			new_ypos_robot(Object, Y, S)
			;			
			member(Robot, DRobot),
			attached(Robot, Object, S), !,
			new_ypos_robot(Robot, RobY, S),
			Y is RobY
			;
			ypos(Object, Y, S), !
		)
		; % else: not tick
		ypos(Object, Y, S)
	).
	
	
direction(Robot, Phi, do2(A,S)) :-
	A = update_direction(Robot, NewPhi, Error) ->
		Phi is NewPhi + Error
		;
		direction(Robot, Phi, S).


velocity(Robot, V, do2(A,S)) :-
	A = update_velocity(Robot, NewV, Error) ->
		V is NewV + Error
		;
		velocity(Robot, V, S).


	
light_sensor_value(Robot, Sensor, Value, do2(A,S)) :-
	A = update_light_sensor(Robot, Sensor, Wavelength, Error) ->
		calc_light_sensor_intensity(Robot, Sensor, Wavelength, Intensity, S),
		Value is Intensity + Error
		;
		light_sensor_value(Robot, Sensor, Value, S).
		

	
distance_sensor_value(Robot, Sensor, Value, do2(A,S)) :-
	A = update_distance_sensor(Robot, Sensor, Error) ->
		calc_distance_sensor_intensity(Robot, Sensor, Intensity, S),
		Value is Intensity + Error
		;
		distance_sensor_value(Robot, Sensor, Value, S).
		
		
carrying(Robot, Item, do2(A,S)) :-
	A = grab(Robot, Item), !
	;
	A \= drop(Robot, Item),
	carrying(Robot, Item, S).
	
light_active(Robot, do2(A,S)) :-
	A = activate_light(Robot), !
	;
	A \= deactivate_light(Robot),
	light_active(Robot, S).
	
getItemInRangePosition(Robot, Item, S) :-
	domain(item, Items),
	item_sensor_range(Range),
	(fromto(Items, In, Out, []), fromto(_, _, Out2, Item), 
		param(Robot, S, Range) do
		In = [Item | Rest],
		object_distance(Robot, Item, Distance, S),
		(Distance =< Range ->
			Out = [],
			Out2 = Item
			;
			Out = Rest,
			Out2 = null
		)
	).
		
	
	

item_sensor_value(R, V, do2(A,S)) :- 
	A = sense_for_item(R) ->
		getItemInRangePosition(R, V, S)
		;
		item_sensor_value(R, V, S).


target_wavelength(R, V, do2(A,S)) :-
	A = set_target_wavelength(R,V2) ->
		V is V2
		;
		target_wavelength(R,V,S).
		
	