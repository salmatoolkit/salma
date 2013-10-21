:- ['../../ecl-src/agasmc'].
:- ['ASCENS_robotic_domain'].
:- ['ASCENS_robotic_scenario_01'].




init_scenario :-
	init_agasmc,
	
	PhiSeg is 2*pi/24,
	(count(I, 1, 24), foreach(LS, LightSensors), 
		foreach(DS, DistanceSensors), param(PhiSeg) do
		sprintf(Str1, "ls%d",[I]), atom_string(LS, Str1),
		sprintf(Str2, "ds%d",[I]), atom_string(DS, Str2),
		Phi is (I-1)*PhiSeg,
		assert(sensor_angle(LS, A) :- A = Phi),
		assert(sensor_angle(DS, A) :- A = Phi)
	),	
	setDomain(boundary, [worldbounds]),
	setDomain(light_sensor, LightSensors),
	setDomain(distance_sensor, DistanceSensors),
	setDomain(drop, [main_goal]),
	setDomain(robot,[rob1,rob2]),
	setDomain(wavelength,[red, green, blue]),
	setDomain(drop_zone, [main_goal]),
	% setDomain(item,[]),
	% setDomain(block,[]),
	
	init_sort_hierarchy(_),
	
	assert(rectangle_width(worldbounds, 500.0)),
	assert(rectangle_height(worldbounds, 500.0)),
	set_current(xpos,[worldbounds], 0.0),
	set_current(ypos,[worldbounds], 0.0),
	set_current(xpos,[main_goal], 450),
	set_current(ypos,[main_goal], 0.0),
	assert(rectangle_width(main_goal, 50.0)),
	assert(rectangle_height(main_goal, 500.0)),

	
	domain(robot, Robots),
	(foreach(R,Robots) do 
		assert(object_radius(R,10.0)),
		set_current(light_active, [R], false),
		set_current(target_wavelength, [R], null),
		set_current(item_sensor_value, [R], null)
		
		),
		
	assert(light_source_wavelength(main_goal, red)), 
	set_current(light_active, [main_goal], true),	
	
	domain(light_source, Lights),
	(foreach(L,Lights) do assert(object_radius(L,10.0))),
	assert(light_sensor_count(24)),
	assert(distance_sensor_count(24)),
	assert(max_light_sensor_range(200.0)),
	A is 0.5*pi, assert(max_light_sensor_angle(A)),
	assert(max_distance_sensor_range(100.0)), 
	assert(max_distance_sensor_angle(A)),
	assert(max_light_sensor_intensity(5.0)), % would correspond to full influence of 5 objects or so
	assert(item_sensor_range(20.0)),	
	
	assert(light_source_wavelength(rob1, blue)),
	assert(light_source_wavelength(rob2, green)),
	set_current(xpos, [rob1], 400.0),
	set_current(ypos, [rob1], 450.0),
	set_current(xpos, [rob2], 50.0),
	set_current(ypos, [rob2], 50.0),
	% set_current(xpos, [item1], 20),
	% set_current(ypos, [item1], 20),
	% set_current(xpos, [item2], 50),
	% set_current(ypos, [item2], 50),
	

	Phi is deg2rad(315),
	set_current(direction,[rob1], Phi),
	set_current(direction,[rob2], 0.0),
	set_current(velocity,[rob1], 2.0),
	set_current(velocity,[rob2], 2.0),
	assert(robot_strength(rob1,10.0)),
	assert(robot_strength(rob2,50.0)),
	
	set_current(time, [], 0),
	(foreach(LS, LightSensors) do
		set_current(light_sensor_value, [rob1, LS], 0.0),
		set_current(light_sensor_value, [rob2, LS], 0.0)
	),
	(foreach(DS, DistanceSensors) do
		set_current(distance_sensor_value, [rob1, DS], 0.0),
		set_current(distance_sensor_value, [rob2, DS], 0.0)
	).

updateSensors(Actions) :-
	findall(I,between(1,24,1,I),Numbers),
	domain(robot, Robots),
	(foreach(I, Numbers), fromto([], In, Out, Actions), param(Robots) do
		(foreach(R, Robots), fromto([], InA, OutA, RobotActions), param(I) do
			append(InA, 
					[update_light_sensor(R, I, 0.0), 
					update_distance_sensor(R, I, 0.0)],
					OutA)
		),
		append(In, RobotActions, Out)
	),
	progress(Actions).
	
print_sensors(Robot) :-
	
	findall(I,between(1,24,1,I),Numbers),
	PhiSeg is 2*pi/length(Numbers),
	halve(Numbers, Left, Right2),
	reverse(Right2, Right),
	printf("\n\n",[]),
	(foreach(S1, Left), foreach(S2, Right), param(Robot, PhiSeg) do
		Phi1 is (S1-1)*PhiSeg, Angle1 is rad2deg(Phi1),
		Phi2 is (S2-1)*PhiSeg, Angle2 is rad2deg(Phi2),
		sprintf(Str1,"ls%d",[S1]), atom_string(LS1, Str1),
		sprintf(Str2,"ls%d",[S2]), atom_string(LS2, Str2),
		sprintf(Str3,"ds%d",[S1]), atom_string(DS1, Str3),
		sprintf(Str4,"ds%d",[S2]), atom_string(DS2, Str4),
		light_sensor_value(Robot, LS1, LV1, s0),
		light_sensor_value(Robot, LS2, LV2, s0),
		distance_sensor_value(Robot, DS1, DV1, s0),
		distance_sensor_value(Robot, DS2, DV2, s0),
         
		printf("(%6.2f = %8.4f) | (%6.2f = %8.4f)                  (%6.2f = %8.4f) | (%6.2f = %8.4f)\n",
				[Angle1,LV1, Angle2, LV2, Angle1,DV1, Angle2,DV2])
	).
		
	