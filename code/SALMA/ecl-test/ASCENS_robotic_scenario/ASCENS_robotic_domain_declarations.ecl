:- dynamic 
	%robot fluents
	xpos/3, ypos/3, direction/3, velocity/3, light_active/2, 
	light_sensor_value/4, distance_sensor_value/4, carrying/3,
	item_sensor_value/3, target_wavelength/3,
	
	% derived fluents
	object_distance/4,	
	%constants
	light_sensor_count/1,	distance_sensor_count/1,
	sensor_angle/2,
	max_light_sensor_range/1, max_light_sensor_angle/1,
	max_distance_sensor_range/1, max_distance_sensor_angle/1,
	max_light_sensor_intensity/1,
	object_radius/2,
	item_weight/2,
	robot_strength/2,
	rectangle_width/2, rectangle_height/2,
	light_source_wavelength/2,
	item_sensor_range/1,
	item_pick_range/1.
	

sorts([light_sensor,distance_sensor,object,rectangular_object,
		circular_object,item,obstacle,robot,block,boundary,light_source]).
sort(wavelength).
sort(drop_zone).

subsort(rectangular_object, object).
subsort(circular_object, object).
	
subsorts([block, boundary], rectangular_object).
subsort(drop_zone, rectangular_object).
subsorts([robot, item], circular_object).
subsorts([robot, block, boundary], obstacle).
subsorts([robot, drop_zone], light_source).



% internal actions for robots
primitive_action(update_direction,[robot, float, float]). % robot, angle, error
primitive_action(update_light_sensor,[robot, light_sensor, wavelength, float]). % robot, sensor, error
primitive_action(update_distance_sensor,[robot, distance_sensor, float]). % robot, sensor, error
primitive_action(update_velocity,[robot, float, float]).
primitive_action(grab,[robot,item]). % pick_up could be a action that takes some time
primitive_action(drop,[robot,item]).
primitive_action(sense_for_item,[robot]).
primitive_action(set_target_wavelength,[robot, wavelength]).
primitive_action(activate_light,[light_source]).
primitive_action(deactivate_light,[light_source]).

fluent(direction,[robot],float).
fluent(velocity,[robot],float).
fluent(light_active,[light_source],boolean).


% SENSORS
fluent(light_sensor_value,[robot,light_sensor],float).
fluent(distance_sensor_value,[robot,distance_sensor],float).
fluent(item_sensor_value,[robot], item).


fluent(target_wavelength,[robot],wavelength).
fluent(xpos,[object],float).
fluent(ypos,[object],float).

fluent(carrying,[robot,item],boolean).

% derived fluents
derived_fluent(object_distance,[object, object], float).