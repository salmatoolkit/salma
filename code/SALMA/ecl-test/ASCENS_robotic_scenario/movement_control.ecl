% Direction = 0 means straight forward
new_xpos_robot(Robot, X, S) :-
	xpos(Robot, XOld, S),
	direction(Robot, Dir, S),
	velocity(Robot, V, S),
	getBeamVector(Dir, V, Dx, _),
	X is XOld + Dx.

new_ypos_robot(Robot, Y, S) :-
	ypos(Robot, YOld, S),
	direction(Robot, Dir, S),
	velocity(Robot, V, S),
	getBeamVector(Dir, V, _, Dy),
	Y is YOld + Dy.
	

	
object_distance(O1, O2, Dist, S) :-
	xpos(O1, X1, S), ypos(O1, Y1, S),
	xpos(O2, X2, S), ypos(O2, Y2, S),
	Dist is max(0,sqrt((X2-X1)^2 + (Y2-Y1)^2) - object_radius(O1) - object_radius(O2)).
	

% normalize angle to 0 - 2*Pi
% 0 means right in front of robot
getAngleToSource(Robot,Source,Angle,Sit) :-
	xpos(Robot, MyX, Sit), ypos(Robot, MyY, Sit),
	xpos(Source, SourceX, Sit), ypos(Source, SourceY, Sit),
	getAngle(MyX, MyY, SourceX, SourceY, Angle).
		

calc_sensor_intensity_from_source(Robot, Source, Sensor, 
	MaxRange, MaxAngle, 
	Intensity, Sit) :-
	% - sensor no. 1 is always steered straight forward
	% - sensor count starts with 1
	% for now we make the following simplifications:
	% - the sensors are located at the center of the robot
	Robot = Source ->
		Intensity is 0
		;
		ThisAngle is sensor_angle(Sensor),		
		xpos(Robot, RX, Sit), ypos(Robot, RY, Sit),
		(isSortOf(Source, rectangular_object) ->
			calc_sensor_intensity_from_rectangular_object(
				RX, RY, ThisAngle, Source, 
				MaxRange, MaxAngle, 
				Intensity, Sit)
			;
			calc_sensor_intensity_from_circular_object(
				RX, RY, ThisAngle, Source, 
				MaxRange, MaxAngle, 
				Intensity, Sit)
		)
	.
	
calc_sensor_intensity_from_circular_object(
	RX, RY, BeamAngle, Source,
	MaxRange, MaxAngleDiff, 
	Intensity, Sit) :-
	xpos(Source, SX, Sit), ypos(Source, SY, Sit),
	% only the extension of the source object is considered
	Dist is max(sqrt((SX - RX)^2 + (SY - RY)^2) - object_radius(Source), 0),
	
	SourceAngle is getAngle(RX, RY, SX, SY),
	Intensity is relativeSensorIntensity(
					SourceAngle, BeamAngle, MaxAngleDiff,
					Dist, MaxRange).
					
calc_sensor_intensity_from_rectangular_object(
	RobotX, RobotY, BeamAngle, Object,
	MaxRange, MaxAngleDiff, 
	Intensity, Sit) :-
	xpos(Object, BX1, Sit),
	xpos(Object, BY1, Sit),
	rectangle_width(Object, BWidth),
	rectangle_height(Object, BHeight),
	calcClosestBeamIntersectingRect(
			RobotX, RobotY, BeamAngle,
			BX1, BY1, BWidth, BHeight, 
			SourceAngle, HitPoint),
	Dist is pointDistance(p(RobotX, RobotY), HitPoint),
	Intensity is relativeSensorIntensity(
					SourceAngle, BeamAngle, MaxAngleDiff,
					Dist, MaxRange).
	
	
relativeSensorIntensity(
	SourceAngle, BeamAngle, MaxAngleDiff,
	Distance, MaxRange,
	Intensity) :-
		AngleDiff is getMinAngleDifference(SourceAngle, BeamAngle),
		Intensity is (1 - min(AngleDiff, MaxAngleDiff) / MaxAngleDiff) *
				(1 - min(Distance, MaxRange) / MaxRange).	
	
	
get_active_light_sources_by_wavelength(Wavelength, Sources, Sit) :-
	domain(light_source, AllSources),
	findall(S, 
			(member(S, AllSources), 
				light_source_wavelength(S, Wavelength),
				light_active(S, Sit)
			), 
			Sources).
	
% a light sensor receives the sum of the intensity of all sources
% TODO: should there be a max intensity?
calc_light_sensor_intensity(Robot, Sensor, Wavelength, Intensity, Sit) :- 
	get_active_light_sources_by_wavelength(
		Wavelength, Sources, Sit),
	MaxRange is max_light_sensor_range,
	MaxAngle is max_light_sensor_angle,
	(foreach(Source, Sources), fromto(0, I1, I2, AbsoluteIntensity), 
		param(Robot, Sensor, MaxRange, MaxAngle, Sit) do
		calc_sensor_intensity_from_source(Robot, Source, Sensor, 
			MaxRange, MaxAngle, ITemp, Sit),
		I2 is I1 + ITemp
	),
	% normalize to scale of [0,1]
	Intensity is min(1.0, AbsoluteIntensity / max_light_sensor_intensity).

% a distance sensor receives only the intensity from the most influencing object
calc_distance_sensor_intensity(Robot, Sensor, Intensity, Sit) :- 
	domain(obstacle, Obstacles),
	MaxRange is max_distance_sensor_range,
	MaxAngle is max_distance_sensor_angle,
	(foreach(Obstacle, Obstacles), fromto(0, I1, I2, Intensity), 
		param(Robot, Sensor, MaxRange, MaxAngle, Sit) do
		calc_sensor_intensity_from_source(Robot, Obstacle, Sensor, 
			MaxRange, MaxAngle, ITemp, Sit),
		I2 is max(I1, ITemp)
	).
	


getMaxSensorValue(Robot, Sensors, SensorFluent, MaxValue, S) :-
	(foreach(Sensor, Sensors), fromto(0, In, Out, MaxValue),
		param(Robot, S, SensorFluent) do
		T =.. [SensorFluent, Robot, Sensor, V, S],
		call(T),
		(V > In -> Out is V ; Out is In)
	).

getMaxDistanceSensorValue(Robot, MaxValue, S) :-
	domain(distance_sensor, Sensors),
	getMaxSensorValue(Robot, Sensors, distance_sensor_value, MaxValue, S).

getMaxLightSensorValue(Robot, MaxValue, S) :-
	domain(light_sensor, Sensors),
	getMaxSensorValue(Robot, Sensors, light_sensor_value, MaxValue, S).


	