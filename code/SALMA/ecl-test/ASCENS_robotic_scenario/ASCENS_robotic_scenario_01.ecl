calcNormedVectorSum(
	Robot, Sensors, Function, Invert,
	SumDx, SumDy, 
	MaxIntensity, Sit) :-
	% idea: calculate analogous to center of gravity, i.e. sum up sensor angles with
	% intensity readings as weight and divide by senor count.
	(foreach(Sensor, Sensors), fromto(vec(0,0), V1, V2, VectorSum), 
	fromto(0, Max1, Max2, MaxIntensity), param(Robot, Function, Sit, Invert) do
		call(Function, Robot, Sensor, Intensity, Sit),
		(Invert = true -> Intensity2 is 1.0 - Intensity ; Intensity2 is Intensity),
		Phi is sensor_angle(Sensor),
		getBeamVector(Phi, Intensity2, Dx, Dy),
		V1 = vec(OldDx, OldDy),
		NewDx is OldDx + Dx, 
		NewDy is OldDy + Dy,
		V2 = vec(NewDx, NewDy),
		% this is not inverted!
		Max2 is max(Max1, Intensity)
	),
	VectorSum = vec(SDx, SDy),
	SumDx is SDx / length(Sensors),
	SumDy is SDy / length(Sensors).
	
	
direction_arbiter(Robot, Sit, NewDirection) :-
	domain(light_sensor, LightSensors),
	calcNormedVectorSum(
		Robot, LightSensors, calc_light_sensor_intensity, false,
		LightCenterDx, LightCenterDy, MaxLightIntensity, Sit),
	domain(distance_sensor, DistanceSensors),
	calcNormedVectorSum(
		Robot, DistanceSensors, calc_distance_sensor_intensity, true,
		FreeSpaceCenterDx, FreeSpaceCenterDy,
		MaxObstacleIntensity, Sit),
	% if no sensor reading is present (out of range) then just keep 
	% current direction
	((MaxLightIntensity =:= 0, MaxObstacleIntensity =:= 0) ->
		direction(Robot, NewDirection, Sit)
		;
		NewDirX is ((1.0 - MaxObstacleIntensity)*LightCenterDx + 
			MaxObstacleIntensity*FreeSpaceCenterDx) / 2,
		NewDirY is ((1.0 - MaxObstacleIntensity)*LightCenterDy + 
			MaxObstacleIntensity*FreeSpaceCenterDy) / 2,
		getVectorAngle(NewDirX, NewDirY, NewDirection)
	).
	
	


		