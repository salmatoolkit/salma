:- dynamic posX/3, posY/3, temperature/3, oxygenLevel/3,
		accelX/3, accelY/3, vX/3, vY/3, 
		% state fluents
		atBuildingEntrance/2, insideBuilding/2, 
		exploringBuilding/2, leavingBuilding/2, conscious/2,
		closeToFire/2, timeToExitBuilding/3,		
		sensorData/3, sensorDataList/3, gmsInDanger/3, 
		nearbyGMInDanger/2,
		id/2, groupLeaderId/2.

sorts([firefighter, groupMember, groupLeader, ensemble]).
subsorts([groupMember, groupLeader], firefighter).

% for now we don't really use the position
fluent(posX, [f:firefighter], integer).
fluent(posY, [f:firefighter], integer).
fluent(temperature, [f:groupMember], integer).
fluent(oxygenLevel, [f:groupMember], integer).
fluent(accelX, [f:firefighter], integer).
fluent(accelY, [f:firefighter], integer).
% velocity is used internally for simulation
fluent(vX, [f:firefighter], integer).
fluent(vY, [f:firefighter], integer).

% qualitative state fluents
fluent(atBuildingEntrance, [gm:groupMember], boolean).
fluent(insideBuilding, [gm:groupMember], boolean).
fluent(exploringBuilding, [gm:groupMember], boolean).
fluent(leavingBuilding, [gm:groupMember], boolean).
fluent(conscious, [gm:groupMember], boolean).
fluent(closeToFire, [gm:groupMember], boolean).
fluent(stuck, [gm:groupMember], boolean).
% this grows when exploring, i.e. we assume that 
% the firefighter gradually moves deeper into the building
fluent(timeToExitBuilding, [gm:groupMember], integer).


% sensorData term format: 
%	sd(groupMemberId, temp, posX, posY, accelX, accelY)
fluent(sensorData, [gm:groupMember], term).
% belief of group leader
fluent(sensorDataList, [gl:groupLeader], list).
% a list of ids of group members who are in danger
fluent(gmsInDanger, [gl:groupLeader], list).

% GM's belief about whether a nearby group member is in danger
fluent(nearbyGMInDanger, [gm:groupMember], boolean).

constant(id, [f:firefighter], integer).
constant(groupLeaderId, [gm:groupMember], integer).

% Firefighters control their motion by setting acceleration.
% In our first simulation this will happen more or less randomly.
primitive_action(setAcceleration,[f:firefighter, 
					ax:integer, ay:integer]).

primitive_action(startLeavingBuilding, [gm:groupMember]).

primitive_action(exchangeData, [gl:groupLeader, gm:groupMember]).

exogenous_action(arriveAtBuilding, [gm:groupMember], []).
exogenous_action(enterFireRange, [gm:groupMember], []).
exogenous_action(leaveFireRange, [gm:groupMember], []).
exogenous_action(getStuck, [gm:groupMember], []).
exogenous_action(breakFree, [gm:groupMember], []).
exogenous_action(passOut, [gm:groupMember], []).
exogenous_action(wakeUp, [gm:groupMember], []).




					
