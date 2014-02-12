proc(park_to_recharge, [veh:vehicle],
	pi([p, plcs],
		request_lot(veh, p) :
		drive_to(veh, p) :
		park(veh, p),
		?(battery_level(veh) > 0.1)
	)).
	
	
test(Results) :- F = forall([veh, vehicle],
				implies(
					occur(start(veh)),
					until(100,
						possible(park_to_recharge(veh)),
						currentPLCS(veh) == currentTargetPLCS(veh)
					)
				)
			),
		evaluate_ad_ahoc(F, Results).
				
						