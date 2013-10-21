proc(transportToX, [r1:robot, i:item, targetX : integer],
	grab(r1,i) : 
	while(xpos(r1) < targetX, 
		move_right(r1)
	) :
	drop(r1,i)
	).