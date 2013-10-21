:- ['geom_utils.ecl'].

point_list_equal(L1, L2, MaxDelta) :-
	(foreach(P1, L1), foreach(P2, L2), param(MaxDelta) do
		P1 = p(X1, Y1), P2 = p(X2, Y2),
		abs(X2 - X1) =< MaxDelta,
		abs(Y2 - Y1) =< MaxDelta
	).

	
case_beam_rect_intersection(
	PX, PY, PhiInDegrees, RX1, RY1, W, H,
	Expected) :-
	Phi is PhiInDegrees * pi / 180, 
	calc_beam_rect_intersections(
		PX, PY, Phi, RX1, RY1, W, H, I),
	printf("p(%.2f,%.2f), phi=%.2f, r(%.2f,%.2f,%.2f,%.2f), I=%w\n",
			[PX,PY, PhiInDegrees, RX1,RY1, W, H, I]),
	point_list_equal(I, Expected, 0.01).
	

	
test_beam_rect_intersection :-
	case_beam_rect_intersection(
		10.0, 10.0, 315.0, 20.0, 15.0, 15.0, 15.0,
		[p(30, 30), p(20, 20)]),
	
	
	Phi2 is 0.0,
	case_beam_rect_intersection(
		30.0,10.0,Phi2,20.0,15.0,15.0,15.0, 
		[p(30.0, 15.0), p(30.0, 30.0)]),
	
	Phi3 is 125.0,
	case_beam_rect_intersection(
		45.0,30.0,Phi3,20.0,15.0,15.0,15.0, 
		[p(23.58, 15.0), p(35.0, 23)]),
	
	Phi4 is 55.0,
	case_beam_rect_intersection(
		45.0,12.0,Phi4,20.0,15.0,15.0,15.0, 	
		[p(20,29.51), p(35, 19)]),
	
	% from inside
	case_beam_rect_intersection(
		24.0,20.0,70.0,
		20.0,15.0,15.0,15.0, 	
		[p(20.0,21.46)]),
	
	case_beam_rect_intersection(
		24.0,20.0,0.0,
		20.0,15.0,15.0,15.0,
		[p(24.0,30.0)]).
		
	
case_closestBeamIntersecting(
	PX, PY, OrigAngle,
	RX1, RY1, W, H,
	ExpectedAngle, ExpectedISec) :-
		OrigPhi is deg2rad(OrigAngle),
		calcClosestBeamIntersectingRect(
			PX, PY, OrigPhi,
			RX1, RY1, W, H, 
			Phi, ISec), 
		rad2deg(Phi, ActualAngle),
		printf("p(%.2f,%.2f), OrigAngle=%.2f, r(%.2f,%.2f,%.2f,%.2f), Angle=%.2f, I=%w\n",
			[PX,PY, OrigAngle, RX1,RY1, W, H, ActualAngle, ISec]),
		abs(ActualAngle - ExpectedAngle) =< 0.01,
		point_list_equal([ISec], [ExpectedISec],0.01).
	
	
test_calcClosestBeamIntersectingRect :- 
	printf("\n\ncalcClosestBeamIntersectingRect\n\n",[]),
	case_closestBeamIntersecting(
		10.0, 10.0, 0.0,
		16.0, 16.0, 14.0, 12.0, 
		341.57, p(16.0, 28.0)),
	case_closestBeamIntersecting(
		15.0, 35.0, 220.0,
		16.0, 16.0, 14.0, 12.0, 
		220.0, p(20.87, 28.0)),
	case_closestBeamIntersecting(
		40.0, 25.0, 150.0,
		16.0, 16.0, 14.0, 12.0, 
		131.99, p(30.0, 16.0)).

test_case_vector2beam(Angle, Length) :-
	deg2rad(Angle, R), 
	getBeamVector(R, Length, X, Y), 
	getVectorAngle(X,Y, A), 
	rad2deg(A,R2),
	
	printf("phi=%.2f , l=%.2f --> (%.2f, %.2f) @ phi'= %.2f\n",
	[Angle, Length, X, Y, R2]), 
	
	Length2 is sqrt(X^2 + Y^2),
	Length2 - Length =< 0.01,
	(Length $= 0, !, 
		R2 $= 0
	; Angle $= 360, !,
		R2 $= 0
	; abs(Angle - R2) =< 0.01, !
	).
		
test_all :-
	test_beam_rect_intersection,
	test_calcClosestBeamIntersectingRect,

	(multifor([I,J],[0,0],[18,10]) do 
		Phi is I*20.0,
		L is J*0.1,
		test_case_vector2beam(Phi,L),
		printf("%.2f - %.2f\n",[Phi,L])
	).
		
	
		
	
	