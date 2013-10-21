filter_points_on_rect(
	Points, 
	RX1,RY1, RWidth, RHeight,
	PointsOnRect) :-
		RX2 is RX1 + RWidth,
		RY2 is RY1 + RHeight,
		(foreach(P, Points), fromto([], In, Out, PointsOnRect),
			param(RX1, RX2, RY1, RY2) do
			P = p(X,Y),
			((RX1 =< X, X =< RX2, RY1 =< Y, Y =< RY2) ->
				append(In, [P], Out)
				;
				Out = In
			)
		).


filter_points(Points, PX, PY, XPred, YPred, FilteredPoints) :-
	(foreach(P, Points), fromto([], In, Out, FilteredPoints),
		param(XPred, YPred, PX, PY) do
		P = p(X,Y),
		TX =.. [XPred | [PX, X]],
		TY =.. [YPred | [PY, Y]],
		((call(TX), call(TY)) ->
			append(In, [P], Out)
			;
			Out = In
		)
	).
		
calc_line_rect_intersections(
	PX, PY, Slope, 
	RX1,RY1, RWidth, RHeight,
	Intersections) :-
		B is PY - Slope * PX,
		RX2 is RX1 + RWidth,
		RY2 is RY1 + RHeight,
		% intersection with horizontal line at RY1
		XI1 is (RY1 - B) / Slope,
		ISec1 = [p(XI1, RY1)],
		% intersection with horizontal line at RY2
		XI2 is (RY2 - B) / Slope,
		append(ISec1, [p(XI2, RY2)], ISec2),
		% intersection with vertical line at RX1
		YI3 is Slope*RX1 + B,
		append(ISec2, [p(RX1, YI3)], ISec3),
		
		YI4 is Slope*RX2 + B,
		append(ISec3, [p(RX2, YI4)], ISec4),
		filter_points_on_rect(ISec4, 
			RX1,RY1, RWidth, RHeight,
			Intersections).
			
		
		
		
calc_vertical_line_rect_intersections(			
	X, RX1,RY1, RWidth, RHeight,
	Intersections) :-
		RX2 is RX1 + RWidth,
		RY2 is RY1 + RHeight,
		((RX1 =< X, X =< RX2) ->
			Intersections = [p(X, RY1), p(X, RY2)] 
			;
			Intersections = []
		).

calc_horizontal_line_rect_intersections(			
	Y, RX1, RY1, RWidth, RHeight,
	Intersections) :-
		RX2 is RX1 + RWidth,
		RY2 is RY1 + RHeight,
		((RY1 =< Y, Y =< RY2) ->
			Intersections = [p(RX1, Y), p(RX2, Y)] 
			;
			Intersections = []
		).
		

		
calc_beam_rect_intersections(
	PX, PY, BeamAngle,
	RX1,RY1, RWidth, RHeight,
	Intersections) :-
		(BeamAngle >= 0.0, BeamAngle < pi/2,
			% left upper quadrant
			(BeamAngle =:= 0 ->
				calc_vertical_line_rect_intersections(
					PX, RX1, RY1, RWidth, RHeight, ISec1)
				;
				Slope is -1.0 * tan(pi/2 - BeamAngle),
				calc_line_rect_intersections(
					PX, PY, Slope,
					RX1,RY1, RWidth, RHeight,
					ISec1)
			),
			filter_points(ISec1, PX, PY, '>=', '=<', Intersections), !
		
		; BeamAngle >= pi/2, BeamAngle < pi,
			% left lower quadrant
			(BeamAngle =:= pi/2 ->
				calc_horizontal_line_rect_intersections(
					PY, RX1, RY1, RWidth, RHeight, ISec1)
				;
				Slope is tan(BeamAngle - pi/2),
				calc_line_rect_intersections(
					PX, PY, Slope,
					RX1,RY1, RWidth, RHeight,
					ISec1)
			),
			filter_points(ISec1, PX, PY, '>=', '>=', Intersections), !
		; BeamAngle >= pi, BeamAngle < 1.5*pi,
			% right lower quadrant
			(BeamAngle =:= pi ->
				calc_vertical_line_rect_intersections(
					PX, RX1, RY1, RWidth, RHeight, ISec1)
				;
				Slope is -1.0*tan(1.5*pi - BeamAngle),
				calc_line_rect_intersections(
					PX, PY, Slope,
					RX1,RY1, RWidth, RHeight,
					ISec1)
			),
			filter_points(ISec1, PX, PY, '=<', '>=', Intersections), !
		; BeamAngle >= 1.5*pi, BeamAngle < 2*pi,
			% upper right quadrant
			(BeamAngle =:= 1.5*pi ->
				calc_horizontal_line_rect_intersections(
					PY, RX1, RY1, RWidth, RHeight, ISec1)
				;
				Slope is tan(BeamAngle - 1.5*pi),
				calc_line_rect_intersections(
					PX, PY, Slope,
					RX1,RY1, RWidth, RHeight,
					ISec1)
			),
			filter_points(ISec1, PX, PY, '=<', '=<', Intersections), !
		; 
			throw(angle_out_of_range(BeamAngle))
		).

pointDistance(P1, P2, Dist) :-
	P1 = p(X1,Y1),
	P2 = p(X2,Y2),
	Dist is sqrt( (X2 - X1)^2 + (Y2 - Y1)^2 ).
		
getClosestPointFromList(
	PX, PY, PointList, ResultPoint) :-
	PointList = [P1 | _],
	Dist1 is pointDistance(p(PX,PY), P1),
	(foreach(P, PointList), fromto(pdist(P1, Dist1), Old, New, Result),
		param(PX,PY) do
		Old = pdist(_, OldDist),
		D is pointDistance(p(PX,PY), P),
		(D < OldDist -> New = pdist(P, D) ; New = Old)
	),
	Result = pdist(ResultPoint,_).
		
% Given a beam (Source (X,Y) + Angle) and a rectangle,
% this function calculates the best approximation
% of a beam that intersects the rectangle, together with
% the resulting intersection point.
%
% The chosen beam can either be the original beam itself,
% if it actually intersects. Otherwise the beam must cut through
% one of the rectangle's edges.
% - the result is given as a p(X,Y) term.
calcClosestBeamIntersectingRect(
	SourceX, SourceY, OriginalBeamAngle,
	RX1,RY1, RWidth, RHeight,
	ResultAngle, Intersection) :-
		% 1.) check if original beam intersects
		calc_beam_rect_intersections(
			SourceX, SourceY, OriginalBeamAngle,
			RX1,RY1, RWidth, RHeight,
			ISec1),
		(length(ISec1) > 0 ->
			getClosestPointFromList(
				SourceX, SourceY, ISec1, Intersection),
			ResultAngle is OriginalBeamAngle
		;
			% The original beam doesn't intersect so we look for the
			% rectangle's edge the beam would cut after the smallest rotation.
			% lower right
			RX2 is RX1 + RWidth, RY2 is RY1,
			% upper right
			RX3 is RX1 + RWidth, RY3 is RY1 + RHeight,
			% upper left
			RX4 is RX1, RY4 is RY1 + RHeight,
			
			getPointWithMinimalRotationDistance(
				SourceX, SourceY, OriginalBeamAngle,
				[p(RX1, RY1), p(RX2, RY2), p(RX3, RY3), p(RX4, RY4)],
				NewBeam),
			NewBeam = beam(Intersection, ResultAngle, _)
		).
			
			
			
getPointWithMinimalRotationDistance(
	SourceX, SourceY, OriginalBeamAngle,
	PointList, Result) :-
	PointList = [P1 | _],
	P1 = p(X1,Y1),
	Angle1 is getAngle(SourceX, SourceY, X1, Y1),
	ADist1 is getMinAngleDifference(OriginalBeamAngle, Angle1),
	(foreach(P, PointList), fromto(beam(P1, Angle1, ADist1), Old, New, Result),
		param(SourceX, SourceY, OriginalBeamAngle) do
			P = p(X,Y),
			Old = beam(_, _,OldDist),
			Angle is getAngle(SourceX, SourceY, X, Y),
			Dist is getMinAngleDifference(OriginalBeamAngle, Angle),
			(Dist < OldDist -> New = beam(P, Angle, Dist) ; New = Old)
	).


getAngle(MyX, MyY, SourceX, SourceY, Angle) :-
	Dy is float(SourceY-MyY),
	Dx is float(SourceX-MyX),
	getVectorAngle(Dx, Dy, Angle).

getVectorAngle(Dx2, Dy2, Angle) :-
	Dx is float(Dx2), Dy is float(Dy2),
	ADx is float(abs(Dx)), ADy is float(abs(Dy)),
	(ADx =\= 0 -> Phi is atan(ADy / ADx) ; Phi is 0.0),
	(Dy >= 0, !, % upper half
		(Dx =:= 0, !, Angle is 0.0
			;
		% upper right quadrant --> 270°+phi 
		Dx > 0, !, Angle is 1.5*pi + Phi 
		;
		% upper left quadrant --> 90° - phi
		Dx < 0, !, Angle is 0.5*pi - Phi
		) 
	;
	Dy < 0,!,
		(Dx =:= 0, !, Angle is pi   
			;
		% lower right quadrant --> 270° - phi
		Dx > 0, !, Angle is 1.5*pi - Phi
		;
		% lower left quadrant --> 90° + phi
		Dx < 0, !, Angle is 0.5*pi + Phi
		)
	).

getBeamVector(Angle, Length, Dx, Dy) :-
	(Angle =:= 0 ; Angle =:= 2*pi), !,
		Dx is 0.0, Dy is float(Length)
	; Angle > 0, Angle < pi/2, !,
		Dx is -1.0 * cos(pi/2 - Angle) * Length,
		Dy is sin(pi/2 - Angle) * Length
	; Angle =:= pi/2, !,
		Dx is -1.0 * Length,
		Dy is 0.0
	; Angle > pi/2, Angle < pi, !,
		Dx is -1.0 * cos(Angle - pi/2) * Length,
		Dy is -1.0 * sin(Angle - pi/2) * Length
	; Angle =:= pi, !,
		Dx is 0.0,
		Dy is -1.0 * Length
	; Angle > pi, Angle < 1.5*pi, !,
		Dx is cos(1.5*pi - Angle) * Length,
		Dy is -1.0 * sin(1.5*pi - Angle) * Length
	; Angle =:= 1.5*pi, !,
		Dx is float(Length),
		Dy is 0.0
	; Angle > 1.5*pi, Angle < 2*pi, !,
		Dx is cos(Angle - 1.5*pi) * float(Length),
		Dy is sin(Angle - 1.5*pi) * float(Length)
	.
	
	
	
	
deg2rad(PhiIn, PhiOut) :-
	PhiOut is PhiIn * pi/180.

rad2deg(PhiIn, PhiOut) :-
	PhiOut is PhiIn * 180/pi.

	
% retrieves the minimal difference between 2 angles
% considering a clockwise as well as a counter-clockwise 
% viewpoint.	
getMinAngleDifference(Angle1, Angle2, Diff) :-
	% sort angles
	A1 is min(Angle1, Angle2),
	A2 is max(Angle1, Angle2),
	D1 is A2 - A1,
	D2 is A1 + 2*pi - A2,
	Diff is min(D1,D2).
		