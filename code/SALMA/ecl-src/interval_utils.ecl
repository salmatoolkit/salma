
% Returns the same result for all given start time intervals.
apply_unique_result(StartTimes, Result, Results) :-
	(foreach(Entry, StartTimes), foreach(R, Results),
		param(Result) do
		R = Entry : Result
	).	

negate_results(OrigResults, Results) :-
	(foreach(Entry, OrigResults), foreach(R, Results) do
		Entry = Interval : OldR,
		(OldR = nondet, !, NewR = nondet
		; OldR = ok, !, NewR = not_ok 
		; OldR = not_ok, !, NewR = ok
		),
		R = Interval : NewR
	).		
	
	
	
% precondition: StartTimes sorted
% LeftBoundary and RightBoundary can both be inf to express
% an open interval in the respective direction.
apply_result_within_interval(StartTimes, Result,
	LeftBoundary, RightBoundary, 
	UnhandledStartTimes, Results) :-
	(foreach(Entry, StartTimes), fromto([], STOld, STNew, UnhandledStartTimes),
		fromto([], RIn, ROut, Results), param(LeftBoundary, RightBoundary, Result) do
			get_interval_intersection(Entry, 
				LeftBoundary, RightBoundary, Intersection, Rest),
			(Intersection = none ->
				ROut = RIn
				;
				append(RIn, [Intersection : Result], ROut)
			), 
			append(STOld, Rest, STNew)			
	).
	

	
get_unanimous_result(OrigResults, Result) :-
	(fromto(OrigResults, In, Out, []), fromto(none, R1, R2, Result) do
		In = [First | Rest],
		First = _ : Res,
		
		(Res = nondet, !,
			Out = [], R2 = nondet
		; R1 = none, !,
			Out = Rest, R2 = Res
		; R1 = Res, !, 
			Out = Rest, R2 = R1
		; R1 \= none, R1 \= Res, !,
			Out = Rest, R2 = ambiguous  % nondet has precedence so don't break loop here!
		)
	).

	
	
	
% precondition for merge_goals:
% list sorted ascending
merge_goals([], []).
merge_goals([Goal], [Goal]) :- true, !.
merge_goals([First | Rest], MergedGoals) :-
	merge_goals_rec(First, Rest, MergedGoals).
	
merge_goals_rec(First, [], [First]).
merge_goals_rec(First, [Second | Rest], MergedGoals) :-
	First = s(Start1_1, Start1_2),
	Second = s(Start2_1, Start2_2),
	% remember: intervals are sorted
	(Start2_1 =< Start1_2 + 1 ->
		% found contiguous intervals -> merge
		NewStart2 is max(Start1_2, Start2_2),
		MergedInterval = s(Start1_1, NewStart2),
		merge_goals_rec(MergedInterval, Rest, MergedGoals)
	;
		% not contiguous
		merge_goals_rec(Second, Rest, RestMergedGoals),
		append([First], RestMergedGoals, MergedGoals)
	).
	

	
	
% returns the intersection of the intervals as s(S,E) term or none if there is none.
% Int is has to be a s(S,E) terms. Start and End specify the interval that
% is intersected with Int. Both S and E could be inf.
get_interval_intersection(Int, Start, End, Intersection, Rest) :-
	Int = s(T1, T2),
	(
		Start = inf, End = inf, !, 
			Intersection = s(T1, T2),
			Rest = []
		;
		Start = inf, End \= inf, !, 
			(
				T2 =< End, !, 
				Intersection = s(T1, T2),
				Rest = []
				;
				T1 =< End, T2 > End, !,
				Intersection = s(T1, End),
				RStart is End + 1,
				Rest = [s(RStart, T2)]
				;
				T1 > End, !,
				Intersection = none,
				Rest = [Int]
			)
		;
		Start \= inf, End = inf, !, 
			(
				T1 >= Start, !,
				Intersection = s(T1, T2),
				Rest = []
				;
				T1 < Start, T2 >= Start, !,
				Intersection = s(Start, T2),
				REnd is Start - 1,
				Rest = [s(T1, REnd)]
				;
				T2 < Start, !,
				Intersection = none,
				Rest = [Int]
			)
		; % both Start and End are numeric
			(
				T2 < Start, !,
				Intersection = none,
				Rest = [Int]
				;
				T1 < Start , T2 >= Start, T2 =< End, !,
				Intersection = s(Start, T2),
				REnd is Start - 1,
				Rest = [s(T1, REnd)]
				;
				T1 < Start, T2 > End, !,
				Intersection = s(Start, End),
				R1End is Start - 1,
				R2Start is End + 1,
				Rest = [s(T1, R1End), s(R2Start, T2)]
				;
				T1 >= Start, T2 =< End, !,
				Intersection = s(T1, T2),
				Rest = []
				;
				T1 >= Start, T1 =< End, T2 > End, !,
				Intersection = s(T1, End),
				RStart is End + 1,
				Rest = [s(RStart, T2)]
				;
				T1 > End, !,
				Intersection = none,
				Rest = [Int]
			)
	).
	
get_intervals_within(IntervalList, Start, End, Selection, 
	Remaining) :-
	(foreach(I, IntervalList), fromto([], In, Out, Selection),
		fromto([], R1, R2, Remaining),
		param(Start, End) do	
			get_interval_intersection(I, Start, End,
				Intersection, Rest),
			(Intersection = none ->
				Out = In
				;
				append(In, [Intersection], Out)
			),
			append(R1, Rest, R2)
	).

get_first_interval_within(IntervalList, Start, End, Selection, 
	Remaining) :-
	(fromto(IntervalList, IntIn, IntOut, []), 
		fromto(none, _, SelOut, Selection),
		fromto([], R1, R2, Remaining),
		param(Start, End) do	
			IntIn = [I | Tail],
			get_interval_intersection(I, Start, End,
				SelOut, Rest),
			(SelOut = none ->
				IntOut = Tail,
				append(R1, [I], R2)
				;
				IntOut = [],
				append(R1, Rest, RTemp),
				append(RTemp, Tail, R2)
			)			
	).

	
	
get_right_bound_continuous_intersection(IntervalList, 
	RightBoundary, Intersection) :-
	% assume: 
	% - IntervalList sorted in ascending order
	% - contiguous intervals have been merged
	(fromto(IntervalList, ILLast, ILNext, []),
		fromto(none, _, IntOut, Intersection),
		param(RightBoundary) do
			ILLast = [Interval | Tail],
			Interval = s(Start, End),
			(Start =< RightBoundary, End >= RightBoundary ->
				IntOut = s(Start, RightBoundary),
				ILNext = []
				;
				IntOut = none,
				ILNext = Tail
			)	
	).

% Returns the highest point that is included in an interval from IntervalList
% and within the given bounds. If no interval intersects the bounds then "none" is returned.
get_max_interval_point_within(IntervalList, 
	LeftBoundary, RightBoundary, Max) :-
		% assumption: IntervalList sorted in ascending order
		(fromto(IntervalList, ListIn, ListOut, []),
			fromto(none, MaxIn, MaxOut, Max),
			param(LeftBoundary, RightBoundary) do
				ListIn =  [s(T1, T2) | Tail],
				(T2 < LeftBoundary, !,
					% interval left from bounds
					ListOut = Tail,
					MaxOut = MaxIn
				; T2 >= LeftBoundary, T1 =< RightBoundary, !,
					MaxOut is min(RightBoundary, T2),
					% there could be other intervals within the bounds so go on
					ListOut = Tail
				; T1 > RightBoundary, !,
					% since the list is sorted in ascending order, we're done
					MaxOut = MaxIn,
					ListOut = []
				)
		).

disect_results(Results, NDList, OkList, NOkList) :-
	(foreach(E, Results), 
		fromto([], NDIn, NDOut, NDList),
		fromto([], OkIn, OkOut, OkList),
		fromto([], NOkIn, NOkOut, NOkList) do
		E = I : Res,
		(Res = nondet, !, 
			append(NDIn, [I], NDOut),
			OkOut = OkIn,
			NOkOut = NOkIn
		; Res = ok, !,
			append(OkIn, [I], OkOut),
			NDOut = NDIn,
			NOkOut = NOkIn
		; Res = not_ok, !,
			append(NOkIn, [I], NOkOut),
			NDOut = NDIn,
			OkOut = OkIn
		)
	).
		
		
and_resvector(R1, R2, RAnd, OverallResult) :-
	% idea:
	% - distribute results to interval lists for ok, not_ok,
	%   and nondet
	% - not_ok intervals are known to be not_ok after and
	% - for each not_ok interval:
	%      * use get_intervals_within to subtract intersections
	%        from ok and nondet lists 
	% - the remaining intervals in the nondet list stay nondet
	% - for each nondet interval:
	%      * use get_intervals_within to subtract intersections
	%        from remaining ok list
	disect_results(R1, NDList1, OkList1, NOkList1),
	disect_results(R2, NDList2, OkList2, NOkList2),
	append(NDList1, NDList2, NDList3),
	sort(1, =<, NDList3, NDList4),
	append(OkList1, OkList2, OkList3),
	sort(1, =<, OkList3, OkList4),
	append(NOkList1, NOkList2, NOkList3),
	sort(1, =<, NOkList3, NOkList4),
	
	(foreach(NOk, NOkList4),
		fromto(OkList4, OkIn, OkOut, OkList5),
		fromto(NDList4, NDIn, NDOut, NDList5) do
			NOk = s(Start, End),
			get_intervals_within(OkIn, Start, End, _, OkOut),
			get_intervals_within(NDIn, Start, End, _, NDOut)			
	),
	(foreach(ND, NDList5),
		fromto(OkList5, OkIn, OkOut, OkList6) do
			ND = s(Start, End),
			get_intervals_within(OkIn, Start, End, _, OkOut)		
	),
	merge_goals(NDList5, NDList6),
	merge_goals(OkList6, OkList7),
	merge_goals(NOkList4, NOkList5),
	apply_unique_result(NDList6, nondet, Results1),
	apply_unique_result(OkList7, ok, Results2),
	apply_unique_result(NOkList5, not_ok, Results3),
	flatten([Results1, Results2, Results3], ResultsUnsorted),
	sort([1,1], =<, ResultsUnsorted, RAnd),
	get_unanimous_result(RAnd, OverallResult).
	

or_resvector(R1, R2, ROr, OverallResult) :-
	disect_results(R1, NDList1, OkList1, NOkList1),
	disect_results(R2, NDList2, OkList2, NOkList2),
	append(NDList1, NDList2, NDList3),
	sort(1, =<, NDList3, NDList4),
	append(OkList1, OkList2, OkList3),
	sort(1, =<, OkList3, OkList4),
	append(NOkList1, NOkList2, NOkList3),
	sort(1, =<, NOkList3, NOkList4),
	
	(foreach(Ok, OkList4),
		fromto(NOkList4, NOkIn, NOkOut, NOkList5),
		fromto(NDList4, NDIn, NDOut, NDList5) do
			Ok = s(Start, End),
			get_intervals_within(NOkIn, Start, End, _, NOkOut),
			get_intervals_within(NDIn, Start, End, _, NDOut)			
	),
	(foreach(ND, NDList5),
		fromto(NOkList5, NOkIn, NOkOut, NOkList6) do
			ND = s(Start, End),
			get_intervals_within(NOkIn, Start, End, _, NOkOut)		
	),
	merge_goals(NDList5, NDList6),
	merge_goals(OkList4, OkList5),
	merge_goals(NOkList6, NOkList7),
	apply_unique_result(NDList6, nondet, Results1),
	apply_unique_result(OkList5, ok, Results2),
	apply_unique_result(NOkList7, not_ok, Results3),
	flatten([Results1, Results2, Results3], ResultsUnsorted),
	sort([1,1], =<, ResultsUnsorted, ROr),
	get_unanimous_result(ROr, OverallResult).
						
					
					
				
	