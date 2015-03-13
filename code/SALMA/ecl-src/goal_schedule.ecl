get_goal_schedule_id(ToplevelFormula, ToSchedule, ScheduleParams, Id) :-
	store_get(goal_id_map, s(ToplevelFormula, ToSchedule, ScheduleParams), Id), !
	;
	incval(next_scheduled_goal_id),
	getval(next_scheduled_goal_id, Id).
	
apply_interval_decision_ok(ScheduleId, Start, End) :-
	% A positive result for Q was found at time End for
	% a formula that ranges from Start to End.
	% Close all matching intervals and add them to the ok list
	
	store_get(scheduled_goals,
		ScheduleId,
		i(NondetIntervals, OkIntervals, NotOkIntervals),
	(fromto(NondetIntervals, In, Out, []),
		fromto([], NondetNewIn, NondetNewOut, NondetIntervalsNew),
		fromto([], NewOkIntervalsIn, NewOkIntervalsOut, NewOkIntervals),
		param(Start, End) do
		In = [Goal | Rest],
		Goal = s(IStartTime1, IStartTime2, IEndTime),
		% TODO: are the first cases even possible?
		((IStartTime2 < Start, ! ; IStartTime1 > End), !,
			% interval not included
			append(NondetNewIn, [Goal], NondetNewOut),
			NewOkIntervalsOut = NewOkIntervalsIn
		; IStartTime1 < Start, IStartTime2 >= Start, 
			IStartTime2 =< End, !,
			% right side is enclosed in [Start, End]
			IStartTime2New is Start - 1,
			append(NondetNewIn, 
				[s(IStartTime1, IStartTime2New, IEndTime)],
				NondetNewOut),
			append(NewOkIntervalsIn, 
				[s(Start, IStartTime2, IEndTime)],
				NewOkIntervalsOut)
		; IStartTime1 < Start, IStartTime2 > End, !,
			% interior part of interval enclosed
			IStartTime2New is Start - 1,
			IStartTime3 is End + 1,
			append(NondetNewIn,
				[s(IStartTime1, IStartTime2New, IEndTime),
					s(IStartTime3, IStartTime2, IEndTime)],
				NondetNewOut),
			append(NewOkIntervalsIn,
				[s(Start, End, IEndTime)],
				NewOkIntervalsOut)
		; IStartTime1 >= Start, IStartTime2 =< End, !,
			% intervall fully enclosed
			NondetNewOut = NondetNewIn,
			append(NewOkIntervalsIn, [Goal], NewOkIntervalsOut)
		)
		
			
				
		
		
		
		
		
		
		

add_nondet_schedule_interval(ScheduleId, StartTime, EndTime) :-
	
	% Check whether the new start interval is contiguous 
	% to an existing one. If so, extend the interval.
	
	store_get(scheduled_goals,
		ScheduleId,
		i(NondetIntervals, OkIntervals, NotOkIntervals),
	
	% We assume that only nondet intervals can be updated	
	
	% sorting should not be necessary  due to construction scheme
	(fromto(NondetIntervals, In, Out, []), 
		fromto([], NondetNewIn, NondetNewOut, NondetIntervalsNew),
		fromto(false, F1, F2, Found),
		param(StartTime, EndTime, State) do
			In = [Goal | Rest],
			Goal = s(IStartTime1, IStartTime2, IEndTime), % remember: it's a nondet interval
			% check for interval order invariant
			(StartTime < IStartTime1 -> throw(starttime_before_istarttime1) ; true),
			(EndTime < IEndTime -> throw(endtime_before_iendtime) ; true),
			(StartTime =< IStartTime2 + 1 ->
				% found contiguous interval -> merge
				NewStartTime2 is max(IStartTime2, StartTime),
				append(NondetNewIn, 
					[s(IStartTime1, NewStartTime2, EndTime) | Rest],
					NondetNewOut),
				Out = [],
				F2 = true
			;
				append(NondetNewIn, [Goal], NondetNewOut),
				Out = Rest,
				F2 = false
			)
	),
	(Found = true ->
		NondetIntervalsResult = NondetIntervalsNew
		;
		% we found no contiguous interval -> add new one
		append(NondetIntervals, [s(StartTime1, StartTime2, EndTime)],
			NondetIntervalsNew)
	),
	store_set(scheduled_goals,
		ScheduleId, i(NondetIntervalsNew, OkIntervals, NotOkIntervals)).