get_goal_schedule_id(ToplevelFormula, ToSchedule, ScheduleParams, Id) :-
	store_get(goal_id_map, s(ToplevelFormula, ToSchedule, ScheduleParams), Id), !
	;
	incval(next_scheduled_goal_id),
	getval(next_scheduled_goal_id, Id),
	store_set(goal_id_map, s(ToplevelFormula, ToSchedule, ScheduleParams), Id).

	
% precondition for merge_goals:
% list sorted ascending
merge_goals([], []).
merge_goals([Goal], [Goal]).
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
			
	
apply_interval_decision(ScheduleId, Start, End, EvalEndTime, Decision) :-
	% A positive result for Q was found at time End for
	% a formula that ranges from Start to End.
	% Close all matching intervals and add them to the ok list
	
	store_get(scheduled_goals,
		ScheduleId,
		i(NondetIntervals, OkIntervals, NotOkIntervals, 
			_)),
	(fromto(NondetIntervals, In, Out, []),
		fromto([], NondetNewIn, NondetNewOut, NondetIntervalsNew),
		fromto([], NewIntervalsIn, NewIntervalsOut, NewIntervals),
		param(Start, End) do
		In = [Goal | Rest],
		Out = Rest,
		Goal = s(IStartTime1, IStartTime2),
		% TODO: are the first cases even possible?
		((IStartTime2 < Start, ! ; IStartTime1 > End), !,
			% interval not included
			append(NondetNewIn, [Goal], NondetNewOut),
			NewIntervalsOut = NewIntervalsIn
		; IStartTime1 < Start, IStartTime2 >= Start, 
			IStartTime2 =< End, !,
			% right side is enclosed in [Start, End]
			IStartTime2New is Start - 1,
			append(NondetNewIn, 
				[s(IStartTime1, IStartTime2New)],
				NondetNewOut),
			append(NewIntervalsIn, 
				[s(Start, IStartTime2)],
				NewIntervalsOut)
		; IStartTime1 < Start, IStartTime2 > End, !,
			% interior part of interval enclosed
			IStartTime2New is Start - 1,
			IStartTime3 is End + 1,
			append(NondetNewIn,
				[s(IStartTime1, IStartTime2New),
					s(IStartTime3, IStartTime2)],
				NondetNewOut),
			append(NewIntervalsIn,
				[s(Start, End)],
				NewIntervalsOut)
		; IStartTime1 >= Start, IStartTime2 =< End, !,
			% intervall fully enclosed
			NondetNewOut = NondetNewIn,
			append(NewIntervalsIn, [Goal], NewIntervalsOut)
		)
	),
	(Decision = ok ->
		append(OkIntervals, NewIntervals, OkIntervalsUnsorted),
		sort(0, =<, OkIntervalsUnsorted, OkIntervalsUnmerged),
		merge_goals(OkIntervalsUnmerged, OkIntervals2),
		NotOkIntervals2 = NotOkIntervals
		;
		append(NotOkIntervals, NewIntervals, NotOkIntervalsUnsorted),
		sort(0, =<, NotOkIntervalsUnsorted, NotOkIntervalsUnmerged),
		merge_goals(NotOkIntervalsUnmerged, NotOkIntervals2),
		OkIntervals2 = OkIntervals
	),	
	store_set(scheduled_goals,
		ScheduleId, i(NondetIntervalsNew, OkIntervals2, 
		NotOkIntervals2, EvalEndTime)).
			

add_nondet_schedule_interval(ScheduleId, StartTime, EvalEndTime) :-
	
	% Check whether the new start interval is contiguous 
	% to an existing one. If so, extend the interval.
	
	(store_get(scheduled_goals,
		ScheduleId,
		i(NondetIntervals, OkIntervals, NotOkIntervals, _)), !
	; % no entry found for id 
	NondetIntervals = [], OkIntervals = [], NotOkIntervals = []
	),	
	% We assume that only nondet intervals can be updated	
	
	% sorting should not be necessary  due to construction scheme
	(fromto(NondetIntervals, In, Out, []), 
		fromto([], NondetNewIn, NondetNewOut, NondetIntervalsNew),
		fromto(false, _, F2, Found),
		param(StartTime, EndTime) do
			In = [Goal | Rest],
			Goal = s(IStartTime1, IStartTime2), % remember: it's a nondet interval
			% check for interval order invariant
			(StartTime < IStartTime1 -> throw(starttime_before_istarttime1) ; true),
			(StartTime =< IStartTime2 + 1 ->
				% found contiguous interval -> merge
				NewStartTime2 is max(IStartTime2, StartTime),
				append(NondetNewIn, 
					[s(IStartTime1, NewStartTime2) | Rest],
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
		append(NondetIntervals, [s(StartTime, StartTime)],
			NondetIntervalsResult)
	),
	store_set(scheduled_goals,
		ScheduleId, i(NondetIntervalsResult, OkIntervals, NotOkIntervals,
						EvalEndTime)).


check_state_filter(Content, StateFilter) :-
	StateFilter = all, !
	;
	StateFilter = nondet, !,
	Content \= ok, 
	Content \= not_ok
	; % ok / not_ok				
	StateFilter = Content.		
		
get_scheduled_goals(ScheduledGoals, StateFilter, LevelFilter) :-
	stored_keys_and_values(scheduled_goals, L),
	(foreach(Entry, L), fromto([], In, Out, ScheduledGoals), param(StateFilter, LevelFilter) do
		Entry = Key - app(_, Content),
		Key = sg(_, Level, _, _, _),
		(  
			((LevelFilter = all, ! ; Level == LevelFilter),
				check_state_filter(Content, StateFilter) ) ->			 
				append(In, [Key], Out)
				;
				Out = In
		)		
	).	
	
get_pending_goals(PendingGoals, LevelFilter) :-
	get_scheduled_goals(PendingGoals, nondet, LevelFilter).
		
get_pending_toplevel_goals(PendingGoals) :-
	get_pending_goals(PendingGoals, 0).
	
	
% TODO: proper cleanup-procedure
print_scheduled_goals(Stream, SortPositions) :-
	stored_keys(scheduled_goals, Keys),
	sort(SortPositions, =<, Keys, SortedKeys),
	nl,
	printf(Stream, "%10s %10s %10s %5s %5s %10s %s\n",["Name", "Time","End Time", "Level","Id","Params","Term"]),
	printf(Stream, "-------------------------------------------------------\n",[]),
	(foreach(Key, SortedKeys), param(Stream) do
		Key = sg(Name, Level, Id, T, EndTime),
		store_get(scheduled_goals, Key, app(Params, F)),
		
		printf(Stream, "%10s %10d %10d %5d %5d %w %w\n",[Name, T, EndTime, Level, Id, Params, F])
	).