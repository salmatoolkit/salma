get_goal_schedule_id(ToplevelFormula, Level, ToSchedule, ScheduleParams, Id) :-
	store_get(goal_id_map, s(ToplevelFormula, Level, ToSchedule, ScheduleParams), Id), !
	; % it's a new goal
	incval(next_scheduled_goal_id),
	getval(next_scheduled_goal_id, Id),
	Description = s(ToplevelFormula, Level, ToSchedule, ScheduleParams),
	store_set(goal_id_map, Description, Id),
	store_set(scheduled_goal_descriptions, Id, Description). 

get_scheduled_goal_description(ScheduleId, Description) :-
	store_get(scheduled_goal_descriptions, ScheduleId, Description)
	;
	throw(unregistered_scheduled_goal(ScheduleId)).
	
	
get_scheduled_intervals_within(PSchedId, Level,
	Start, End, 
	NondetIntervals, OkIntervals, NotOkIntervals) :-
	store_get(scheduled_goals, g(Level, PSchedId), 
		i(NondetIntervalsAll, OkIntervalsAll, NotOkIntervalsAll, _)),
	get_intervals_within(NondetIntervalsAll, 
		Start, End, NondetIntervals),
	get_intervals_within(OkIntervalsAll, 
		Start, End, OkIntervals),
	get_intervals_within(NotOkIntervalsAll, 
		Start, End, NotOkIntervals).
	
		

	
	
apply_one_interval_decision(NondetIntervals, OkIntervals, NotOkIntervals,
	Start, End, Decision, 
	NondetIntervals2, OkIntervals2, NotOkIntervals2) :-
	(fromto(NondetIntervals, In, Out, []),
		fromto([], NondetNewIn, NondetNewOut, NondetIntervals2),
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
		append(OkIntervals, NewIntervals, OkIntervals2),
		NotOkIntervals2 = NotOkIntervals
		;
		append(NotOkIntervals, NewIntervals, NotOkIntervals2),
		OkIntervals2 = OkIntervals
	).	
	
	
	
apply_interval_decisions(ScheduleId, Level, Decisions, EvalEndTime) :-
	% Decisions is a list of tuples s(Start, End) : Result
	% a formula that ranges from Start to End.
	% Close all matching intervals and add them to the ok list
	
	store_get(scheduled_goals,
		g(Level, ScheduleId),
		i(NondetIntervals, OkIntervals, NotOkIntervals, _)),
	(foreach(D, Decisions), 
		fromto(NondetIntervals, In1, Out1, NondetIntervalsNew),
		fromto(OkIntervals, In2, Out2, OkIntervalsUnsorted),
		fromto(NotOkIntervals, In3, Out3, NotOkIntervalsUnsorted), 
		fromto(false, OkChangedIn, OkChangedOut, OkChanged),
		fromto(false, NotOkChangedIn, NotOkChangedOut, NotOkChanged) do
			D = s(Start, End) : Res,
			(Res = nondet -> % just go ahead for nondet
				Out1 = In1, Out2 = In2, Out3 = In3,
				OkChangedOut = OkChangedIn, NotOkChangedOut = NotOkChangedIn
			; 				
				apply_one_interval_decision(In1, In2, In3,
					Start, End, Res, Out1, Out2, Out3),
				(Res = ok -> OkChangedOut = true ; OkChangedOut = OkChangedIn),
				(Res = not_ok -> NotOkChangedOut = true ; 
									NotOkChangedOut = NotOkChangedIn)									
			)
	),
	(OkChanged = true ->
		sort(0, =<, OkIntervalsUnsorted, OkIntervalsUnmerged),
		merge_goals(OkIntervalsUnmerged, OkIntervals2)
		;
		OkIntervals2 = OkIntervalsUnsorted
	),
	(NotOkChanged = true ->
		sort(0, =<, NotOkIntervalsUnsorted, NotOkIntervalsUnmerged),
		merge_goals(NotOkIntervalsUnmerged, NotOkIntervals2)
		;
		NotOkIntervals2 = NotOkIntervalsUnsorted		
	),	
	store_set(scheduled_goals,
		g(Level, ScheduleId), i(NondetIntervalsNew, OkIntervals2, 
		NotOkIntervals2, EvalEndTime)).
			

add_nondet_schedule_interval(ScheduleId, Level, StartTime, EvalEndTime) :-
	
	% Check whether the new start interval is contiguous 
	% to an existing one. If so, extend the interval.
	
	(store_get(scheduled_goals,
		g(Level, ScheduleId),
		i(NondetIntervals, OkIntervals, NotOkIntervals, _)), !
	; % no entry found for id 
	NondetIntervals = [], OkIntervals = [], NotOkIntervals = []
	),	
	% We assume that only nondet intervals can be updated	
	
	% sorting should not be necessary  due to construction scheme
	(fromto(NondetIntervals, In, Out, []), 
		fromto([], NondetNewIn, NondetNewOut, NondetIntervalsNew),
		fromto(false, _, F2, Found),
		param(StartTime) do
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
		g(Level, ScheduleId), i(NondetIntervalsResult, OkIntervals, NotOkIntervals,
						EvalEndTime)).


check_state_filter(StateVector, StateFilter) :-
	StateFilter = all, !
	;
	StateVector = i(NondetIntervals, OkIntervals, NotOkIntervals, _),
	(
		StateFilter = nondet, !,
		length(NondetIntervals) > 0
		;
		StateFilter = ok, !,
		length(OkIntervals) > 0
		;
		StateFilter = not_ok, !,
		length(NotOkIntervals) > 0
	).
		
get_scheduled_goals(ScheduledGoals, StateFilter, LevelFilter) :-
	stored_keys_and_values(scheduled_goals, L),
	(foreach(Entry, L), fromto([], In, Out, ScheduledGoals), 
	param(StateFilter, LevelFilter) do
		Entry = Key - StateVector,
		Key = g(Level, _),
		(  
			((LevelFilter = all, ! ; Level == LevelFilter),
				check_state_filter(StateVector, StateFilter) ) ->			 
				append(In, [Entry], Out)
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
	stored_keys_and_values(scheduled_goals, Goals),
	(foreach(G, Goals), foreach(L, Lines) do
		G = g(Level, ScheduleId) - State,
		get_scheduled_goal_description(ScheduleId, Description),
		Description = s(ToplevelFormula, _, ToSchedule, ScheduleParams),
		L = l(Level, ToplevelFormula, ScheduleId, ToSchedule, ScheduleParams, State)
	),	
	sort(SortPositions, =<, Lines, SortedLines),
	nl,
	printf(Stream, "%5s %10s %5s %10s %15s %s\n", ["Level", "Name", "Id", "Term", "Params", "State"]),
	printf(Stream, "-------------------------------------------------------\n",[]),
	(foreach(Line, SortedLines), param(Stream) do		
		Line = l(Level, Name, Id, Term, Params, State),
		printf(Stream, "%5d %10s %5d %10s %15s %s\n",[Level, Name, Id, Term, Params, State])
	).