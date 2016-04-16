% Looks up a termgoal instance with given paramters and returns its id's if the goal instance
% already exists in the schedule. Otherwise, a new id is created.
get_goal_schedule_id(ToplevelFormula, Level, ToSchedule, ScheduleParams, Id) :-
        /* $D$ */ 
        store_get(goal_id_map, s(ToplevelFormula, Level, ToSchedule, ScheduleParams), Id), /* $D$ */ !
	; % it's a new goal
        /* $D$ */ 
	incval(next_scheduled_goal_id),
	getval(next_scheduled_goal_id, Id),
	Description = s(ToplevelFormula, Level, ToSchedule, ScheduleParams),
	store_set(goal_id_map, Description, Id),
	store_set(scheduled_goal_descriptions, Id, Description),
	store_set(scheduled_goals,
		g(Level, Id), i([], [], [], -1)). 


% Returns the goal description for the given schedule id.
get_scheduled_goal_description(ScheduleId, Description) :-
        /* $D$ */ 
        store_get(scheduled_goal_descriptions, ScheduleId, Description), /* $D$ */ !
	;
        /* $D(IGNORED)$ */ 
	throw(unregistered_scheduled_goal(ScheduleId)).
	

% Returns all scheduled goal instances within the given interval [Start, End]. 	
get_scheduled_intervals_within(PSchedId, Level,
                               Start, End, 
                               NondetIntervals, OkIntervals, NotOkIntervals) :-
        /* $D$ */ 
        (store_get(scheduled_goals, g(Level, PSchedId), 
                   i(NondetIntervalsAll, OkIntervalsAll, NotOkIntervalsAll, _)), !
        ; throw(unregistered_scheduled_goal(PSchedId))),
		
	get_intervals_within(NondetIntervalsAll, 
		Start, End, NondetIntervals, _),
	get_intervals_within(OkIntervalsAll, 
		Start, End, OkIntervals, _),
	get_intervals_within(NotOkIntervalsAll, 
		Start, End, NotOkIntervals, _).
	

% adds a decision interval to the corresponding OK/NOT_OK interval list and
% subtracts the corresponding intersection from the NONET list
apply_one_interval_decision(NondetIntervals, OkIntervals, NotOkIntervals,
                            Start, End, Decision, 
                            NondetIntervals2, OkIntervals2, NotOkIntervals2) :-
        /* $D$ */ 
        (fromto(NondetIntervals, In, Out, []),
         fromto([], NondetNewIn, NondetNewOut, NondetIntervals2),
         param(Start, End) do
            /* $D$ */ 
            In = [Goal | Out],
            get_interval_intersection(Goal, Start, End,
                                      _, RestOfIntersection),
            append(NondetNewIn, RestOfIntersection, NondetNewOut)
        ),
        (Decision = ok ->
            /* $D$ */ 
            append(OkIntervals, [s(Start, End)], OkIntervals2),
            NotOkIntervals2 = NotOkIntervals
        ;
            /* $D$ */ 
            append(NotOkIntervals,  [s(Start, End)], NotOkIntervals2),
            OkIntervals2 = OkIntervals
        ).	
	
	
	
apply_interval_decisions(ScheduleId, Level, Decisions, EvalEndTime) :-
	% Decisions is a list of tuples s(Start, End) : Result
	% a formula that ranges from Start to End.
        % Close all matching intervals and add them to the ok list
	/* $D$ */ 
	(store_get(scheduled_goals,
                   g(Level, ScheduleId),
                   i(NondetIntervals, OkIntervals, NotOkIntervals, _)) , !
        ; throw(unregistered_scheduled_goal(ScheduleId))),
        (foreach(D, Decisions), 
         fromto(NondetIntervals, In1, Out1, NondetIntervalsNew),
         fromto(OkIntervals, In2, Out2, OkIntervalsUnsorted),
         fromto(NotOkIntervals, In3, Out3, NotOkIntervalsUnsorted), 
         fromto(false, OkChangedIn, OkChangedOut, OkChanged),
         fromto(false, NotOkChangedIn, NotOkChangedOut, NotOkChanged) do
            /* $D$ */ 
            D = s(Start, End) : Res,
            (Res = nondet -> % just go ahead for nondet
                /* $D$ */ 
                Out1 = In1, Out2 = In2, Out3 = In3,
                OkChangedOut = OkChangedIn, NotOkChangedOut = NotOkChangedIn
            ; 				
                /* $D$ */ 
                apply_one_interval_decision(In1, In2, In3,
                                            Start, End, Res, Out1, Out2, Out3),
                (Res = ok -> /* $D$ */ OkChangedOut = true ; /* $D$ */ OkChangedOut = OkChangedIn),
                (Res = not_ok -> /* $D$ */ NotOkChangedOut = true ; 
                                           /* $D$ */ NotOkChangedOut = NotOkChangedIn)									
            )
        ),
	(OkChanged = true ->
            /* $D$ */ 
            sort(0, =<, OkIntervalsUnsorted, OkIntervalsUnmerged),
            merge_goals(OkIntervalsUnmerged, OkIntervals2)
        ;
	    /* $D$ */ 
            OkIntervals2 = OkIntervalsUnsorted
        ),
        (NotOkChanged = true ->
            /* $D$ */ 
            sort(0, =<, NotOkIntervalsUnsorted, NotOkIntervalsUnmerged),
            merge_goals(NotOkIntervalsUnmerged, NotOkIntervals2)
        ;
            /* $D$ */ 
            NotOkIntervals2 = NotOkIntervalsUnsorted		
        ),	
        store_set(scheduled_goals,
                  g(Level, ScheduleId), i(NondetIntervalsNew, OkIntervals2, 
                                          NotOkIntervals2, EvalEndTime)).
			

add_nondet_schedule_interval(ScheduleId, Level, StartTime, EvalEndTime) :-
	
	% Check whether the new start interval is contiguous 
	% to an existing one. If so, extend the interval.
        /* $D$ */ 
	(store_get(scheduled_goals,
                   g(Level, ScheduleId),
                   i(NondetIntervals, OkIntervals, NotOkIntervals, _)), !
        ; % no entry found for id 
         throw(unregistered_scheduled_goal(add_nondet_schedule_interval, ScheduleId))),	
	% We assume that only nondet intervals can be updated	
        
        % sorting should not be necessary  due to construction scheme
        (fromto(NondetIntervals, In, Out, []), 
         fromto([], NondetNewIn, NondetNewOut, NondetIntervalsNew),
         fromto(false, _, F2, Found),
         param(StartTime) do
            /* $D$ */ 
            In = [Goal | Rest],
            Goal = s(IStartTime1, IStartTime2), % remember: it's a nondet interval
                                                % check for interval order invariant
            (StartTime < IStartTime1 -> throw(starttime_before_istarttime1) ; true),
            (StartTime =< IStartTime2 + 1 ->
                % found contiguous interval -> merge
                /* $D$ */ 
                NewStartTime2 is max(IStartTime2, StartTime),
                append(NondetNewIn, 
                       [s(IStartTime1, NewStartTime2) | Rest],
                       NondetNewOut),
                Out = [],
                F2 = true
            ;
                /* $D$ */ 
                append(NondetNewIn, [Goal], NondetNewOut),
                Out = Rest,
                F2 = false
            )
        ),
        (Found = true ->
            /* $D$ */ 
            NondetIntervalsResult = NondetIntervalsNew
        ;
            % we found no contiguous interval -> add new one
            /* $D$ */ 
            append(NondetIntervals, [s(StartTime, StartTime)],
                   NondetIntervalsResult)
        ),
	store_set(scheduled_goals,
                  g(Level, ScheduleId), i(NondetIntervalsResult, OkIntervals, NotOkIntervals,
                                          EvalEndTime)).


check_state_filter(StateVector, StateFilter) :-
        StateFilter = all, /* $D$ */ !
        ;
        /* $D$ */ 
        StateVector = i(NondetIntervals, OkIntervals, NotOkIntervals, _),
        (
            StateFilter = nondet, /* $D$ */ !,
            length(NondetIntervals) > 0
        ;
            StateFilter = ok, /* $D$ */ !,
            length(OkIntervals) > 0
        ;
            StateFilter = not_ok, /* $D$ */ !,
            length(NotOkIntervals) > 0
        ).

get_scheduled_goals(ScheduledGoals, StateFilter, LevelFilter) :-
        /* $D$ */ 
        stored_keys_and_values(scheduled_goals, L),
        (foreach(Entry, L), fromto([], In, Out, ScheduledGoals), 
         param(StateFilter, LevelFilter) do
            /* $D$ */ 
            Entry = Key - StateVector,
            Key = g(Level, _),
            (  
                ((LevelFilter = all, ! ; Level == LevelFilter),
                 check_state_filter(StateVector, StateFilter) ) ->			 
                /* $D$ */ 
                append(In, [Entry], Out)
            ;
                /* $D$ */ 
                Out = In
            )		
        ).	

get_pending_goals(PendingGoals, LevelFilter) :-
        /* $D$ */ 
        get_scheduled_goals(PendingGoals, nondet, LevelFilter).
		
get_pending_toplevel_goals(PendingGoals) :-
	/* $D$ */ 
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
		printf(Stream, "%5d %10s %5d %10w %15w %w\n",[Level, Name, Id, Term, Params, State])
	).