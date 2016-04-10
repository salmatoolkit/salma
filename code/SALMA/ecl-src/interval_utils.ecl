
% Returns the same result for all given start time intervals.
apply_unique_result(StartTimes, Result, Results) :-
        /* $D$ */ 
        (foreach(Entry, StartTimes), foreach(R, Results),
         param(Result) do
            /* $D$ */ 
            R = Entry : Result
        ).	

% negates all results in OrigResults≈‚
negate_results(OrigResults, Results) :-
        /* $D$ */ 
        (foreach(Entry, OrigResults), foreach(R, Results) do
            /* $D$ */ 
            Entry = Interval : OldR,
            (OldR = nondet, /* $D$ */ !, 
             NewR = nondet
            ; OldR = ok, /* $D$ */ !, 
              NewR = not_ok 
            ; OldR = not_ok, /* $D$ */ !, 
              NewR = ok
            ;
             throw(unsupported_result(OldR))
            ),
            R = Interval : NewR
        ).		
	


% precondition: StartTimes sorted
% LeftBoundary and RightBoundary can both be inf to express
% an open interval in the respective direction.
apply_result_within_interval(StartTimes, Result,
                             LeftBoundary, RightBoundary, 
                             UnhandledStartTimes, Results) :-
        /* $D$ */ 
        (foreach(Entry, StartTimes), fromto([], STOld, STNew, UnhandledStartTimes),
         fromto([], RIn, ROut, Results), param(LeftBoundary, RightBoundary, Result) do
            /* $D$ */ 
            get_interval_intersection(Entry, 
                                      LeftBoundary, RightBoundary, Intersection, Rest),
            (Intersection = none ->
                /* $D$ */ 
                ROut = RIn
            ;
                /* $D$ */ 
                append(RIn, [Intersection : Result], ROut)
            ), 
            append(STOld, Rest, STNew)			
        ).
	

% Returns a concrete verdict if all results in OrigResults are either ok or not_ok. Otherwise,
% if at least one result is nondet, nondet is returned. In the last case where no nodet is contained but there are both
% ok and not_ok entries, ambiguous is returned.
get_unanimous_result(OrigResults, Result) :-
        /* $D$ */ 
        (fromto(OrigResults, In, Out, []), fromto(none, R1, R2, Result) do
            /* $D$ */ 
            In = [First | Rest],
            First = _ : Res,
            
            (Res = nondet, /* $D$ */ !,
             Out = [], R2 = nondet
            ; R1 = none, /* $D$ */ !,
              Out = Rest, R2 = Res
            ; R1 = Res, /* $D$ */ !, 
              Out = Rest, R2 = R1
            ; R1 \= none, R1 \= Res, /* $D$ */ !,
              Out = Rest, R2 = ambiguous  % nondet has precedence so don't break loop here!
            )
        ).

	
	
	
% precondition for merge_goals:
% list sorted ascending
merge_goals([], []) :- /* $D$ */ true.
merge_goals([Goal], [Goal]) :- true, /* $D$ */ !.
merge_goals([First | Rest], MergedGoals) :-
        /* $D$ */ 
        merge_goals_rec(First, Rest, MergedGoals).

merge_goals_rec(First, [], [First]) :- /* $D$ */ true.
merge_goals_rec(First, [Second | Rest], MergedGoals) :-
        /* $D$ */ 
        First = s(Start1_1, Start1_2),
        Second = s(Start2_1, Start2_2),
        % remember: intervals are sorted
        (Start2_1 =< Start1_2 + 1 ->
            % found contiguous intervals -> merge
            /* $D$ */ 
            NewStart2 is max(Start1_2, Start2_2),
            MergedInterval = s(Start1_1, NewStart2),
            merge_goals_rec(MergedInterval, Rest, MergedGoals)
        ;
            % not contiguous
            /* $D$ */ 
            merge_goals_rec(Second, Rest, RestMergedGoals),
            append([First], RestMergedGoals, MergedGoals)
        ).
	

	
	
% returns the intersection of the intervals as s(S,E) term or none if there is none.
% Int is has to be a s(S,E) terms. Start and End specify the interval that
% is intersected with Int. Both S and E could be inf.
get_interval_intersection(Int, Start, End, Intersection, Rest) :-
        /* $D$ */ 
        Int = s(T1, T2),
        (
            Start = inf, End = inf, /* $D$ */ !, 
            Intersection = s(T1, T2),
            Rest = []
        ;
            Start = inf, End \= inf, /* $D$ */ !, 
            (
                T2 =< End, /* $D$ */ !, 
                Intersection = s(T1, T2),
                Rest = []
            ;
                T1 =< End, T2 > End, /* $D$ */ !,
                Intersection = s(T1, End),
                RStart is End + 1,
                Rest = [s(RStart, T2)]
            ;
                T1 > End, /* $D$ */ !,
                Intersection = none,
                Rest = [Int]
            )
        ;
            Start \= inf, End = inf, /* $D$ */ !, 
            (
                T1 >= Start, /* $D$ */ !,
                Intersection = s(T1, T2),
                Rest = []
            ;
                T1 < Start, T2 >= Start, /* $D$ */ !,
                Intersection = s(Start, T2),
                REnd is Start - 1,
                Rest = [s(T1, REnd)]
            ;
                T2 < Start, /* $D$ */ !,
                Intersection = none,
                Rest = [Int]
            )
        ; % both Start and End are numeric
	    /* $D$ */ 
            (
                T2 < Start, /* $D$ */ !,
                Intersection = none,
                Rest = [Int]
            ;
                T1 < Start , T2 >= Start, T2 =< End, /* $D$ */ !,
                Intersection = s(Start, T2),
                REnd is Start - 1,
                Rest = [s(T1, REnd)]
            ;
                T1 < Start, T2 > End, /* $D$ */ !,
                Intersection = s(Start, End),
                R1End is Start - 1,
                R2Start is End + 1,
                Rest = [s(T1, R1End), s(R2Start, T2)]
            ;
                T1 >= Start, T2 =< End, /* $D$ */ !,
                Intersection = s(T1, T2),
                Rest = []
            ;
                T1 >= Start, T1 =< End, T2 > End, /* $D$ */ !,
                Intersection = s(T1, End),
                RStart is End + 1,
                Rest = [s(RStart, T2)]
            ;
                T1 > End, /* $D$ */ !,
                Intersection = none,
                Rest = [Int]
            )
        ).
	

% returns the interval list in Selection that is selected from IntervalList by [Start, End]
get_intervals_within(IntervalList, Start, End, Selection, 
                     Remaining) :-
        /* $D$ */ 
        (foreach(I, IntervalList), fromto([], In, Out, Selection),
         fromto([], R1, R2, Remaining),
         param(Start, End) do	
            /* $D$ */ 
            get_interval_intersection(I, Start, End,
                                      Intersection, Rest),
            (Intersection = none ->
                /* $D$ */ 
                Out = In
            ;
                /* $D$ */ 
                append(In, [Intersection], Out)
            ),
            append(R1, Rest, R2)
        ).

% returns the firs interval from IntervalList that is selected by [Start, End]
get_first_interval_within(IntervalList, Start, End, Selection, 
                          Remaining) :-
        /* $D$ */ 
        (fromto(IntervalList, IntIn, IntOut, []), 
         fromto(none, _, SelOut, Selection),
         fromto([], R1, R2, Remaining),
         param(Start, End) do	
            /* $D$ */ 
            IntIn = [I | Tail],
            get_interval_intersection(I, Start, End,
                                      SelOut, Rest),
            (SelOut = none ->
                /* $D$ */ 
                IntOut = Tail,
                append(R1, [I], R2)
            ;
                /* $D$ */ 
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
        /* $D$ */ 
        (fromto(IntervalList, ILLast, ILNext, []),
         fromto(none, _, IntOut, Intersection),
         param(RightBoundary) do
            /* $D$ */ 
            ILLast = [Interval | Tail],
            Interval = s(Start, End),
            (Start =< RightBoundary, End >= RightBoundary ->
	        /* $D$ */ 
                IntOut = s(Start, RightBoundary),
                ILNext = []
            ;
                /* $D$ */ 
                IntOut = none,
                ILNext = Tail
            )	
        ).

% Returns the highest point that is included in an interval from IntervalList
% and within the given bounds. If no interval intersects the bounds then "none" is returned.
get_max_interval_point_within(IntervalList, 
                              LeftBoundary, RightBoundary, Max) :-
        % assumption: IntervalList sorted in ascending order
        /* $D$ */ 
        (fromto(IntervalList, ListIn, ListOut, []),
         fromto(none, MaxIn, MaxOut, Max),
         param(LeftBoundary, RightBoundary) do
            /* $D$ */ 
            ListIn =  [s(T1, T2) | Tail],
            (T2 < LeftBoundary, /* $D$ */ !,
             % interval left from bounds
             ListOut = Tail,
             MaxOut = MaxIn
            ; T2 >= LeftBoundary, T1 =< RightBoundary, /* $D$ */ !,
              MaxOut is min(RightBoundary, T2),
              % there could be other intervals within the bounds so go on
              ListOut = Tail
            ; T1 > RightBoundary, /* $D$ */ !,
              % since the list is sorted in ascending order, we're done
              MaxOut = MaxIn,
              ListOut = []
            )
        ).

% partitions the result mapping in Result into three separate mappings according to the labels
disect_results(Results, NDList, OkList, NOkList) :-
        /* $D$ */ 
        (foreach(E, Results), 
         fromto([], NDIn, NDOut, NDList),
         fromto([], OkIn, OkOut, OkList),
         fromto([], NOkIn, NOkOut, NOkList) do
            /* $D$ */ 
            E = I : Res,
            (Res = nondet, /* $D$ */ !, 
             append(NDIn, [I], NDOut),
             OkOut = OkIn,
             NOkOut = NOkIn
            ; Res = ok, /* $D$ */ !,
              append(OkIn, [I], OkOut),
              NDOut = NDIn,
              NOkOut = NOkIn
            ; Res = not_ok, /* $D$ */ !,
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
        /* $D$ */ 
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
            /* $D$ */ 
            NOk = s(Start, End),
            get_intervals_within(OkIn, Start, End, _, OkOut),
            get_intervals_within(NDIn, Start, End, _, NDOut)			
        ),
        (foreach(ND, NDList5),
         fromto(OkList5, OkIn, OkOut, OkList6) do
            /* $D$ */ 
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
        /* $D$ */ 
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
            /* $D$ */ 
            Ok = s(Start, End),
            get_intervals_within(NOkIn, Start, End, _, NOkOut),
            get_intervals_within(NDIn, Start, End, _, NDOut)			
        ),
        (foreach(ND, NDList5),
         fromto(NOkList5, NOkIn, NOkOut, NOkList6) do
            /* $D$ */ 
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
						
					
					
				
	