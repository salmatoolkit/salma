:- dynamic domain/2.
:- dynamic domain/3.
:- dynamic sort/1.
:- dynamic sorts/1.
:- dynamic dynamic_sort/1.
:- dynamic subsort/2.
:- dynamic subsorts/2.
:- dynamic sort_hierarchy_unsynced/0.
:- local variable(next_object_id, 1).

sort(sort).

domain(Sort, D) :- domain(Sort, D, s0).

fluent(domain, [s:sort], list).

domain(Sort, D, s0) :- get_current(sort, [Sort], D).		
domain(Sort, D, slast) :- get_last(sort, [Sort], D).

remove_duplicates(L1, L2) :-
	% (foreach(E, L1), fromto([], In, Out, L2) do
		% (member(E, In) -> 
			% Out = In
			% ;
			% append(In, [E], Out)
		% )
	% ).
	sort(L1, L2).
	
get_all_sorts(Sorts) :-
	findall(SList, sorts(SList), SListList),
	flatten(SListList, Sorts1),
	findall(S, sort(S), Sorts2),
	append(Sorts1, Sorts2, RawSorts),
	remove_duplicates(RawSorts, Sorts).

get_all_domains(Domains) :-
	get_all_sorts(Sorts),
	(foreach(Sort, Sorts), foreach(D, Domains) do
		domain(Sort, Entities),
		D = Sort : Entities
	).
	
isSortOf(Object, Sort) :-
	domain(Sort, D) ->
		member(Object, D)
	;
	throw(unregistered_domain(Sort)).

% TODO: communication
combineDomains(Domains, CombinedDomain) :-
	(foreach(D, Domains), fromto([], D2, D3, CombinedDomain) do
		domain(D, Entities),
		append(D2, Entities, D3)
	).

is_uninitialized(Sort, AllSorts) :-
	member(Sort, AllSorts),
	not get_current(domain, [Sort], _).
	
init_uninitialized_sorts :-
	get_all_sorts(AllSorts),
	findall(S, is_uninitialized(S, AllSorts), Sorts),
	(foreach(S, Sorts) do
		set_current(domain, [], []).
	).

get_transitive_domain(Sort, Domain) :-
	findall(D, subsort(D, Sort), SubSorts1),
	findall(D, subsorts(D, Sort), SubSorts2),
	flatten(SubSorts2, FlatSubSorts2),
	append(SubSorts1, FlatSubSorts2, SubSortsTemp),
	remove_duplicates(SubSortsTemp, SubSorts),
	(domain(Sort, D1), ! ; D1 = []),
	(foreach(SubSort, SubSorts), fromto(D1, In, Out, DomainTemp) do
		get_transitive_domain(SubSort, SD),
		append(In, SD, Out),
	),
	remove_duplicates(DomainTemp, Domain).

set_sort_hierarchy_unsynced(IsUnsynced) :-
	(IsUnsynced = true ->
		(not(sort_hierarchy_unsynced) -> assert(sort_hierarchy_unsynced) ; true)
		;
		(sort_hierarchy_unsynced -> retract(sort_hierarchy_unsynced) ; true)
	).	
		
	
init_sort_hierarchy(Domains) :-
	get_all_sorts(Sorts),
	set_current(domain, [sort], Sorts),
	init_uninitialized_sorts,
	(foreach(Sort, Sorts) do
		get_transitive_domain(Sort, Domain),
		set_current(domain, [Sort], Domain)
	),
	set_sort_hierarchy_unsynced(false),
	get_all_domains(Domains).
	
	