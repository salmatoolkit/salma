domain(robot,D) :-
        D=[rob1,rob2,rob3].

domain(item, D) :- D=[item1, item2].
charged(rob1).
charged(rob2).
charged(rob3).

active(rob1).
active(rob2).
active(rob3).


moving(rob1).
moving(rob2).






test1(T2) :- subst_in_term(x,bla, foo(a, bar(x)), T2).
test2(T2) :- transform_term(forall([x,robot], charged(x)),T2).
test3(T2) :- transform_term(forall([x,robot], moving(x)),T2).
test4(T2) :- transform_term(exists([x,robot], moving(x)),T2).
test5(T2) :- transform_term(
				for exists([x,robot], moving(x)),T2).
