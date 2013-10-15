domain(robot, D) :- D = [rob1, rob2, rob3].


rep(T,Var,NewV,T2) :- 

transform(forall(Var,Sort,T), T2) :- domain(Sort, D), true. 