:- lib(eplex).

lp_example(Cost) :-
      eplex_solver_setup(min(X)),
      (X+Y $>= 3),
      (X-Y $= 0),
      eplex_solve(Cost).
	  
lp_example2(Cost) :-
      eplex_solver_setup(min(X)),
	  X :: 0..10,
      Y $= 3*X,
	  Y $= 0.2,
      eplex_solve(Cost).