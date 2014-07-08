:- dynamic domain/3.
:- dynamic domain/2.

domain(Sort, D) :- domain(Sort, D, s0).

domain(Sort, D, s0) :- D = [1,2,3].
