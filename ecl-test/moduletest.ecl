:- module(moduletest).
:- use_module(moduletest2).
:- export bar/0, foo/0, getfluents/0, fluent/3.
:- dynamic fluent/3.

foo :-
	write("Foo!\n"),
	baz.
	
bar :-
	write("Bar!\n").
	
getfluents :-
	findall([X,Y,Z], fluent(X,Y,Z), L),
	write(L),nl.

