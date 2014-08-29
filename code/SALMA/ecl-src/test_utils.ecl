assertEquals(Expression, Expected, Msg) :-
	(number(Expected) ->
		F = =:=(Expression, Expected)
		;
		F = =(Expression, Expected)
	),
	evaluate_ad_hoc(F, R),
	(R = not_ok ->
		printf(stderr, "Assertion error: expected %w for %w.", [Expected, Expression]), 
		throw(assertion_error(Msg, Expression, Expected))
		;
		true
	).
	

	