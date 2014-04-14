
-module(t).
-export([run/1,
	 equal/2,
	 equal/3,
	 no_die/1]).

run(Module) ->
	Tests = [F || {F, 0} <- Module:module_info(exports), 
				is_test(F)],
	[Module:F() || F <- Tests],
	io:format("~n").

equal(A, B) ->
	equal(A, B, []).

equal(A, A, _Note) ->
	io:format("."),
	ok;
equal(A, B, Note) ->
	Message = io_lib:format(
			"~nTest Failed! ~n"
			"\tExpected: [~p] ~n" 
			"\tBut was:  [~p] ~n~s ~n", 
			[A, B, Note]),
	io:format(lists:flatten(Message)),
	fail.

no_die(F) when is_function(F) ->
	no_die({F, []});
no_die({F, Note}) when is_function(F) ->
	try F() of
		_ ->
			io:format("."),
			ok
	catch
		Type:E ->
			Message = io_lib:format("~nTest Died! ~n\tCaught: [~p:~p]"
					"~n~s ~n", [Type, E, Note]),
			io:format(lists:flatten(Message)),
			fail
	end.

is_test(F) ->
	case atom_to_list(F) of
		"test_" ++ _ ->
			true;
		_ ->
			false
	end.

