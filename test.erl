-module(test).
-compile(export_all).
-c(facein).

% Start the server
test00() ->
	case facein:start(42) of
		{ok, _} -> true;
		_ -> false
	end.

% Get empty list of friends
test01() ->
	{ok, Pid} = facein:start(42),
	case facein:friends(Pid) of
		[] -> true;
		_ -> false
	end.

% Test `add_friend`
test02() ->
	{ok, P} = facein:start(42),
	{ok, Q} = facein:start(1337),
	case facein:add_friend(P, Q) of
		ok -> true;
		_ -> false
	end.

% Test `friends` more.
test03() ->
	{ok, P} = facein:start(42),
	{ok, Q} = facein:start(1337),
	facein:add_friend(P, Q),
	% P should now be friends with Q aka 1337.
	case facein:friends(P) of
		[{_SomePid, 1337}] -> true;
		_ -> false
	end.
