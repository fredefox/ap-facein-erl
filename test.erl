-module(test).
-compile(export_all).
-c(facein).

% Start the server
test00() ->
	facein:start(42).

% Get empty list of friends
test01() ->
	{ok, Pid} = facein:start(42),
	facein:friends(Pid).

% Test `add_friend`
test02() ->
	{ok, P} = facein:start(42),
	{ok, Q} = facein:start(1337),
	facein:add_friend(P, Q),
	% P should now be friends with Q aka 1337.
	facein:friends(P).
