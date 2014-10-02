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

% The sample graph.
test04() ->
	{ok, Ken} = facein:start(ken),
	{ok, Andrzej} = facein:start(andrzej),
	{ok, Susan} = facein:start(susan),
	{ok, Reed} = facein:start(reed),
	{ok, Jessica} = facein:start(jessica),
	{ok, Jen} = facein:start(jen),
	{ok, Tony} = facein:start(tony),
	facein:add_friend(Ken, Andrzej),
	facein:add_friend(Andrzej, Ken),
	facein:add_friend(Andrzej, Susan),
	facein:add_friend(Susan, Andrzej),
	facein:add_friend(Susan, Jen),
	facein:add_friend(Susan, Jessica),
	facein:add_friend(Susan, Reed),
	facein:add_friend(Jen, Susan),
	facein:add_friend(Jen, Jessica),
	facein:add_friend(Jen, Tony),
	facein:add_friend(Jessica, Jen),
	facein:add_friend(Reed, Tony),
	facein:add_friend(Reed, Jessica),

	facein:broadcast(Ken, blabla, 42).
