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
