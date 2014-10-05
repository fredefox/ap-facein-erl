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
setupGraph() ->
	% Set up the graph
	% Add nodes
	{ok, Ken} = facein:start(ken),
	{ok, Andrzej} = facein:start(andrzej),
	{ok, Susan} = facein:start(susan),
	{ok, Reed} = facein:start(reed),
	{ok, Jessica} = facein:start(jessica),
	{ok, Jen} = facein:start(jen),
	{ok, Tony} = facein:start(tony),
	% Add the edges
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
	dict:from_list([
		{ken, Ken},
		{andrzej, Andrzej},
		{susan, Susan},
		{reed, Reed},
		{jessica, Jessica},
		{jen, Jen},
		{tony, Tony}
	]).
% All the tests mentioned in "Minimal testing" is given here
test04() ->
	G = setupGraph(),
	{ok, Jen} = dict:find(jen, G),
	{ok, Susan} = dict:find(susan, G),
	{ok, Tony} = dict:find(tony, G),
	{ok, Jessica} = dict:find(jessica, G),
	% Jen's friends
	case facein:friends(Jen) of
		[{Susan, susan}, {Jessica, jessica}, {Tony, tony}] -> true;
		X -> X
	end.
test05() ->
	G = setupGraph(),
	{ok, Jessica} = dict:find(jessica, G),
	{ok, Ken} = dict:find(ken, G),
	{ok, Tony} = dict:find(tony, G),
	% broadcasting
	facein:broadcast(Jessica, blabla, 2),
	facein:broadcast(Ken, albalb, 3),
	% The results of the broadcasts
	% Really we can't know when (if) she receives this messages
	% Hopefully 0.1 secs will be enough and she will have received it.
	timer:sleep(100),
	case facein:received_messages(Tony) of
		[{jessica, blabla}] -> true;
		% Really this should also be valid:
		%
		%     [] -> true
		%
		_ -> false
	end.
test06() ->
	G = setupGraph(),
	{ok, Jessica} = dict:find(jessica, G),
	{ok, Ken} = dict:find(ken, G),
	{ok, Susan} = dict:find(susan, G),
	% broadcasting
	facein:broadcast(Jessica, blabla, 2),
	facein:broadcast(Ken, albalb, 3),
	% As above we can't know how much time will pass.
	timer:sleep(100),
	% Here we also can not know from whom she receives the message first.
	case facein:received_messages(Susan) of
		[{jessica, blabla}, {ken, albalb}] -> true;
		[{ken, albalb}, {jessica, blabla}] -> true;
		% ... and again any subsets of this would also strictly speaking be ok
		_ -> false
	end.
test07() ->
	G = setupGraph(),
	{ok, Jessica} = dict:find(jessica, G),
	{ok, Ken} = dict:find(ken, G),
	{ok, Reed} = dict:find(reed, G),
	% broadcasting
	facein:broadcast(Jessica, blabla, 2),
	facein:broadcast(Ken, albalb, 3),
	% The results of the broadcasts
	timer:sleep(100),
	case facein:received_messages(Reed) of
		[{ken, albalb}] -> true;
		_ -> false
	end.

test_all() -> [
	test01(),
	test02(),
	test03(),
	test04(),
	test05(),
	test06(),
	test07()
	].
