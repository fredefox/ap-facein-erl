-module(facein).
-export([start/1, add_friend/2, friends/1, broadcast/3, received_messages/1]).

%
% a) `start/1`
%
start(Id) ->
		Pid = spawn(fun() -> serve(Id, gb_trees:empty(), gb_trees:empty()) end),
		{ok, Pid}.

serve(Id, Friends, Msgs) ->
	% Server-implementation of all responses.
	receive
		% TODO
		{Pid, get_friends} ->
			% This does not follow specification. It does indeed return a list
			% of two-tuples, but the tuples are reversed. They should be:
			%
			%     {Id, Pid}
			%
			% But are in fact:
			%
			%     {Pid, Id}
			Pid ! {self(), gb_trees:to_list(Friends)},
			serve(Id, Friends, Msgs);
		{Pid, {add_friend, F}} ->
			Name = request(F, get_name),
			% TODO: runtime error occurs here if `Friends` contain F
			NewFriends = gb_trees:enter(F, Name, Friends),
			Pid ! {self(), ok},
			serve(Id, NewFriends, Msgs);
		{Pid, get_name} ->
			Pid ! {self(), Id},
			serve(Id, Friends, Msgs);
		{broadcast, M, R} ->
			if R == 0 -> [];
			   R > 0 ->
				   % loop through friends and send message
				   %
				   % TODO: We might have actually already broadcast this
				   % message.  Consider the sample graph in the case where
				   % Susan has broadcast some message with radius >3. She will
				   % then later receive a message from Andrzej and Jen with the
				   % same message and the radius 2 less. She should ideally
				   % only forward one of these messages.
				   Broadcast = fun(Friend) -> Friend ! {broadcast, M, R-1} end,
				   lists:map(Broadcast, gb_trees:keys(Friends))
			end,
			serve(Id, Friends, Msgs);
		{Pid, get_messages} -> throw(undefined)
	end.

% Synchronous requests
request(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} -> Response
	end.

%
% b) `add_friend/2`
%
add_friend(P, F) ->
	request(P, {add_friend, F}).

%
% c) `friends/1`
%
friends(P) -> request(P, get_friends).

%
% d) `broadcast/3`
%
broadcast(P, M, R) -> P ! {broadcast, M, R}.

%
% e) `received_messages/1`
%
received_messages(P) -> request(P, get_messages).
