%
% This is an implementation of the "FaceIn"-library.
%
% The implementation follows the specification located at:
%
%     [advanced programming course homepage]
%     (http://www.diku.dk/~kflarsen/ap-e2014/facein/communication-facein.html)
%
% Written by Frederik Hanghøj Iversen
% for the course Advanced Programming
% at The University of Copenhagen 2014
%
% me@fredefox.eu /^._
%  ,___,--~~~~--' /'~
%  `~--~\ )___,)/'
%      (/\\_  (/\\_
%

-module(facein).

-export([start/1, add_friend/2, friends/1, broadcast/3, received_messages/1]).

%
%     Person-server
%
% This function is the implementation of a "Person-Server". Each clause in the
% outer-most `receive`-statement corresponds to a function this library
% exports.
%
serve(Id, Friends, Msgs) ->
	% Server-implementation of all responses.
	receive
		{Pid, get_friends} ->
			% TODO This does not follow specification. It does indeed return a
			% list of two-tuples, but the tuples are reversed. They should be:
			%
			%     {Id, Pid}
			%
			% But are in fact:
			%
			%     {Pid, Id}
			%
			Pid ! {self(), gb_trees:to_list(Friends)},
			serve(Id, Friends, Msgs);
		{Pid, {add_friend, F}} ->
			Name = request(F, get_name),
			NewFriends = gb_trees:enter(F, Name, Friends),
			Pid ! {self(), ok},
			serve(Id, NewFriends, Msgs);
		{Pid, get_name} ->
			Pid ! {self(), Id},
			serve(Id, Friends, Msgs);
		{Pid, {init_broadcast, M, R}} ->
			% `Message` is a tuple with the name of originial broadcaster and
			% the message
			Message = {Id, M},
			self() ! {broadcast, Message, R},
			Pid ! {self(), ok},
			serve(Id, Friends, Msgs);
		{broadcast, M, R} ->
			{Ref, _} = M,
			NewMsgs = gb_trees:enter(Ref, M, Msgs),
			if R == 0 -> serve(Id, Friends, NewMsgs);
			   R > 0 ->
				   case gb_trees:is_defined(Ref, Msgs) of
					   true ->
						   serve(Id, Friends, NewMsgs);
					   _ ->
						   Broadcast =
						       fun(Friend) -> Friend ! {broadcast, M, R-1} end,
						   lists:map(Broadcast, gb_trees:keys(Friends)),
						   serve(Id, Friends, NewMsgs)
				   end
			end;
		{Pid, get_messages} ->
			Pid ! {self(), gb_trees:values(Msgs)},
			serve(Id, Friends, Msgs)
	end.

%
%     Synchronous requests
%
% This function defines the simple protocol for interacting with the
% person-servers
%
request(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} -> Response
	end.

%
%
%     a) `start/1`
%
% start(N) starts a new person server for a person with the name N (a string,
% for instance). The function returns {ok,Pid}, where Pid is the
% process ID of the new person server.
%
start(Id) ->
		Pid = spawn(fun() -> serve(Id, gb_trees:empty(), gb_trees:empty()) end),
		{ok, Pid}.

%     b) `add_friend/2`
%
% `add_friend(P, F)` adds `F` to `P`’s friend list. The person server `P`
% contacts `F` to get the name of the friend and then stores that information.
% `F` is the process ID of a thread running a person-server. The function
% returns `ok` on success.
%
add_friend(P, F) ->
	request(P, {add_friend, F}).

%
%     c) `friends/1`
%
% `friends(P)` returns a list of friends for the person. The result is a list
% of pairs, where the first component is the name of the friend and the second
% is a process ID for the friend’s person server.
%
% TODO: The tuples are reversed.
%
friends(P) -> request(P, get_friends).

%
%     d) `broadcast/3`
%
% `broadcast(P, M, R)` sends the message `M` (any term) to all friends within a
% radius R (an integer) of P. This function is non-blocking, hence it can
% return before the message has reached all friends.
%
broadcast(P, M, R) -> request(P, {init_broadcast, M, R}).

%
%     e) `received_messages/1`
%
% `received_messages(P)` returns a list of all messages received by `P`. The
% list is a list of pairs of terms, where the first is the name of the original
% person who broadcasted the message and the second term is the message
% broadcasted.
%
received_messages(P) -> request(P, get_messages).
