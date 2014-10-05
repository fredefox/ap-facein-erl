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
		{Pid, get_friends} ->
			% TODO This does not follow specification. It does indeed return a
			% list of two-tuples, but the tuples are reversed. They should be:
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
broadcast(P, M, R) -> request(P, {init_broadcast, M, R}).

%
% e) `received_messages/1`
%
received_messages(P) -> request(P, get_messages).
