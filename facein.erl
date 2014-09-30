-module(facein).
-export([start/1, add_friend/2, friends/1, broadcast/3, received_messages/1]).
%-export(your stuff here).

%
% a) `start/1`
%
start(Id) -> {
		ok,
		spawn(fun() -> serve(Id, []) end)
	   }.

serve(Id, Friends) ->
	% Server-implementation of all responses.
	receive
		% TODO
		{Pid, get_friends} -> Pid ! Friends
	end.

request(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} -> Response
	end.
%
% b) `add_friend/2`
%
add_friend(P, F) ->
	% Somehow add the result of the following call to `P`.
	P = request(F, get_friends) ++ P.

%
% c) `friends/1`
%
friends(P) -> throw(unimplemented).

%
% d) `broadcast/3`
%
broadcast(P, M, R) -> throw(unimplemented).

%
% e) `received_messages/1`
%
received_messages(P) -> throw(unimplemented).
