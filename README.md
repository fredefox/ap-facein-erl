# Advanced Programming Assigmnet 5

Written by Frederik Hanghøj Iversen
for the course Advanced Programming
at Copenhagen University 2014

This report is for the fourth assignment in the course Advanced Programming.

This docuement is written in [GitHub Flavored Markdown](https://help.github.com/articles/github-flavored-markdown). It may best be viewed in the markdown-format but is also included as a pdf. The pdf-version was created with [pandoc](http://johnmacfarlane.net/pandoc/).

The project is maintened with git and repository acces can be granted by finding [me on GitHub](https://github.com/fredefox).

In this report I present the results of my efforts to implement a library for "Face In" in Erlang. The specification for the library can be found [here](http://www.diku.dk/~kflarsen/ap-e2014/facein/communication-facein.html) (henceforth "the specification").

Source-code is attached along with pdf and markdown-versions of this report.

# Introduction
The code I present here is an implementations of the library defined in The Specification. The code defines a very simple protocol for synchronous message-passing as well as utilizing some asynchronous calls. It is implemented with a single thread per node in the network.

## Code structure
The implementation is found in `facein.erl` and can be accessed by doing e.g.:

```
c(facein).
```

In an interactive erlang-session.

The code is annoted with comments and there is one comment-block corresponding to each bullet in the sspecification. The comments are slight variations of the ones given in the specification. E.g.: 

```erlang
%
%     a) `start/1`
%
% `start(N)` starts a new person server for a person with the name `N` (a
% string, for instance). The function returns `{ok,Pid}`, where `Pid` is the
% process ID of the new person server.
%
```

One helper-function representing the synchronous message-passing protocol is supplied as well as one function acting as the Person Server.

### Tests
The tests are included in `test.erl`. The results of all the tests can seen by doing:

```erlang
c(facein), c(test), test:test_all().
```

In an interactive erlang-session. Test-cases are likewise annoted with comments.

## Code-review
First the overall structure of the code will be presented and then the implementation-details of one of the functions will be presented.

All code-samples are taken directly from the source stripped of comments and blank lines.

### Overall structure
The code is structuered so that first the function is the one defining a Person Server's operation. Each clause in the outer-most `receive`-statement (with one exception) is the server-implementation corresponding to some client-side function. These client-side functions are the ones this module exports. All of these follow a very simple synchronous message-passing protocol which is defined in the function `request/2`:

```erlang
request(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.
```

This function send some data to a another process and then immediately waits for the response. The server must therefore respond with a tuple where the first element is the process ID of the server itself and the second element is the response. Here is an example of how this looks on the server-side:

```erlang
serve(Id, Friends, Msgs) ->
    receive
        {Pid, get_friends} ->
            Pid ! {self(), gb_trees:to_list(Friends)},
            serve(Id, Friends, Msgs);
```

The server-functions works by doing tail-recursive calls to the function itself passing along the state, i.e. `Friends` and `Msgs` in each call, optionally updating them.

Implementing the client-side of each procedure then becomes a simple matter of delegation:

```erlang
add_friend(P, F) -> request(P, {add_friend, F}).
```

The real implementation-details are on the server-side of things.

### Broadcast
Perhaps the most interesting method-implementation is `broadcast`. Here I will explain the server-side implementaiton of this method.

It's (server-side) definition looks like this:

```erlang
{Pid, {init_broadcast, M, R}} ->
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
```

A client sends a request to the person server by issuing the `init_broadcast`-command. This call is synchronous. This just means that the server reports that it has successfully received the command and has commenced the broadcast. The server will thus respond before the broadcast finnishes.

The server then sends a message to itself. That is, the first actual broadcast. The server then checks if the radius is greater than zero in which case it continues the broadcast by mapping the locally defined function `Broadcast` over all its friends - the list returned by `gb_trees:keys`. In any case the message is stored in the thread with `gb_trees:enter(Ref, M, Msgs)`.

## Tests
The minimal testing described in the specification has been performed on the sample graph and can be found in `test.erl`. One thing to note about the tests is that it can be tricky to automate tests for an asynchronous method like `broadcast` since there is no way of knowing when the broadcast finnishes and thus we can not know if issuing a `received_messages` will reflect the final state. To alleviate this a `sleep`-statement has been inserted. Please note that this will not ensure that the tests returns the desired results. Really the valid results of doing a `received_messages` is any subset of the messages that should at some point return to any given node. See the comments in the test-suite for further details.

# Conclusion
In this report my efforts to implement the Face In library was presented. The library handles asynchronous and synchronous message-passing in a distributed network. The test-cases have been run with success although they are not entirely robust in the sense that they might give false negatives.
