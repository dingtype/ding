-module(client).
-author("Joe Chasinga <jo.chasinga@gmail.com>").
-export([logon/2, logoff/2, init/2, ping/3, loop/2, send/3]).

logon(ServerNode, Name) ->
    spawn(?MODULE, init, [ServerNode, Name]).

logoff(ServerNode, Name) ->
    {server, ServerNode} ! {offline, Name}.

init(ServerNode, Name) ->
    {server, ServerNode} ! {online, Name, self()},
    loop(ServerNode, Name).

ping(ServerNode, From, To) ->
    {server, ServerNode} ! {ping, From, To}.

send(ServerNode, Msg, From) ->
    Words = string:split(Msg, " ", all),
    TargetUsers = [tl(N) || N <- Words, hd(N) == $@],
    lists:foreach(fun(U) -> ping(ServerNode, From, U) end, TargetUsers),
    {server, ServerNode} ! {send, Msg, From}.

loop(ServerNode, Name) ->
    receive
        {msg, Msg, From} ->
            io:format("~p said: ~p~n", [From, Msg]),
            loop(ServerNode, Name);
        {send_msg, Msg} ->
            {server, ServerNode} ! {send, Msg, Name},
            loop(ServerNode, Name);
        {online_nodes, OnlineNodes} ->
            io:format("Total ~p users are online~n", [length(OnlineNodes)]),
            loop(ServerNode, Name);
        {ping, From} ->
	    io:format("You get pinged by ~p~n", [From]),
	    loop(ServerNode, Name);
        go_offline ->
	    io:format("", []),
            {server, ServerNode} ! {offline, Name}
    end.

