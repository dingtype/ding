-module(simple).
-author("Joe Chasinga <jo.chasinga@gmail.com>").
-export([
	server/1,
	client/2,
	client_send/3,
	client_ping/3,
	start_server/0,
	start_client/2,
	stop_client/2]).

notify_clients([], _) ->
    ok;
notify_clients([{Pid, _Name}| RestNodes], {Msg, From}) ->
    Pid ! {msg, Msg, From},
    notify_clients(RestNodes, {Msg, From});
notify_clients([{Pid, _Name}| RestNodes], Msg) ->
    Pid ! {online_nodes, Msg},
    notify_clients(RestNodes, Msg).

server(OnlineNodes) ->
    receive
	{send, Msg, From} ->
	    notify_clients(OnlineNodes, {Msg, From}),
	    server(OnlineNodes);
	{ping, From, To} ->
	    {TargetPid, _Name} = lists:keyfind(To, 2, OnlineNodes),
	    TargetPid ! {ping, From},
	    server(OnlineNodes);
	{online, Name, Pid} ->
	    NewOnlineNodes = [{Pid, Name}| OnlineNodes],
	    notify_clients(NewOnlineNodes, NewOnlineNodes),
	    server(NewOnlineNodes);
	{offline, Name} ->
	    NewOnlineNodes = lists:keydelete(Name, 2, OnlineNodes),
	    notify_clients(NewOnlineNodes, NewOnlineNodes),
	    server(NewOnlineNodes)
    end.

client(ServerNode, Name) ->
    {server, ServerNode} ! {online, Name, self()},
    client_loop(ServerNode, Name).

client_loop(ServerNode, Name) ->
    receive
	{msg, Msg, From} ->
	    io:format("~p said: ~p~n", [From, Msg]),
	    client_loop(ServerNode, Name);
	{send_msg, Msg} ->
	    {server, ServerNode} ! {send, Msg, Name},
	    client_loop(ServerNode, Name);
	{online_nodes, OnlineNodes} ->
	    io:format("Total ~p users are online~n", [length(OnlineNodes)]),
	    client_loop(ServerNode, Name);
	{ping, From} ->
		io:format("You get pinged by ~p~n", [From]),
		client_loop(ServerNode, Name);
	go_offline ->
		io:format("", []),
	    {server, ServerNode} ! {offline, Name}
    end.

client_ping(ServerNode, From, To) ->
    {server, ServerNode} ! {ping, From, To}.

client_send(ServerNode, Msg, From) ->
    Words = string:split(Msg, " ", all),
    TargetUsers = [tl(N) || N <- Words, hd(N) == $@],
    lists:foreach(fun(U) -> client_ping(ServerNode, From, U) end, TargetUsers),
    {server, ServerNode} ! {send, Msg, From}.

start_server() ->
    register(server, spawn(?MODULE, server, [[]])).

start_client(ServerNode, Name) ->
    spawn(?MODULE, client, [ServerNode, Name]).

stop_client(ServerNode, Name) ->
	{server, ServerNode} ! {offline, Name}.

% start() ->
%     StartingNodes = [],
%     Pid = spawn(simple, server, [StartingNodes]),
%     spawn(simple, client, [Pid, "east"]),
%     spawn(simple, client, [Pid, "west"]),
%     spawn(simple, client, [Pid, "poff"]).



