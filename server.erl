-module(server).
-author("Joe Chasinga <jo.chasinga@gmail.com>").
-export([start/0, broadcast/2, loop/1]).

%%--------------------------------------------------------------------
%% @doc  Broadcasts message to all connected nodes
%% @end
%%--------------------------------------------------------------------
-spec broadcast(Nodes::[{pid(),string() | atom()}], 
		Msg::{string(),string() | atom()} | string()
	       ) -> none().
broadcast([], _) ->
    ok;
broadcast([{Pid, _Name}| RestNodes], {Msg, From}) ->
    Pid ! {msg, Msg, From},
    broadcast(RestNodes, {Msg, From});
broadcast([{Pid, _Name}| RestNodes], Msg) ->
    Pid ! {online_nodes, Msg},
    broadcast(RestNodes, Msg).


%%--------------------------------------------------------------------
%% @doc  Server loop
%%       Usually not called directly
%% @end
%%--------------------------------------------------------------------
-spec loop(Nodes::[{pid(),string() | atom()}]) -> node().
loop(OnlineNodes) ->
    receive
        {send, Msg, From} ->
            broadcast(OnlineNodes, {Msg, From}),
            loop(OnlineNodes);
        {ping, From, To} ->
	        case lists:keyfind(To, 2, OnlineNodes) of
		    {TargetPid, _Name} ->
		        TargetPid ! {ping, From};
		    _ -> 
		        ok
	        end,
	        loop(OnlineNodes);

        {online, Name, Pid} ->
            NewOnlineNodes = [{Pid, Name}| OnlineNodes],
            broadcast(NewOnlineNodes, NewOnlineNodes),
            loop(NewOnlineNodes);

        {offline, Name} ->
            io:format("~p is logging off~n", [Name]),
                NewOnlineNodes = lists:keydelete(Name, 2, OnlineNodes),
            io:format("Total ~p users are online~n", [length(NewOnlineNodes)]),
                broadcast(NewOnlineNodes, NewOnlineNodes),
                loop(NewOnlineNodes)
    end.

%%--------------------------------------------------------------------
%% @doc  Start the server loop
%% @end
%%--------------------------------------------------------------------
-spec start() -> none().
start() ->
    register(server, spawn(?MODULE, loop, [[]])).
