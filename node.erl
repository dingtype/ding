-module(node).
-author("Joe Chasinga <jo.chasinga@gmail.com>").
-export([start/1]).

%%--------------------------------------------------------------------
%% @doc  Starts both server and client
%% @end
%%--------------------------------------------------------------------
-spec start(Name::string() | atom()) -> none().
start(Name) ->
    server:start(),
    client:logon("server@pieohpah",Name).
    

