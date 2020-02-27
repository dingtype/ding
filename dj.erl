-module(dj).
-author("Joe Chasinga <jo.chasinga@gmail.com>").
-export([start/0, play/1, loop/0, start_local/0]).

%%--------------------------------------------------------------------
%% @doc  Dj loop
%%       Usually not called directly
%% @end
%%--------------------------------------------------------------------
-spec loop() -> none().
loop() ->
    receive
        {From, {play, Audio}} ->
            io:format("Playing sound for ~p~n", [From]),
            play(Audio),
            loop();
        _ -> 
            io:format("oh, oh"),
            loop()
	end.

-spec play() -> none().
play() ->
    io:format("Playing sound by DJ"),
    Cmd = "play ~s",
    File = "assets/sounds/~s",
    Filename = "service-bell_daniel_simion.wav",
    Cmd1 = lists:flatten( io_lib:format(Cmd, [
        lists:flatten( io_lib:format(File, [Filename]) )
    ]) ),
    os:cmd(Cmd1).

%%--------------------------------------------------------------------
%% @doc  Play the chosen audio clip
%% @end
%%--------------------------------------------------------------------
-spec play(Audio::string() | atom()) -> none().
play(Audio) when is_list(Audio), Audio =:= "service-bell" ->
    play();
play(Audio) when is_atom(Audio), Audio =:= 'service-bell' ->
    play();
play(_) ->
    play().

%%--------------------------------------------------------------------
%% @doc  Start the server loop
%% @end
%%--------------------------------------------------------------------
-spec start() -> none().
start() ->
    register(dj, spawn(?MODULE, loop, [])).

-spec start_local() -> none().
start_local() -> loop().
