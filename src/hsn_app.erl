%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <Eldon Bait and Tackle>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(hsn_app).

-behaviour(application).

-define(PORT, 8081).


-export([start/2, stop/1]).

start(_Type, _Args) ->
    
    
    case hsn_app_sup:start_link() of
        {ok, SupPid} ->
            start_cowboy_listener(),
            {ok, SupPid};
        Error ->
            Error
    end.

stop(_State) ->
    cowboy:stop_listener(handshake_listener),
    ok.

start_cowboy_listener() ->
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/handshake", handshake_handler, []},
            {"/transmission", transmission_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(handshake_listener,
        [{port, ?PORT}],
        #{env => #{dispatch => Dispatch}}).

