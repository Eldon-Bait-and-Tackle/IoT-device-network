%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(hsn_app).

-behaviour(application).

-define(PORT, 8081).


-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/handshake", handshake_handler, []},
            {"/transmission", transmission_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_http(handshake_listener, 100,
        [{port, ?PORT}],
        #{env => #{dispatch => Dispatch}}),

    handshake_api_sup:start_link().

stop(_State) ->
    ok.
