%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <Eldon Bait and Tackle>
%%% @doc
%%%
%%% @end
%%% Created : 24. Sept 2025 21:44
%%%-------------------------------------------------------------------
-module('IoT-network-application').
-author("Eldon").
-behavior(application).

%% API
-export([start/2, stop/1, allowed_methods/2]).

allowed_methods(Req, State) ->
    { [<<"GET">>, <<"POST">>], Req, State }.

start(_Type, _Args) ->

    Dispatch = cowboy_router:compile([
        {'_', [
            
            {"/api/module", module_request_handler, []},
            {"/api/handshake", module_handshake_handler, []},


            {"/api/module/:id", front_hive_handler, []},
            {"/api/region/:id", front_region_handler, []},
            {"/api/generals", front_generals_handler, []}
        
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 0707}],
        #{env => #{dispatch => Dispatch}}
    ),
    {ok, self()}.
    
    
    
stop(_State) ->
    ok.
    
    
