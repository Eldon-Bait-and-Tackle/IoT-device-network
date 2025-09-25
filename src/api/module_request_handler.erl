%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <Eldon Bait and Tackle>
%%% @doc
%%%
%%% @end
%%% Created : 24. Sept 2025 22:44
%%%-------------------------------------------------------------------
-module(module_request_handler).
-author("Eldon").
-behavior(cowboy_handler).

%% API
-export([]).

init(Req, State) ->
    
    Body = jiffy:encode(#{status => <<"ok">>}),
    Internal_Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req),
    {ok, Internal_Req, State}.

