%%%-------------------------------------------------------------------
%%% @author eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2025 1:44 PM
%%%-------------------------------------------------------------------
-module(data_cache_manager).
-author("eldon").

%% API
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    data_cache_sup:start_link().

stop(_State) ->
    ok.

