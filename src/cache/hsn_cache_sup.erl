%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2025 20:27
%%%-------------------------------------------------------------------
-module(hsn_cache_sup).
-behaviour(supervisor).
-author("Eldon").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
        intensity => 5,
        period => 10},

    ModuleCache = #{id => module_cache,
        start => {module_cache, start_link, []},
        restart => permanent,
        type => worker},

    MapCache = #{id => map_cache,
        start => {map_cache, start_link, []},
        restart => permanent,
        type => worker},

    HeuristicsCache = #{id => heuristics_cache,
        start => {heuristics_cache, start_link, []},
        restart => permanent,
        type => worker},

    TransCache = #{id => transmission_cache,
        start => {transmission_cache, start_link, []},
        restart => permanent,
        type => worker},

    {ok, {SupFlags, [ModuleCache, MapCache, HeuristicsCache, TransCache]}}.
