%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2025 20:28
%%%-------------------------------------------------------------------

-module(hsn_app_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all, % If one distinct subsystem fails, restart everything to ensure consistency
        intensity => 0,
        period => 1},

    DbSup = #{id => hsn_db_sup,
        start => {hsn_db_sup, start_link, []},
        type => supervisor,
        shutdown => infinity},

    CacheSup = #{id => hsn_cache_sup,
        start => {hsn_cache_sup, start_link, []},
        type => supervisor,
        shutdown => infinity},

    ProcessingSup = #{id => hsn_processing_sup,
        start => {hsn_processing_sup, start_link, []},
        type => supervisor,
        shutdown => infinity},

    {ok, {SupFlags, [DbSup, CacheSup, ProcessingSup]}}.