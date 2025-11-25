%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2025 20:28
%%%-------------------------------------------------------------------
-module(hsn_external_sup).
-author("Eldon").

%% API
-export([init/1]).
-behaviour(supervisor).

-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
        intensity => 5,
        period => 10},

    DbWorker = #{id => database_handler,
        start => {database_handler, start_link, []},
        restart => permanent,
        type => worker},

    {ok, {SupFlags, [DbWorker]}}.