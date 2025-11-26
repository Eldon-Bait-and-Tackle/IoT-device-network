%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2025 20:28
%%%-------------------------------------------------------------------
-module(hsn_processing_sup).
-author("Eldon").
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    
    P_manager = #{id => processing_manager,
        start => {processing_manager, start_link, []},
        restart => permanent,
        type => worker},

    Hsn_logger = #{id => hsn_logger,
        start => {hsn_logger, start_link, []},
        restart => permanent,
        type => worker},

    {ok, {SupFlags, [Hsn_logger, P_manager]}}.
