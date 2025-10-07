%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(module_cache).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(module_cache_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #module_cache_state{}}.

handle_call(_Request, _From, State = #module_cache_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #module_cache_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #module_cache_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #module_cache_state{}) ->
    ok.

code_change(_OldVsn, State = #module_cache_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
