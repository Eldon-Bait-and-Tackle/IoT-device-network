%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(heuristics_cache).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    new_results/1, get_results_by_module/1, get_results_by_list/1, get_all_results/0,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).
-include("records.hrl").

-record(heuristics_cache_state, {}).

%%%===================================================================
%%% API functions
%%%===================================================================

new_results(Results_map) ->
    gen_server:cast(?SERVER, {new_results, Results_map}).

get_results_by_module(Module_id) ->
    case ets:lookup(?TABLE, Module_id) of
        [] ->
            {error, not_found};
        [Payload] ->
            {ok, to_map(Payload)}
    end.

get_all_results() ->
    Objects = ets:tab2list(?TABLE),
    lists:map(fun to_map/1, Objects).

get_results_by_list(Mid_list) ->
    Result = lists:flatmap(
        fun(ModuleID) ->
            ets:lookup(?TABLE, ModuleID)
        end,
        Mid_list
    ),
    {ok, Result}.

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?TABLE, [set, public, named_table, {keypos, 1}]),
    {ok, #heuristics_cache_state{}}.

handle_call(_Request, _From, State = #heuristics_cache_state{}) ->
    {reply, ok, State}.
handle_cast({new_results, Map}, State = #heuristics_cache_state{}) ->
    update_with_new_map(Map),
    hsn_logger:send_log(?SERVER, "Heuristc Cache received and updated"),
    {noreply, State};
handle_cast(_Request, State = #heuristics_cache_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #heuristics_cache_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #heuristics_cache_state{}) ->
    ok.

code_change(_OldVsn, State = #heuristics_cache_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

to_map({Module_id, Self_temp, Avg_temp, Within_range, Deviation}) ->
    #{
        <<"module_id">> => Module_id,
        <<"self_temp">> => Self_temp,
        <<"avg_neighbor_temp">> => Avg_temp,
        <<"within_range">> => Within_range,
        <<"deviation">> => Deviation
    };
to_map({Module_id, Self_temp, no_neighbors, Within_range}) ->
    #{
        <<"module_id">> => Module_id,
        <<"self_temp">> => Self_temp,
        <<"avg_neighbor_temp">> => <<"no_neighbors">>,
        <<"within_range">> => Within_range,
        <<"deviation">> => 0.0
    }.

update_with_new_map([]) ->
    ok;
update_with_new_map([Head | Remaining]) ->
    ets:insert(?TABLE, Head),
    update_with_new_map(Remaining).
