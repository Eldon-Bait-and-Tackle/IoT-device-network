%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(heuristic_processor).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    
    code_change/3]).

-define(SERVER, ?MODULE).

-record(heuristic_processor_state, {}).

-include("records.hrl").

-define(HEURISTIC_TEMP, 20.0).
-define(ALLOWED_DEVIATION, 0.05).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Result = process(),
    hsn_logger:send_log(?MODULE, "Heuristic processor is done, sending to cache"),
    save_to_cache(Result),
    {stop, normal}.

handle_call(_Request, _From, State = #heuristic_processor_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #heuristic_processor_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #heuristic_processor_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #heuristic_processor_state{}) ->
    ok.

code_change(_OldVsn, State = #heuristic_processor_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

save_to_cache(Result_map) ->
    heuristics_cache:new_results(Result_map).

process() ->
    {ok, ModuleIDs} = module_cache:get_all_ids(),
    lists:map(fun(ID) -> check_module_temp(ID) end, ModuleIDs).

check_module_temp(ModuleID) ->
    SelfTemp = get_module_temp(ModuleID),

    case map_cache:get_neighbors(ModuleID) of
        {ok, NeighborIDs} when length(NeighborIDs) > 0 ->
            AvgNeighborTemp = get_neighbor_avg_temp(NeighborIDs),
            {IsWithinRange, Deviation} = is_within_range(SelfTemp, AvgNeighborTemp),
            {ModuleID, SelfTemp, AvgNeighborTemp, IsWithinRange, Deviation};
        _ ->
            {ModuleID, SelfTemp, no_neighbors, true}
    end.

is_within_range(CurrentTemp, AverageTemp) ->
    Deviation = abs(CurrentTemp - AverageTemp) / AverageTemp,
    {Deviation =< ?ALLOWED_DEVIATION, Deviation}.

get_neighbor_avg_temp(NeighborIDs) ->
    {Sum, Count} = lists:foldl(
        fun(ID, {AccSum, AccCount}) ->
            Temp = get_module_temp(ID),
            {AccSum + Temp, AccCount + 1}
        end,
        {0.0, 0},
        NeighborIDs
    ),
    Sum / Count.

get_module_temp(ModuleID) ->
    case transmission_cache:get_recent_reading(ModuleID) of
        {ok, #transmission{temperature = Temp}} ->
            Temp;
        _ ->
            ?HEURISTIC_TEMP
    end.