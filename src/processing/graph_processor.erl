%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(graph_processor).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_DIST, 5).
-include("records.hrl").

%-record(graph_processor_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->

    case module_cache:get_module_map() of
        {ok, Points} when map_size(Points) > 0 ->
            {ok, GabrielGraph} = gabriel_graph(Points),

            MapList = maps:values(GabrielGraph),

            map_cache:new_map(MapList),
            hsn_logger:send_log(?MODULE, "New Gabriel Graph Calculated");
        _ ->
            hsn_logger:send_log(?MODULE, "Something is preventing the graph processor... ")
    end,

    {stop, normal}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

gabriel_graph(Map) ->
    Keys = maps:keys(Map),
    gabriel_graph(Map, Keys, #{}).

gabriel_graph(_Map, [], AccMap) ->
    {ok, AccMap};

gabriel_graph(Map, [CurrentID | RemainingKeys], AccMap) ->
    CurrentNode = maps:get(CurrentID, Map),

    CandidateIDs = maps:keys(Map) -- [CurrentID],

    Neighbors = find_gabriel_neighbors(CurrentNode, CandidateIDs, Map),

    NewNode = CurrentNode#node{neighbors = Neighbors},
    NewAccMap = maps:put(CurrentID, NewNode, AccMap),

    gabriel_graph(Map, RemainingKeys, NewAccMap).

find_gabriel_neighbors(CurrentNode, CandidateIDs, Map) ->
    lists:filtermap(fun(OtherID) ->

        OtherNode = maps:get(OtherID, Map),

        check_gabriel_condition(CurrentNode, OtherNode, Map)
                    end,
        CandidateIDs).

%% Check if OtherNode is a Gabriel neighbor of CurrentNode
check_gabriel_condition(CurrentNode, OtherNode, Map) ->
    #node{location = {L21, L22}} = OtherNode,
    #node{location = {L11, L12}} = CurrentNode,

    case {is_number(L11), is_number(L12), is_number(L21), is_number(L22)} of
        {true, true, true, true} ->
            Dist = math:sqrt(math:pow(L21 - L11, 2) + math:pow(L22 - L12, 2)),
            Radius = Dist / 2,

            MidX = (L11 + L21) / 2,
            MidY = (L12 + L22) / 2,

            AllNodes = maps:values(Map),
            IsBlocked = lists:any(fun(TestNode) ->
                is_in_circle(TestNode, MidX, MidY, Radius)
                                  end, AllNodes -- [CurrentNode, OtherNode]),

            not IsBlocked;
        _ ->
            false
    end.


is_in_circle(#node{location = {TX, TY}}, CX, CY, Radius) ->
    Dist = math:sqrt(math:pow(TX - CX, 2) + math:pow(TY - CY, 2)),
    Dist < Radius.