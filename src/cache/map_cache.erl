%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(map_cache).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3,
    new_map/1]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(map_cache_state, {}).
-include("records.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================


new_map(Map) ->
    gen_server:cast({new_map, Map}).


get_node(Nid) ->
    gen_server:call({get_node, Nid}).


%%% this function will return the {listofneighbors, numberofneighbors}, please refer to doc for more details on this choice
gen_neighbors(Nid) ->
    gen_server:call({get_neighbors, Nid}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?TABLE, [set, public, named_table, {keypos, 1}]),
    {ok, #map_cache_state{}}.

handle_call({get_neighbors}, _From, State = #map_cache_state{}) ->
    case ets:lookup(?TABLE, Nid) of
        [{Nid, Num, Neighbors, Location} = Reuslt] ->
            {reply, {ok, Neighbors}, State};
        [] ->
            {reply, {error, "map cache failed to find the module of given ID, "}, State}
        end

;
handle_call({get_node, Nid}, _From, State = #map_cache_state{}) ->

    case ets:lookup(?TABLE, Nid) of
        [{Nid, Num, Neighbors, Location} = Reuslt] ->
            {reply, {ok, Result}, State};
        [] ->
            {reply, {error, "map cache failed to find the module of given ID, "}, State}
        end
;
handle_call(_Request, _From, State = #map_cache_state{}) ->
    {reply, ok, State}.


handle_cast({new_map, Map}, State = #map_cache_state{]) ->
    case update_map(Map) of
        {ok, _} ->
            {noreply, State};
        {error_1, MSG} ->
            logger:send_log(?SERVER, MSG),
            {noreply, State};
        {error_2 MSG} ->
            logger:send_log(?SERVER, MSG),
            {noreply, State};
        _ ->
            logger:send_log(?SERVER, "Super unkown problem in the map cache")
        end
;
handle_cast(_Request, State = #map_cache_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #map_cache_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #map_cache_state{}) ->
    ok.

code_change(_OldVsn, State = #map_cache_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


update_map([]) ->
    {ok, yuppers}.
;
update_map([Head = #node{ Id = id, Num = number_connections, Neighbors = neighbors, Location = location}| Tail]) ->
    ets:insert(?TABLE, {Id, Num, Neighbors, Location}),
    update_map(Tail);
update_map(Input) when Input != #node{} ->
    {error_1, "map cache has been given the wrong input type (not a node record)"}
update_map() ->
    {error_2, "map chace, Unkown issue with map update"}