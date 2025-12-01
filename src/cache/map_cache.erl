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
    new_map/1, get_neighbors/1, get_node/1, get_map/0]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(map_cache_state, {}).
-include("records.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

new_map(Map) ->
    gen_server:call(?SERVER, {new_map, Map}).

get_node(Nid) ->
    gen_server:call(?SERVER, {get_node, Nid}).

%%% this function will return the {listofneighbors, numberofneighbors}, please refer to doc for more details on this choice
get_neighbors(Nid) ->
    gen_server:call(?SERVER, {get_neighbors, Nid}).


get_map() ->
    Objects = ets:tab2list(?TABLE),
    lists:map(fun node_to_map/1, Objects).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?TABLE, [set, public, named_table, {keypos, 2}]),
    {ok, #map_cache_state{}}.

handle_call({get_neighbors, Nid}, _From, State = #map_cache_state{}) ->
    
    case ets:lookup(?TABLE, Nid) of
        [#node{neighbors = Neighbors}] ->
            {reply, {ok, Neighbors}, State};
        [] ->
            {reply, {error, "map cache failed to find the module of given ID, "}, State}
        end;


handle_call({get_node, Nid}, _From, State = #map_cache_state{}) ->

    case ets:lookup(?TABLE, Nid) of
        [Result = #node{}] ->
            {reply, {ok, Result}, State};
        [] ->
            {reply, {error, "map cache failed to find the module of given ID, "}, State}
        end;


handle_call({new_map, Map}, _From,  State = #map_cache_state{}) ->
    case update_map(Map) of
        {ok, complete} ->
            {reply, ok, State};
        {error_1, MSG} ->
            hsn_logger:send_log(?SERVER, MSG),
            {reply, {error, MSG}, State};
        {error_2, MSG} ->
            hsn_logger:send_log(?SERVER, MSG),
            {reply, {error, MSG}, State};
        _ ->
            hsn_logger:send_log(?SERVER, "Super unkown problem in the map cache"),
            {reply, {error, "UNKOWN"}, State}
    end
;

handle_call(_Request, _From, State = #map_cache_state{}) ->
    {reply, ok, State}.

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


node_to_map(#node{id = Id, location = {Lat, Long}, number_connections = Conns, neighbors = Neighbors}) ->
    #{
        <<"id">> => Id,
        <<"location">> => [Lat, Long], %% Convert tuple {Lat, Long} to List [Lat, Long]
        <<"connections">> => Conns,
        <<"neighbors">> => Neighbors
    }.



update_map([]) ->
    {ok, complete};
update_map([Head| Tail]) when is_record(Head, node) ->
    ets:insert(?TABLE, Head),
    update_map(Tail);
update_map(_) ->
    hsn_logger:send_log(?SERVER, "map cache update map has failed"),
    {error_2, "map chace, Unkown issue with map update"}.