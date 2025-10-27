%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(graph_processor).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).


%%% consider changing these later...
-define(MAXDIST, 5).

-record(graph_processor_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #graph_processor_state{}}.

handle_call(_Request, _From, State = #graph_processor_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #graph_processor_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #graph_processor_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #graph_processor_state{}) ->
    ok.

code_change(_OldVsn, State = #graph_processor_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%% This function should make a request to the module cache for a list of all modules, should get a #modules_for_map{#{module_id, lat, log}} as that is all that is needed for this
retrieve_modules() ->
	
	{}
.

%%% This functions should send a response to our map_cache server to update the cache with new values.
update_cache() ->
	
	{}
	
.



%%% here is where we determine if the neighbor is valid. Note that this does NOT include pruning or logical rules, but simply determines if it COULD be...
threshold(#node{location = {L11, L12}}, #node{location = {L21, L22}}) ->
	Distance = math:sqrt(math:pow(L21 - L11, 2) + math:pow(L22 - L12, 2)),
	case Distance < ?MAXDIST of
		true ->
			{true, Distance};
		_ ->
			{false, Distance}
		end.


	



gabriel_neighbor(Module, [], Found, Number) ->
	Found;
gabriel_neighbor(Module, _, Found, Number) when Number > 2 ->
	Found;
%%% ignore yourself :), double check this - eldon :{
gabriel_neighbor(Module, [Current|Remaining], Found, Number) when Module == Current ->
	gabriel_neighbor(Module, Remaining, Found, Number);
gabriel_neighbor(Module, [Current|Remaining], Found, Number) ->
	case threshold(Module, Current) of
		{true, Distance} ->
			New_Neighbor = #neighbor{id = Current, distance = Distance},
			gabriel_neighbor(Module, Remaining, [New_Neighbor | Found], Number+1);
		_ ->
			gabriel_neighbor(Module, Remaining, Found, Number)
	end.
	
	
	
	
%%% initial setup of the queue where we add all the key values into the queue
%%% the map will be akin to #{key => module_id, Value = #node{}}, Queue =[Module_ids], Finished = [Module_ids]
gabriel_graph(Map, [], _Finished) ->
    case maps:keys(Map) of
        [] ->
            {err, "Error with gabriel_graph, Map is appearing as empty"}; %%% add log here
        Keys ->
            gabriel_graph(Map, Keys, Map)
    end;
gabriel_graph(_Map, [], Finished_Map) ->
	{ok, Finished_Map};
gabriel_graph(Map, [Head|Remaining], Finished) ->
    Current_Node = Map#{Head},
    Neighbor_IDs = maps:keys(Map),
    Neighbors = gabriel_neighbor(Current_Node, Neighbor_IDs, [], 0),
    
    
    %%% adding error hanlding here

    New_Node = Current_Node#node{id = , neighbors = Neighbors},
    New_Map = Map#{Head := New_Node},
    
    gabriel_graph(New_Map, Remaining, Finished).
	
		

	
	
	
