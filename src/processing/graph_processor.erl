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

start_link(Manager_pid, Points) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Manager_pid, Points], []).


%%% this is only useful when you are actually running, and you only have one task
init([Manager_pid, Points]) ->
    {ok, GabrielGraph} = gabriel_graph(ModuleMap, [], ModuleMap),
    gen_server:cast(Manager_pid, {ok, GabrielGraph}),
    {stop, normal, #graph_processor_state{}}.
	

%%% this should really only do its process then stop.
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


threshold(#node{location = {L11, L12}}, #node{location = {L21, L22}}) ->
	Distance = math:sqrt(math:pow(L21 - L11, 2) + math:pow(L22 - L12, 2)),
	case Distance < ?MAXDIST of
		true ->
			{true, Distance};
		_ ->
			{false, Distance}
		end.

is_gabriel_neighbor(Module, Current, AllModules) ->
	#node{location = {L11, L12}} = Module,
	#node{location = {L21, L22}} = Current,
	
	Distance = math:sqrt(math:pow(L21 - L11, 2) + math:pow(L22 - L12, 2)),
	Radius = Distance * 0.5,
	
	CenterpointX = (L11 + L21) * 0.5,
	CenterpointY = (L12 + L22) * 0.5,
	
	is_gabriel_neighbor(Module, Current, AllModules, Radius, CenterpointX, CenterpointY).

is_gabriel_neighbor(_Module, _Current, [], _Radius, _CX, _CY) ->
	true;
is_gabriel_neighbor(Module, Current, [Test|Remaining], Radius, CX, CY) when Test == Current; Test == Module ->
	is_gabriel_neighbor(Module, Current, Remaining, Radius, CX, CY);
is_gabriel_neighbor(Module, Current, [#node{location = {TX, TY}}=Test|Remaining], Radius, CX, CY) ->
	TestDistance = math:sqrt(math:pow(CX - TX, 2) + math:pow(CY - TY, 2)),
	
	case TestDistance < Radius of
		true ->
			false;
		false ->
			is_gabriel_neighbor(Module, Current, Remaining, Radius, CX, CY)
	end.


gabriel_neighbor(Module, NeighborIDs, Map, Found, Number) ->
	gabriel_neighbor(Module, NeighborIDs, Map, Found).
gabriel_neighbor(_Module, [], Found, Map) ->
	Found;
gabriel_neighbor(Module, [CurrentID|Remaining], Map, Found) ->
	Current = maps:get(CurrentID, Map),
	
	case Current == Module of
		true ->
			gabriel_neighbor(Module, Remaining, Map, Found);
		false ->
			case is_gabriel_neighbor(Module, Current, maps:values(Map)) of
				true ->
					#node{location = {L11, L12}} = Module,
					#node{location = {L21, L22}} = Current,
					Distance = math:sqrt(math:pow(L21 - L11, 2) + math:pow(L22 - L12, 2)),
					New_Neighbor = #neighbor{id = CurrentID, distance = Distance},
					gabriel_neighbor(Module, Remaining, Map, [New_Neighbor | Found]);
				false ->
					gabriel_neighbor(Module, Remaining, Map, Found)
			end
	end.
	
	
	
	
%%% initial setup of the queue where we add all the key values into the queue
%%% the map will be akin to #{key => module_id, Value = #node{}}, Queue =[Module_ids], Finished = [Module_ids]
gabriel_graph(Map, [], _Finished) ->
    case maps:keys(Map) of
        [] ->
            {err, "Error with gabriel_graph, Map is appearing as empty"};
        Keys ->
            gabriel_graph(Map, Keys, Map)
    end;
gabriel_graph(_Map, [], Finished_Map) ->
	{ok, Finished_Map};
gabriel_graph(Map, [Head|Remaining], Finished) ->
    Current_Node = maps:get(Head, Map),
    Neighbor_IDs = maps:keys(Map),
    
    Neighbors = gabriel_neighbor(Current_Node, Neighbor_IDs, Map, [], 0),
    
    New_Node = Current_Node#node{neighbors = Neighbors},
    New_Map = Map#{Head := New_Node},
    
    gabriel_graph(New_Map, Remaining, Finished).
	
	
	
