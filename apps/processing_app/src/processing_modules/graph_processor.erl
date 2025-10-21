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
-define(MaxDist, 5).
-define(MaxLat, 5).
-define(MaxLong, 3).

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
threshold(#node{long = L11, lat = L12}, #node{long = L21, lat=L22} ->
	Distance = math:sqrt(math:pow(L21 - L11) + math:pow(L22 - L11))
	case Distance < MaxDist of
		true ->
			true;
		_ ->
			false
		end.





gabriel_neighbor(Module, [], Found, Number) ->
	{Found, Number};
gabriel_neighbor(Module, _, Found, Number) when Number > 2 ->
	{Found, Number};
gabriel_neighbor(Module, [Current|Remaining], Found, Number) ->
	case threshold(Module, Current) of
		true ->
			gabriel_neighbor(Module, Remaining, [Found|Current], Number+1);
		_ ->
			gabriel_neighbor(Module. Remaining, Found, Number)
		end.
	
	
	
	
%%% initial setup of the queue where we add all the key values into the queue
gabriel_graph(Map, [], []) ->
		case maps:keys(Map) of
			[] ->
				err;
			Keys ->
				gabriel_graph(Map, Keys, [])
		end;
gabriel_graph(#{}, Queue, Finished) ->
	[Finished | Queue];
gabriel_graph(Map, Queue, Finished) ->
	
	
