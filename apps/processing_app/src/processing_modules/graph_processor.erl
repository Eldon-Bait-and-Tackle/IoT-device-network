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


%%% (#node, [#pre_node])

build_node(#node(id = Module_id, number_connections = Number) = Node, _) when Number >= 3 > ->
	Node.
build_node(Module, Modules_Not_Checked) ->
	
	
	
	
	.
