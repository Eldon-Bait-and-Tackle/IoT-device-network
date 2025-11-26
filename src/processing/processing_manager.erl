%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(processing_manager).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3,
    heuristics_processing/0, graph_processing/0
]).

-define(SERVER, ?MODULE).
-define(TICK, ?MODULE).

-record(processing_manager_state, {
    timer_reference
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

heuristics_processing() ->
    gen_server:cast(?SERVER, {heuristics}).

graph_processing() ->
    gen_server:cast(?SERVER, {graph}).




%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Timer = erlang:send_after(?TICK, self(), tick),
    {ok, #processing_manager_state{timer_reference = Timer}}.

handle_call(_Request, _From, State = #processing_manager_state{}) ->
    {reply, ok, State}.


handle_cast({heuristics}, State = #processing_manager_state{}) ->
    {ok, _Pid} = supervisor:start_child(heuristic_processor, [self()]),
    {noreply, State};
handle_cast(_Request, State = #processing_manager_state{}) ->
    {noreply, State}.



handle_info(tick, State#{timer_reference = OldTimerRef}) ->
    erlang:cancel_timer(OldTimerRef),
    process(),
    Timer = erlang:send_after(?TICK, self(), tick),
    {noreply, State#{timer_reference = Timer}};

handle_info(_Info, State = #processing_manager_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #processing_manager_state{}) ->
    ok.

code_change(_OldVsn, State = #processing_manager_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


process() ->
