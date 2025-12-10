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


-define(HOUR, 3600000).
-define(SERVER, ?MODULE).
-define(TICK, 36000 * 1).

-record(processing_manager_state, {timer_reference}).

heuristics_processing() ->
    gen_server:cast(?SERVER, {heuristics}).

graph_processing() ->
    gen_server:cast(?SERVER, {graph}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    graph_processing(),
    timer:sleep(6000),
    heuristics_processing(),
    Timer = erlang:send_after(?TICK, self(), tick),
    {ok, #processing_manager_state{timer_reference = Timer}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({heuristics}, State) ->
    heuristic_processor:start_link(),
    {noreply, State};

handle_cast({graph}, State) ->
    graph_processor:start_link(),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(tick, State = #processing_manager_state{timer_reference = OldTimerRef}) ->
    erlang:cancel_timer(OldTimerRef),

    graph_processing(),
    timer:sleep(1000),
    heuristics_processing(),

    Timer = erlang:send_after(?TICK, self(), tick),
    {noreply, State#processing_manager_state{timer_reference = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.