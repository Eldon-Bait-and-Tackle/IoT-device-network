%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(transmission_handler).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(transmission_handler_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([Trasmission_data, ]) ->
    
    {ok, #transmission_handler_state{}}.

terminate(_Reason, _State = #transmission_handler_state{}) -> ok.

handle_call(_Request, _From, State = #transmission_handler_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #transmission_handler_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #transmission_handler_state{}) ->
    {noreply, State}.



code_change(_OldVsn, State = #transmission_handler_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

module_validation(Module_Validation) ->
