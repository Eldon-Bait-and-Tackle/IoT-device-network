%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(hsn_logger).

-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3,
    send_log/2]).

-define(SERVER, ?MODULE).

-record(logger_state, {}).

%%%===================================================================
%%% api functions
%%%===================================================================

send_log(Module, Message) ->
    gen_server:cast(?SERVER, {log, Module, Message}).




%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(File) ->
    case file:open(File, [write, append, delayed_write, {encoding, utf8}]) of
        {ok, IoDevice} ->
            {ok, IoDevice};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State = #logger_state{}) ->
    {reply, ok, State}.

handle_cast({log, Module, Message}, IoDevice) ->
    
    %%% this is the saving logic of the logging messages
    LogLine = io_lib:format("~p ~p: ~s~n", [erlang:localtime(), Module, Message]),
    file:write(IoDevice, LogLine),
    {noreply, IoDevice};

handle_cast(_Request, State = #logger_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #logger_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #logger_state{}) ->
    ok.

code_change(_OldVsn, State = #logger_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
