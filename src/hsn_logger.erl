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
    send_log/2, send_log/3]).

-define(SERVER, ?MODULE).

-record(logger_state, {}).

%%%===================================================================
%%% api functions
%%%===================================================================

send_log(Module, Message) ->
    gen_server:cast(?SERVER, {log, Module, Message}).


%%% added because apparently this is needed for some of the more complex formatting
send_log(Module, Format, Args) ->
    FormattedMsg = io_lib:format(Format, Args),
    gen_server:cast(?SERVER, {log, Module, FormattedMsg}).




%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    %%% this was breaking it so I am going to disable this for now...
%%    case file:open(File, [write, append, delayed_write, {encoding, utf8}]) of
%%        {ok, IoDevice} ->
%%            {ok, IoDevice};
%%        {error, Reason} ->
%%            {stop, Reason}
%%    end.
    {ok, #logger_state{}}.

handle_call(_Request, _From, State = #logger_state{}) ->
    {reply, ok, State}.

handle_cast({log, Module, Message}, State) ->
    io:format("~p [~p]: ~p~n", [erlang:localtime(), Module, Message]),
    {noreply, State };

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
