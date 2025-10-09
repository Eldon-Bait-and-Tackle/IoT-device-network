%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(transmission_cache).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(transmission_cache_state, {}).

%%%===================================================================
%%% API functions
%%%===================================================================

new_transmission(Transmission) ->
    gen_server:cast(?SERVER, {new_transmission, Transmission}).
    







%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?TABLE, [set, protected, named_table, {keypos, 1}]),
    {ok, #transmission_cache_state{}}.

handle_call(_Request, _From, State = #transmission_cache_state{}) ->
    {reply, ok, State}.
    
    

handle_cast({new_transmission, #transmission{module_id = Mi, temperature = T, moisture = M, battery = B}}, State = #transmission_cache_state{}) ->

    Time = "Temporary",
    
    New_Transmission_Record = #transmission_record{
        time = T,
        temperrature = T,
        moisture = M,
        battery = B
    },


    case ets:lookup(?TABLE, Mi) of
        [{_Mi, Old_Transmission_Records}] ->
            ets:insert(?TABLE, {Mi, [Old_Transmission_Records | New_Transmission_Record]});
            
        [] ->
            %% MODULE IS POSTING TO ONE THAT DOES NOT EXIST !!!!!!!!!!!
            {}
        end,

    {noreply, State};


handle_cast(_Request, State = #transmission_cache_state{}) ->
    {noreply, State}.
    
    
    

handle_info(_Info, State = #transmission_cache_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #transmission_cache_state{}) ->
    ok.

code_change(_OldVsn, State = #transmission_cache_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
