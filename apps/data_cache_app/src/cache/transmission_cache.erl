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
-include("records.hrl") %% doule check this on I have no idea if this is how this actually works...

%%%===================================================================
%%% API functions
%%%===================================================================

new_transmission(Transmission) ->
    gen_server:cast(?SERVER, {new_transmission, Transmission}).
    
get_general_last_reading(Module_id) ->
    gen_server:call(?SERVER, {get_general_last_reading, Module_id}).






%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?TABLE, [set, protected, named_table, {keypos, 1}]),
    {ok, #transmission_cache_state{}}.


handle_call({get_general_last_reading, Module_id}, _From, State = #transmission_cache_state{}) ->
    
        %% here they are asking for the most modern readings from each module, this will only pull values from the cache and I should  add a smaller cache that contains each of these values.
        
        Result = ets:foldl(
        
            fun({Module_id, [Transmission | _]}, Acc) ->
                [{Module_id, Transmission} | Acc];
            (_, Acc) ->
                Acc
            end,
            [],
            ?TABLE
        ),
        
        %% might need to include list:reverse here if this cuases problems later. It is simply in backwards order which shouln't releasitcally matter.

    {reply, Result, State};
handle_call(_Request, _From, State = #transmission_cache_state{}) ->
    {reply, ok, State}.
    
    

handle_cast({new_transmission, #transmission{hmac = Hmac, module_id = Module_id, chip_id = Chip_id, temperature = Temperature,
    moisture = Moisture, battery = Battery}}, State = #transmission_cache_state{}) ->

    %% stole this from the internet, will ahve to test if this is intended, it claims to be compatibvle with sql database datetime...
    Time = calendar:system_time_to_rfc3339(erlang:system_time(millisecond), [{unit, millisecond}, {time_designator, false}, {separator, $s}, {template, "Y-M-D h:m:sZ"}]).
    
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
