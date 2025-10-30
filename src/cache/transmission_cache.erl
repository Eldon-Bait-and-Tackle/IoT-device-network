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
    code_change/3,
    new_transmission/1, get_general_last_reading/1]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(transmission_cache_state, {}).
-include("records.hrl"). %% doule check this on I have no idea if this is how this actually works...

%%%===================================================================
%%% API functions
%%%===================================================================

new_transmission(Transmission) ->
    gen_server:cast(?SERVER, {new_transmission, Transmission}).
    
get_general_last_reading(Module_id) ->
    gen_server:call(?SERVER, {get_general_last_reading, Module_id}).

get_recent_reading(Module_Id) ->
    gen_server:call(?SERVER, {get_recent_reading, Module_Id}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?TABLE, [set, protected, named_table, {keypos, 1}]),
    {ok, #transmission_cache_state{}}.

handle_call({get_general_last_reading, _Module_id}, _From, State = #transmission_cache_state{}) ->
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
        %% might need to include list:reverse here if this causes problems later. It is simply in backwards order which shouln't releasitcally matter.
    {reply, Result, State};
handle_call({get_last_reading, Id}, _From, State = #transmission_cache_state{}) ->
    Result = case ets:lookup(?TABLE, Id) of
                 [] ->
                     {err, not_found};
                 [{Id, [LatestRecord | _]}] ->
                     {ok, LatestRecord}
             end,   
    {reply, Result, State};
handle_call(_Request, _From, State = #transmission_cache_state{}) ->
    {reply, ok, State}.
    
    

handle_cast({new_transmission, Transmission = #transmission{module_id = Module_id, chip_id = Chip_id, hmac = Hmac}}, State = #transmission_cache_state{}) ->

    case module_cache:verify_module(Module_id, Chip_id, Hmac) of 
        %%% Module is verified and it's transmission is valid
        {ok, true} ->
            %% stole this from the internet, will ahve to test if this is intended, it claims to be compatibvle with sql database datetime...
            Time = calendar:system_time_to_rfc3339(erlang:system_time(millisecond), [{unit, millisecond}, {time_designator, false}, {separator, $s}, {template, "Y-M-D h:m:sZ"}]),
            New_record = transmission_to_record(Transmission, Time, Module_id),
            case ets:lookup(?TABLE, Module_id) of
                [{Module_id, Old_records}] ->
                    ets:insert(?TABLE, {Module_id, [Old_records | New_record]}),
                    {noreply, ok, State};
                [{Module_id, []}] ->
                    ets:insert(?TABLE, {Module_id, [New_record]}),
                    {noreply, ok, State};
                _ ->
                    %%% THIS STATE ACTUALLY JUST MEANS THAT THE MODULE EXISTS BUT IS NOT IN CACHE, I need a database connection here so that when this happens it will load the record instead of err
                    {noreply, {err, "Module has been verified but does not exist in cache"}}
            end;

        {ok, false} ->
            {noreply, {err, "Module has failed to verify"}}
        end;



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


transmission_to_record(Trasmission = #transmission{temperature = Temperature, moisture = Moisture, battery = Battery}, Time, Module_id) ->
    #transmission_record{
    
        %%% The transmission record will be added when the transmission record is added into the database...
    
        module_id = Module_id,
        time = Time,
        temperature = Temperature,
        moisture = Moisture,
        battery = Battery
    }.