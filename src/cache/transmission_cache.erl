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
    new_transmission/1, get_general_last_reading/1, get_recent_reading/1, load_from_db/0]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

%%% Table Design, {Moudle_id, [tranmissions]}.

-record(transmission_cache_state, {}).
-include("records.hrl"). %% doule check this on I have no idea if this is how this actually works...

%%%===================================================================
%%% API functions
%%%===================================================================

new_transmission(Transmission) ->
    gen_server:cast(?SERVER, {new_transmission, Transmission}).
    
get_general_last_reading(Module_id) ->
    gen_server:call(?SERVER, {get_general_last_reading, Module_id}).

get_recent_reading(Id) ->
    case ets:lookup(?TABLE, Id) of
         [] ->
             {error, not_found};
         [{Id, [LatestRecord | _]}] ->
             {ok, LatestRecord}
     end.


load_from_db() -> gen_server:cast(?SERVER, {load_from_db}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?TABLE, [set, protected, named_table, {keypos, 1}]),
    load_from_db(),
    {ok, #transmission_cache_state{}}.


%%% FIX
%%% This blocks the gen server, consdier having a smaller return rather than the entire table in the future? or break this function into its own handler... idk
handle_call({get_general_last_reading, _Module_id}, _From, State = #transmission_cache_state{}) ->
        Result = ets:foldl(
            fun({Module_id, [Transmission | _]}, Acc) ->
                [{Module_id, Transmission} | Acc];
            (_, Acc) ->
                Acc
            end,
            [],
            ?TABLE
        ),
    {reply, {ok, Result}, State};
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


handle_cast({load_from_db}, State) ->
    case database_handler:load_transmission_cache() of
        {ok, Transmissions} ->
            GroupedMap = group_by_module(Transmissions),
            GroupedList = maps:to_list(GroupedMap),
            ets:insert(?TABLE, GroupedList),
            hsn_logger:send_log(?MODULE, "Cache warm-up complete. Loaded latest records."),
            {noreply, State};
        {error, _Reason} ->
            hsn_logger:send_log(?MODULE, "Failed to load most recent transmissions"),
            {noreply, State}
    end;
handle_cast({new_transmission, Transmission = #transmission{module_id = Module_id}}, State = #transmission_cache_state{}) ->
    %%% Module is preverified in a previous step, no need to check it beyond this point unless desired
    case ets:lookup(?TABLE, Module_id) of
        
        [{Module_id, Old_transmissions}] ->
            ets:insert(?TABLE, {Module_id, [Transmission | Old_transmissions]}),
            {noreply, State};
        [{Module_id, []}] ->
            ets:insert(?TABLE, {Module_id, [Transmission]}),
            {noreply, State};
        _ ->
            
            %%% FIX LATER, VALIDATE THAT THIS IS A REGISTERED MODULE BEFORE ALLOWING INSERTION, also you should really add
            %%% this from the module cache instead.
            ets:insert(?TABLE, {Module_id, [Transmission]}),
            hsn_logger:send_log(?MODULE, "Module Transmission cached (first entry): ~p", [Module_id]),
            {noreply, State}
        
        end
    ;


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


group_by_module(Records) ->
    lists:foldl(
        fun(Rec = #transmission{module_id = Mid}, Acc) ->
            Map = maps:get(Mid, Acc, []),
            maps:put(Mid, [Rec | Map], Acc)
        end,
        #{},
        Records
    ).




