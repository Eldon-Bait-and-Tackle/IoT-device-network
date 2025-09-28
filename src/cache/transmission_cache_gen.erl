%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(transmission_cache_gen).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-export([
    add_transmission/1,
    get_module/1,
    is_loaded/1,
    unload/1,
    get_general/0,
    database_dump/0,
    check/0
]).


-define(SERVER, ?MODULE).

-include("records.hrl").

%%%===================================================================
%%% API
%%%===================================================================


add_transmission(Transmissions) ->
    gen_server:cast(?MODULE, {add, Transmissions}).


get_module(Key) ->
    gen_server:call(?MODULE, {get, Key}).


is_loaded(Key) ->
    gen_server:call(?MODULE, {loaded, Key}).
    

unload(Key) ->
    gen_server:cast(?MODULE, {unload, Key}).


get_general() ->
    gen_server:call(?MODULE, {general}).
    
    
database_dump() ->
    gen_server:cast(?MODULE, {dump}).


check() ->
    gen_server:cast(?MODULE, {check}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    %% I am going to add the load in here, I just need to find a way to manage loading this module after the database module...
    {ok, #{}}.


%%% This cast is for getting the exact data from a module. Currently it is required to 'own' this module by having all the personal information about it.

handle_call({get, Key = #module_data{module_id = External_id}}, _From, State = #transmission_cache_gen_state{transmissions = Transmissions}) ->
    
    case maps:get(External_id, Transmissions, not_found) of
        not_found ->
            %% I want to add database checking here, If we cant find your module, then we should simply
            {reply, {error, not_found}, State};

        {Internal_data, Module_transmissions} ->
        
            %% NEVER RETURN MODULE DATA, IT CONTAINS USER DATA, SHOULD CHANGE THIS.
            if 
                Key == Internal_data ->
                    {reply, {ok, Module_transmissions}, State};
                
                true ->
                {reply, {error, key_missmatch}, State}
            end;
                            
        _ ->
            {reply, {error, not_found}, State}
    end.




handle_cast({add, Module_data = #module_data{}, Transmission = #transmission{}}, State = #transmission_cache_gen_state{}) ->
{noreply, State}.


            
    



handle_info(_Info, State = #transmission_cache_gen_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #transmission_cache_gen_state{}) ->
    ok.

code_change(_OldVsn, State = #transmission_cache_gen_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
