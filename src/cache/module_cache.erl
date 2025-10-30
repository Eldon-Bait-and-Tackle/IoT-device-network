%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(module_cache).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3,
    retrieve_location/1, verify_module/3, store_challenge/3, verify_response/3, verify_auth_token/1]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(module_cache_state, {}).
-include("records.hrl").


%%%===================================================================
%%% API functions
%%%===================================================================


retrieve_location(Module_id) ->
    gen_server:call(?SERVER, {retrieve_location, Module_id}).
    
verify_module(Hmac, Chip_id, Module_id) ->
    gen_server:call(?SERVER, {verify_module, Hmac, Chip_id, Module_id}).

store_challenge(Challenge, Module_id, Chip_id) ->
    gen_server:call(?SERVER, {store_challenge, Challenge, Module_id, Chip_id}).

verify_response(Module_id, Chip_id, Response) ->
    gen_server:call(?SERVER, {verify_response, Module_id, Chip_id, Response}).

verify_auth_token(AuthToken) ->
    gen_server:call(?SERVER, {verify_auth_token, AuthToken}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    %%% The keypos is 2 here because the record name is first when the record is converted it is {module, module_id ....}
    ets:new(?TABLE, [set, private, named_table, {keypos, 2}]),
    
    %%% on init I want to have this load all modules from database, once database is added I will include this
    
    
    {ok, #module_cache_state{}}.



handle_call({retrieve_location, Module_id}, _From, State = #module_cache_state{}) ->

    case ets:lookup(?TABLE, Module_id) of
        [#module{location = Location}] ->
            {reply, {ok, Location}, State};
        _ ->
            %% THE MODULE IS NOT IN CACHE, in the future I want to add a database integration so that it will check the database if it exists or not.
            {reply, {err, "Unkown Module Id"}, State}
        end;
        
        
handle_call({verify_module, Hmac, Chip_id, Module_id}, _From, State = #module_cache_state{}) ->

    case ets:lookup(?TABLE, Module_id) of
        [#module{hmac = Hmac, chip_id = Chip_id, module_id = Module_id}] ->
            {reply, {ok, true}, State};
        _ ->
            {reply, {ok, false}, State}
        end,
    {reply, {err, "Module Verification Has Failed"}, State};

handle_call({store_challenge, Challenge, Module_id, Chip_id}, _From, State = #module_cache_state{}) ->
    case ets:lookup(?TABLE, Module_id) of
        [Module = #module{chip_id = Chip_id}] ->
            UpdatedModule = Module#module{challenge = Challenge},
            ets:insert(?TABLE, UpdatedModule),
            {reply, {ok, Challenge}, State};
        _ ->
            {reply, {error, "Module not registered or Chip ID mismatch"}, State}
    end.
handle_call({verify_response, Module_id, Chip_id, Response}, _From, State = #module_cache_state{}) ->
    case ets:lookup(?TABLE, Module_id) of
        [#module{hmac = SecretKey, chip_id = Chip_id, challenge = Challenge}] ->
            
            ExpectedHmac = crypto:hmac(sha256, SecretKey, Challenge),
            IsVerified = (ExpectedHmac == Response),
            UpdatedModule = Module#module{challenge = <<>>},
            ets:insert(?TABLE, UpdatedModule),

            {reply, {ok, IsVerified}, State};
        _ ->
            {reply, {ok, false}, State}
    end;

handle_call(_Request, _From, State = #module_cache_state{}) ->
    {reply, ok, State}.
    
    
    

handle_cast({load_module, Module = #module{}}, State = #module_cache_state{}) ->

    ets:insert(?TABLE, Module),
    {noreply, State};
    
    
handle_cast(_Request, State = #module_cache_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #module_cache_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #module_cache_state{}) ->
    ok.

code_change(_OldVsn, State = #module_cache_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
