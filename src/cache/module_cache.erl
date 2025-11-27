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
    retrieve_location/1, load_module/1, get_module_map/0, get_all_ids/0,
    store_challenge/2, verify_response/2, get_module_data/1,
    load_modules/0]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(module_cache_state, {}).
-include("records.hrl").


%%%===================================================================
%%% API functions
%%%===================================================================


%%% User access options


%%% a general release of data is required to have a user authentication for any values beyond processed anonomised user statistics :)
get_module_data(User_Auth) ->
    gen_server:call({get_module_data, User_Auth}).

retrieve_location(Module_id) ->
    gen_server:call(?SERVER, {retrieve_location, Module_id}).
    
store_challenge(Challenge, Module_id) ->
    gen_server:call(?SERVER, {store_challenge, Challenge, Module_id}).

verify_response(Module_id, Response) ->
    gen_server:call(?SERVER, {verify_response, Module_id, Response}).

load_module(Module = #module{}) ->
    gen_server:cast(?SERVER, {load_module, Module}).

get_module_map() ->
    gen_server:call(?SERVER, {get_module_map}).

get_all_ids() ->
    gen_server:call(?SERVER, {get_all_ids}).

load_modules() ->
    gen_server:cast(?SERVER, load_modules).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?TABLE, [set, private, named_table, {keypos, 2}]),
    load_modules(),
    {ok, #module_cache_state{}}.




%%% NOT CURRENTLY IMPLEMENTED
handle_call({get_module_data, _User_auth}, _FROM, State= #module_cache_state{}) ->

    {reply, {ok, null}, State}


;

handle_call({retrieve_location, Module_id}, _From, State = #module_cache_state{}) ->
    case safe_lookup(Module_id) of
        [#module{location = Location}] ->
            {reply, {ok, Location}, State};
        _ ->
            %% THE MODULE IS NOT IN CACHE, in the future I want to add a database integration so that it will check the database if it exists or not.
            {reply, {err, "Unkown Module Id"}, State}
        end;
        
handle_call({store_challenge, Challenge, Module_id}, _From, State = #module_cache_state{}) ->
    case safe_lookup(Module_id) of
        [Module] ->
            UpdatedModule = Module#module{challenge = Challenge},
            ets:insert(?TABLE, UpdatedModule),
            {reply, {ok, null}, State};
        _ ->
            {reply, {error, "Module not registered or Chip ID mismatch"}, State}
    end;

handle_call({verify_response, Module_id, Response}, _From, State = #module_cache_state{}) ->
    case safe_lookup(Module_id) of
        [Module = #module{hmac = SecretKey, chip_id = Chip_id, challenge = Challenge}] ->
            case Challenge of
                undefined ->
                    IsVerified = (SecretKey =:= Response),
                    {reply, {ok, IsVerified}, State};
            _ ->
                ExpectedMac = crypto:mac(hmac, sha256, SecretKey, Challenge), %%% This might be broken.. from :hmac/3 to :mac/4...
                IsVerified = (SecretKey =:= Response),
                UpdatedModule = Module#module{challenge = undefined},
                ets:insert(?TABLE, UpdatedModule),
                {reply, {ok, IsVerified}, State}
            end;
        [] ->
            {reply, {ok, false}, State}
    end;

handle_call({get_module_map}, _From, State = #module_cache_state{}) ->
    Map = ets:foldl(
        fun(#module{module_id = Id, location = Loc}, Acc) ->
            Acc#{Id => #node{id = Id, location = Loc}}
        end,
        #{},
        ?TABLE
    ),
    {reply, {ok, Map}, State};

handle_call({get_all_ids}, _From, State = #module_cache_state{}) ->
    List = ets:foldl(
        fun(#module{module_id = Id}, Acc) ->
            [Id | Acc]
        end,
        [],
        ?TABLE
    ),
    {reply, {ok, List}, State};

handle_call(_Request, _From, State = #module_cache_state{}) ->
    {reply, ok, State}.
    
    
    
handle_cast(load_modules, State = #module_cache_state{}) ->
    case database_handler:load_modules() of
        {ok, Modules} ->
            ets:insert(?TABLE, Modules),
            hsn_logger:send_log(?MODULE, io_lib:format("Module cache loaded ~p devices.", [length(Modules)])),
            {noreply, State};
        {error, _Reason} ->
            hsn_logger:send_log(?MODULE, "Module Loading has failed"),
            {noreply, State}
    end;
        
        
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


safe_lookup(Id) ->
    case ets:lookup(?TABLE, Id) of
        [] when is_integer(Id) ->
            ets:lookup(?TABLE, integer_to_binary(Id));
        [] when is_binary(Id) ->
            try ets:lookup(?TABLE, binary_to_integer(Id))
            catch _:_ -> [] end;
        Result -> Result
    end.