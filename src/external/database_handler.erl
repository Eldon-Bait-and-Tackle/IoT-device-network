%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(database_handler).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3,

    register_module/1, claim_module/2, update_module_location/3,
    new_transmission/1,
    retrieve_module_data/1, 
    
    load_modules/0, load_transmission_cache/0]).

-define(SERVER, ?MODULE).

-record(database_handler_state, {
    connection
}).
-include("records.hrl").


%%%===================================================================
%%% API functions
%%%===================================================================

%%% Modules

new_transmission(Payload) ->
    gen_server:call(?SERVER, {new_transmission, Payload}).

register_module(Secret) ->
    gen_server:call(?SERVER, {register_module, Secret}).

claim_module(UserId, Secret) ->
    gen_server:call(?SERVER, {claim_module, UserId, Secret}).

retrieve_module_data(Module_id) ->
    gen_server:call(?SERVER, {retrieve_module, Module_id}).

load_modules() ->
    gen_server:call(?SERVER, {load_modules}).

%%% Transmissions

get_latest_transmissions() ->
    gen_server:call(?SERVER, {get_latest_transmissions}).

load_transmission_cache() ->
    gen_server:call(?SERVER, {load_transmission_cache}).

update_module_location(Module_id, Lat, Long) ->
    gen_server:call(?SERVER, {update_module_location, Module_id, Lat, Long}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    DbConfig = application:get_env(hsn_app, db_settings, #{}),
    io:format("DEBUG: Connecting to DB with: ~p~n", [DbConfig]),


    case connection(DbConfig) of
        {ok, Connection} ->
            {ok, #database_handler_state{connection = Connection}};
        {error, Reason} ->
            io:format("DEBUG: Connecting to DB with: ~p~n", [Reason]),
            {stop, Reason}
    end.


%%%===================================================================
%%% Modules
%%%===================================================================

handle_call({retrieve_module, Module_id}, _From, State = #database_handler_state{connection = Connection}) ->

    Query = "SELECT module_id, secret_key, lat, long, owner_id, is_claimed FROM modules WHERE module_id = $1",
    case epgsql:equery(Connection, Query, [Module_id]) of
        {ok, _Columns, [] } ->
            {reply, {error, module_not_found}, State};
        {ok, _Columns, [Row]} ->
            {reply, {ok, row_to_module_record(Row)}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({update_module_location, Module_id, Lat, Long}, _From, State = #database_handler_state{connection = Connection}) ->

    Query = "UPDATE modules "
            "SET LAT = $1, LONG = $2, "
            "WHERE module_id = $3;",
    
    case epgsql:equery(Connection, Query, [Lat, Long, Module_id]) of
        {ok, _Cols, _Rows} ->
            {reply, {ok, ok}, State};
        {error, Reason} ->
            hsn_logger:send_log(?SERVER, Reason),
            {reply, {error, Reason}, State};
        _ ->
            hsn_logger:send_log(?SERVER, "something has gone wrong with update module location"),
            {reply, {error, unknown}, State}
    end;

handle_call({load_modules}, _From, State = #database_handler_state{connection = Connection}) ->

    Query = "SELECT module_id, secret_key, lat, long, owner_id, is_claimed FROM modules",
    case epgsql:equery(Connection, Query, []) of
        {ok, _Cols, Rows} ->
            Recs = [row_to_module_record(R) || R <- Rows],
            {reply, {ok, Recs}, State};
        {error, Reason} ->
            hsn_logger:send_log(?MODULE, "DB Error loading module cache: ~p ", [Reason]),
            {reply, {error, Reason}, State}
    end;



handle_call({register_module, Secret}, _From, State = #database_handler_state{connection = C}) ->
    %% Try to find existing module by Secret
    Query = "SELECT module_id FROM modules WHERE secret_key = $1",
    case epgsql:equery(C, Query, [Secret]) of
        {ok, _, [{ModuleId}]} ->
            {reply, {ok, ModuleId}, State};
        {ok, _, []} ->
            Insert = "INSERT INTO modules (secret_key, is_claimed) VALUES ($1, FALSE) RETURNING module_id, secret_key, lat, long, owner_id, is_claimed",
            case epgsql:equery(C, Insert, [Secret]) of
                {ok, _, _, [Row]} ->
                    Result = row_to_module_record(Row),
                    _ = module_cache:load_module(Result),
                    {reply, {ok, Result#module.module_id}, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({claim_module, UserId, Secret}, _From, State = #database_handler_state{connection = C}) ->
    Query = "UPDATE modules SET is_claimed = TRUE, owner_id = $1 WHERE secret_key = $2 AND is_claimed = FALSE RETURNING module_id",
    case epgsql:equery(C, Query, [UserId, Secret]) of
        {ok, _, _, [{ModID}]} ->
            module_cache:load_modules(),
            {reply, {ok, ModID}, State};
        _ ->
            {reply, {error, invalid_secret}, State}
    end;



%%%===================================================================
%%% Transmissions
%%%===================================================================


handle_call({load_transmission_cache}, _From, State = #database_handler_state{connection = Connection}) ->

    Query = "SELECT transmission_id, module_id, time, temperature, moisture, battery "
    "FROM transmission "
    "WHERE time >= NOW() - INTERVAL '7 days' "
    "ORDER BY module_id, time DESC",

    case epgsql:equery(Connection, Query) of
        {ok, _Columns, []} ->
            {reply, {ok, []}, State};
        {ok, _Columns, Rows} ->
            Records = [row_to_transmission_record(R) || R <- Rows],
            {reply, {ok, Records}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_latest_transmissions}, _From, State = #database_handler_state{connection = Connection}) ->

    Query = "SELECT DISTINCT ON (module_id) "
    "transmission_id, module_id, time, temperature, moisture, battery "
    "FROM transmission "
    "ORDER BY module_id, time DESC",

    case epgsql:equery(Connection, Query) of
        {ok, _Cols, Rows} ->
            Records = [row_to_transmission_record(R) || R <- Rows],
            {reply, {ok, Records}, State};
        Error ->
            hsn_logger:send_log(?MODULE, "DB Error loading cache: ~p", [Error]),
            {reply, {error, Error}, State}
    end;

handle_call({new_transmission, {Mid, T, M, B}}, _From, State = #database_handler_state{connection = Connection}) ->

    Module_id = binary_to_integer(Mid),
    Temperature = safe_to_float(T),
    Moisture = safe_to_float(M),
    Battery = safe_to_float(B),

    Query = "INSERT INTO transmission (module_id, time, temperature, moisture, battery) "
    "VALUES ($1, NOW(), $2, $3, $4) "
    "RETURNING transmission_id, module_id, time, temperature, moisture, battery",

    Params = [Module_id, Temperature, Moisture, Battery],

    case epgsql:equery(Connection, Query, Params) of
        {ok, _Count, _Columns, [Row]} ->
            NewRecord = row_to_transmission_record(Row),
            transmission_cache:new_transmission(NewRecord),
            {reply, {ok, NewRecord#transmission.time}, State};
        {error, Reason} ->
            hsn_logger:send_log(?SERVER, Reason),
            {reply, {error, Reason}, State};
        _Other ->
            hsn_logger:send_log(?SERVER, "epgsql has failed with unknown issues"),
            {reply, {error, unknown}, State}
    end;




handle_call(_Request, _From, State = #database_handler_state{}) ->
    {reply, ok, State}.





handle_cast(_Request, State = #database_handler_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #database_handler_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #database_handler_state{}) ->
    ok.

code_change(_OldVsn, State = #database_handler_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


safe_to_float(Bin) ->
    try binary_to_float(Bin)
    catch
        error:badarg ->
            try float(binary_to_integer(Bin))
            catch _:_ -> 0.0 end
    end.


connection(Config) ->
    epgsql:connect(Config).


row_to_transmission_record({Transmission_id, Module_id, Time, Temperature, Moisture, Battery}) ->
    #transmission{
        transmission_id = Transmission_id,
        module_id = Module_id,
        time = Time,
        temperature = Temperature,
        moisture = Moisture,
        battery = Battery
    }.

row_to_module_record({Module_id, Secret, Lat, Long, Owner_id, Claimed}) ->
    #module{
        module_id = Module_id,
        secret_key = Secret,
        owner_id = Owner_id,
        is_claimed = Claimed,
        location = {
            case Lat of null -> 0.0; _ -> Lat end,
            case Long of null -> 0.0; _ -> Long end
        }
    }.