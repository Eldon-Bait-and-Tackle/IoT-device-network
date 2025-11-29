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

    new_transmission/1, register_module/2,
    retrieve_module_data/1, load_modules/0,
    get_latest_transmissions/0]).

-define(SERVER, ?MODULE).

-record(database_handler_state, {
    connection
}).
-include("records.hrl").


%%%===================================================================
%%% API functions
%%%===================================================================

new_transmission(Payload) ->
    gen_server:cast(?SERVER, {new_transmission, Payload}).

register_module(Chip_id, Hmac) ->
    gen_server:call(?SERVER, {register_module, Chip_id, Hmac}).

retrieve_module_data(Module_id) ->
    gen_server:call(?SERVER, {retrieve_module, Module_id}).

get_latest_transmissions() ->
    gen_server:call(?SERVER, {get_latest_transmissions}).

load_modules() ->
    gen_server:call(?SERVER, {load_modules}).

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

handle_call({retrieve_module, Module_id}, _From, State = #database_handler_state{connection = Connection}) ->

    Query = "SELECT module_id, chip_id, hmac, lat, long FROM modules WHERE module_id = $1",
    case epgsql:equery(Connection, Query, [Module_id]) of
        {ok, _Columns, [] } ->
            {reply, {error, "Module Not Found"}, State};
        {ok, _Columns, [Row]} ->
            {reply, {ok, row_to_module_record(Row)}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({load_modules}, _From, State = #database_handler_state{connection = Connection}) ->
    
    Query = "SELECT module_id, chip_id, hmac, lat, long FROM modules",
    case epgsql:equery(Connection, Query, []) of
        {ok, _Cols, Rows} ->
            Recs = [row_to_module_record(R) || R <- Rows],
            {reply, {ok, Recs}, State};
        {error, Reason} ->
            hsn_logger:send_log(?MODULE, "DB Error loading module cache: ~p ", [Reason]),
            {reply, {error, Reason}, State}
    end;
    
    
    

%%%% FIX
%%% Consider figuring out how to use SQL ON CONFLICT, this contians a race condition
handle_call({register_module, Chip_id, Hmac}, _From, State = #database_handler_state{connection = Connection}) ->

    FindQuery = "SELECT module_id, chip_id, hmac, lat, long FROM modules WHERE chip_id = $1",

    case epgsql:equery(Connection, FindQuery, [Chip_id]) of
        {ok, _Columns, [_Row]} ->
            hsn_logger:send_log(?SERVER, "This request is attempting to register a module that already exsits...?"),
            {reply, {error, "This request is attempting to register a module that already exsits...?"}, State};
        {ok, _Columns, []} ->
            %%% DEV
            InsertQuery = "INSERT INTO modules (chip_id, hmac) VALUES ($1, $2) "
            "RETURNING module_id, chip_id, hmac, lat, long",

            case epgsql:equery(Connection, InsertQuery, [Chip_id, Hmac]) of
                {ok, _Count, _Columns2, [NewRow]} ->
                    hsn_logger:send_log(?SERVER, "The new module has been added to the database, loading"),
                    NewModuleRecord = row_to_module_record(NewRow),
                    module_cache:load_module(NewModuleRecord),
                    {reply, {ok, NewModuleRecord#module.module_id}, State};
                {error, Reason} ->
                    hsn_logger:send_log(?SERVER, Reason),
                    {reply, {error, Reason}, State}
            end;
        {ok, _COUNT} ->
            hsn_logger:send_log(?SERVER, "database handler is getting that weird problem where it only returns the count.. :("),
            {reply, {error, count_problem}, State};
        {error, Reason} ->
            hsn_logger:send_log(?SERVER, Reason),
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
            {reply, Records, State};
        Error ->
            hsn_logger:send_log(?MODULE, "DB Error loading cache: ~p", [Error]),
            {reply, {error, Error}, State}
    end;

handle_call(_Request, _From, State = #database_handler_state{}) ->
    {reply, ok, State}.



handle_cast({new_transmission, Payload}, State = #database_handler_state{connection = Connection}) ->

    {M, T, M, B} = Payload,
    Module_id = binary_to_integer(M),
    Temperature = binary_to_float(T),
    Moisture = binary_to_float(M),
    Battery = binary_to_integer(B),



    Query = "INSERT INTO transmission (module_id, time, temperature, moisture, battery) "
        "VALUES ($1, NOW(), $2, $3, $4) "
        "RETURNING transmission_id, module_id, time, temperature, moisture, battery",

    Params = [Module_id, Temperature, Moisture, Battery],

    case epgsql:equery(Connection, Query, Params) of
        {ok, _Count, _Columns, [Row]} ->
            NewRecord = row_to_transmission_record(Row),
            transmission_cache:new_transmission(NewRecord),
            {noreply, State};
        {error, Reason} ->
            hsn_logger:send_log(?SERVER, Reason),
            {noreply, State};
        _Other ->
            hsn_logger:send_log(?SERVER, "epgsql has failed with unknown issues"),
            {noreply, State}
    end;

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

row_to_module_record({Module_id, Chip_id, Hmac, Lat, Long}) ->
    #module{
        module_id = Module_id,
        chip_id = Chip_id,
        hmac = Hmac,
        location = {
            case Lat of null -> 0.0; _ -> Lat end,
            case Long of null -> 0.0; _ -> Long end
        }
    }.