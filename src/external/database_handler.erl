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
    retrieve_module_data/1, retrieve_all_modules_data/0, load_module/1,
    retrieve_all_modules_last_transmissions/0]).

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

retrieve_all_modules_data() ->
    gen_server:call(?SERVER, {retrieve_all_modules_data}).

retrieve_all_modules_last_transmissions() ->
    gen_server:call(?SERVER, {retrieve_all_modules_last_transmissions}).

load_module(Module) ->
    gen_server:cast(?SERVER, {load_modules, Module}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, DbConfig} = application:get_env(hsn_app, db_settings),
    {ok, Conn} = epgsql:connect(DbConfig),
    {ok, #database_handler_state{connection = Conn}}.

handle_call({retrieve_module, Module_id}, _From, State = #database_handler_state{connection = Connection}) ->

    Query = "SELECT module_id, chip_id, hmac, location FROM modules WHERE module_id = $1",
    case epgsql:squery(Connection, Query, [Module_id]) of
        {ok, _Columns, [] } ->
            {reply, {err, "Module Not Found"}, State};
        {ok, _Columns, [Row]} ->
            {reply, {ok, row_to_module_record(Row)}, State};
        {error, Reason} ->
            {reply, {err, Reason}, State}
    end;

handle_call({retrieve_all_modules_data}, _From, State = #database_handler_state{connection = Connection}) ->

    Query = "SELECT module_id, chip_id, hmac, location FROM modules",
    case epgsql:squery(Connection, Query) of
        {ok, _Columns, []} ->
            {reply, {err, "Unable to find any modules? "}, State};
        {ok, _Columns, Rows} ->
            {reply, {ok, row_to_module_record_helper(Rows)}, State};
        {error, Reason} ->
            logger:send_log(?SERVER, "An error has occured with retreive_all_module_data"),
            {reply, {err, Reason}, State}
    end;


%%%% FIX
%%% Consider figuring out how to use SQL ON CONFLICT, this contians a race condition
handle_call({register_module, Chip_id, Hmac}, _From, State = #database_handler_state{connection = Connection}) ->
    FindQuery = "SELECT module_id, chip_id, user_id, hmac, location FROM modules WHERE chip_id = $1",

    case epgsql:squery(Connection, FindQuery, [Chip_id]) of
        {ok, _Columns, [Row]} ->
            logger:send_log(?SERVER, "This request is attempting to register a module that already exsits...?"),
            {reply, {ok, row_to_module_record(Row)}, State};

        {ok, _Columns, []} ->
            logger:send_log(?SERVER, "NEW Module has been created and added into the db."),
            InsertQuery = "INSERT INTO modules (chip_id, hmac) VALUES ($1, $2) " ++
            "RETURNING module_id, chip_id, user_id, hmac, location",

            case epgsql:squery(Connection, InsertQuery, [Chip_id, Hmac]) of
                {ok, _Columns, [NewRow]} ->
                    NewModuleRecord = row_to_module_record(NewRow),
                    module_cache:load_module(NewModuleRecord),
                    {reply, {ok, NewModuleRecord}, State};
                {error, Reason} ->
                    logger:send_log(?SERVER, Reason),
                    {reply, {err, Reason}, State}
            end;
        {error, Reason} ->
            logger:send_log(?SERVER, Reason),
            {reply, {err, Reason}, State}
    end;


handle_call({retrieve_latest_transmission_by_module}, _From, State = #database_handler_state{connection = Connection}) ->

    Query = "", %%% FIX
    case epgsql:squery(Connection, Query, [Module_id]) of
        {ok,_Columns, []} ->
            {reply, {err, "ERROR (retrieve_latest_transmission_by_module), Either unable to get a transmisison, or the module_id does not exist. "}};
        {ok, _Columns, Row} ->
            {reply, {err, "No transmissions found in the last 7 days"}, State};
        {error, Reason} ->
            logger:send_log(?SERVER, "An error has occured when trying to retrieve_latest_transmission_by_module"),
            {reply, {err, Reason}, State}
    end;


handle_call({retrieve_all_modules_last_transmissions}, _From, State = #database_handler_state{connection = Connection}) ->

    Query = "SELECT t1.* FROM transmission AS t1 INNER JOIN (SELECT module_id, MAX(transmission_id) AS max_transmission_id FROM transmission WHERE time >= DATE('now', '-7 days') GROUP BY module_id ) AS t2 ON t1.module_id = t2.module_id AND t1.transmission_id = t2.max_transmission_id",

    case epgsql:squery(Connection, Query) of
        {ok, _Columns, []} ->
            logger:send_log(?SERVER, "An error occured when trying to retreive all module data"),
            {reply, {err, "An error occured when trying to retreive all module data"}, State};
        {ok, _Columns, [Rows]} ->
            {reply, {ok, row_to_transmission_record_helper(Rows)}, State};
        {error, Reason} ->
            logger:send_log(?SERVER, Reason),
            {reply, {err, Reason}, State}
    end;

handle_call(_Request, _From, State = #database_handler_state{}) ->
    {reply, ok, State}.



handle_cast({new_transmission, Payload}, State = #database_handler_state{connection = Connection}) ->

    {Module_id, Temperature, Moisture, Battery} = Payload,


    Query = "INSERT INTO transmission (module_id, time, temperature, moisture, battery) " ++
        "VALUES ($1, NOW(), $2, $3, $4) " ++
        "RETURNING transmission_id, module_id, time, temperature, moisture, battery",

    Params = [Module_id, Temperature, Moisture, Battery],

    case epgsql:squery(Connection, Query, Params) of
        {ok, _Columns, [Row]} ->
            NewRecord = row_to_transmission_record(Row),
            transmission_cache:new_transmission(NewRecord),
            {noreply, State};
        {error, Reason} ->
            logger:send_log(?SERVER, Reason),
            {noreply, State};
        _Other ->
            logger:send_log(?SERVER, "epgsql has failed with unknown issues"),
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

row_to_transmission_record_helper([Head | Tail]) ->
    [row_to_transmission_record(Head) | row_to_transmission_record_helper(Tail)];
row_to_transmission_record_helper([]) ->
    [].



row_to_transmission_record([Transmission_id, Module_id, Time, Temperature, Moisture, Battery]) ->
    Record = #transmission_record{
        transmission_id = Transmission_id,
        module_id = Module_id,
        time = Time,
        temperature = Temperature,
        moisture = Moisture,
        battery = Battery
    },
    Record.

row_to_module_record_helper([Head | Tail]) ->
    [row_to_module_record(Head) | row_to_module_record_helper(Tail)];
row_to_module_record_helper([]) ->
    [].

row_to_module_record([Module_id, Chip_id, Hmac, Location]) ->
    Module_record = #module{
        module_id = Module_id,
        chip_id = Chip_id,
        hmac = Hmac,
        location = Location
    },
    Module_record.