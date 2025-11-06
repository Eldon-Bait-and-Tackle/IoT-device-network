%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(transmission_handler).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-include("records.hrl").
-record(transmission_handler_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    {ok, Body, Req2} = cowboy_req:body(Req),

    case {Method, decode_transmission_payload(Body)} of
        {<<"POST">>, #{<<"module_id">> := Mid, <<"hmac">> := Hmac} = Data} ->
            handle_transmission(Mid, Hmac, Data, Req2, State);
        _ ->
            logger:send_log(?SERVER, "Invalid Transmission Request"),
            {ok, cowboy_req:reply(400, Req2, <<"Invalid Transmission Request">>, State)}
    end.

terminate(_Reason, _State = #transmission_handler_state{}) -> ok.

handle_call(_Request, _From, State = #transmission_handler_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #transmission_handler_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #transmission_handler_state{}) ->
    {noreply, State}.

code_change(_OldVsn, State = #transmission_handler_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

module_validation(Module_id, Chip_id, Response) ->
    module_cache:verify_response(Module_id, Chip_id, Response).

decode_transmission_payload(Body) ->
    try
        UrlDecoded = uri_string:dissect_query(Body),
        lists:foldl(fun({Key, Value}, Acc) ->
            maps:put(list_to_binary(Key), list_to_binary(Value), Acc)
                    end, #{}, UrlDecoded)
    catch _:_ -> #{} end.

handle_transmission(Module_id, Hmac, Data, Req, State) ->
    case module_validation(Module_id, Hmac, Data) of
        {ok, true} ->
            transmission_cache:new_transmission(),
            {ok, cowboy_req:reply(200, Req, <<"OK">>, State)};
        {error, invalid_hmac} ->
            {ok, cowboy_req:reply(401, Req, <<"Unauthorized: Invalid HMAC">>, State)};

        _ ->
            {ok, cowboy_req:reply(403, Req, <<"Forbidden">>, State)}
    end.

create_and_cache_record(ModuleId, Temp, Moist, Bat) ->
    Id = erlang:unique_integer([positive]),
    Time = erlang:system_time(second),

    Record = #transmission_record{
        transmission_id = Id,
        module_id = ModuleId,
        time = Time,
        temperature = Temp,
        moisture = Moist,
        battery = Bat
    }.
