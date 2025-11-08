-module(transmission_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-include("records.hrl").

init(Req, State) ->
    Method = cowboy_req:method(Req),
    {ok, Body, Req2} = cowboy_req:body(Req),

    case {Method, decode_transmission_payload(Body)} of
        {<<"POST">>, #{<<"module_id">> := Mid, <<"hmac">> := Hmac, <<"temperature">> := Temp, <<"moisture">> := Moisture, <<"battery">> := Bat, <<"chip_id">> := Cid} = Data} ->
            Payload = {Mid, Temp, Moisture, Bat},
            handle_transmission(Mid, Hmac, Cid, Payload, Req2, State);
        _ ->
            logger:send_log(?MODULE, "Invalid Transmission Request"),
            {ok, cowboy_req:reply(400, Req2, <<"Invalid Transmission Request">>, State)}
    end.

decode_transmission_payload(Body) ->
    try
        UrlDecoded = uri_string:dissect_query(Body),
        lists:foldl(fun({Key, Value}, Acc) ->
            maps:put(list_to_binary(Key), list_to_binary(Value), Acc)
                    end, #{}, UrlDecoded)
    catch _:_ -> #{} end.

module_validation(Module_id, Hmac, Cid) ->
    module_cache:verify_module(Hmac, Module_id, Cid).

handle_transmission(Mid, Hmac, Cid, Payload, Req, State) ->
    case module_validation(Mid, Hmac, Cid) of
        {ok, true} ->
            database_handler:new_transmission(Payload),
            {ok, cowboy_req:reply(200, Req, <<"OK">>, State)};
        _ ->
            {ok, cowboy_req:reply(403, Req, <<"Forbidden">>, State)}
        end.
    

handle_transmission(Module_id, Hmac, Chip_id, Payload, Req, State) ->
    case module_validation(Module_id, Hmac, Chip_id) of
        {ok, true} ->
            
            %%% we have to add to the database first before we add to the cahce beccause of the unique number system. 
            %%% this is to say that this funciton will add the new payload into cache after this cast is called.
            database_handler:new_transmission(Payload),
            
            {ok, cowboy_req:reply(200, Req, <<"OK">>, State)};
        {error, invalid_hmac} ->
            {ok, cowboy_req:reply(401, Req, <<"Unauthorized: Invalid HMAC">>, State)};
        _ ->
            {ok, cowboy_req:reply(403, Req, <<"Forbidden">>, State)}
    end.