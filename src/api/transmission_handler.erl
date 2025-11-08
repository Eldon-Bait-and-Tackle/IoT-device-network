-module(transmission_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-include("records.hrl").

init(Req, State) ->
    Method = cowboy_req:method(Req),
    {ok, Body, Req2} = cowboy_req:body(Req),

    case {Method, decode_transmission_payload(Body)} of
        {<<"POST">>, #{<<"module_id">> := Mid, <<"hmac">> := Hmac} = Data} ->
            handle_transmission(Mid, Hmac, Data, Req2, State);
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

module_validation(Module_id, Hmac, Data) ->
    module_cache:verify_response(Module_id, Hmac, Data).

handle_transmission(Module_id, Hmac, Data, Req, State) ->
    case module_validation(Module_id, Hmac, Data) of
        {ok, true} ->
            %% Pass the data payload to the cache
            transmission_cache:new_transmission(Data),
            {ok, cowboy_req:reply(200, Req, <<"OK">>, State)};
        {error, invalid_hmac} ->
            {ok, cowboy_req:reply(401, Req, <<"Unauthorized: Invalid HMAC">>, State)};
        _ ->
            {ok, cowboy_req:reply(403, Req, <<"Forbidden">>, State)}
    end.