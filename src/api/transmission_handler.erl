-module(transmission_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-include("records.hrl").

init(Req, State) ->
    Method = cowboy_req:method(Req),
    {ok, Body, Req2} = cowboy_req:read_body(Req),

    case {Method, decode_transmission_payload(Body)} of
        {<<"POST">>, #{<<"signature">> := Sig, <<"module_id">> := Mid, <<"temperature">> := Temp, <<"moisture">> := Moist, <<"battery">> := Bat}} ->

            VerificationData = <<Mid/binary, Temp/binary, Moist/binary, Bat/binary>>,

            case module_cache:verify_transmission(Mid, VerificationData, Sig) of
                true ->
                    database_handler:new_transmission({Mid, Temp, Moist, Bat}),
                    Req3 = cowboy_req:reply(200, #{}, <<"OK">>, Req2),
                    {ok, Req3, State};
                false ->
                    Req3 = cowboy_req:reply(403, #{}, <<"Invalid Signature">>, Req2),
                    {ok, Req3, State}
            end;
        _ ->
            Req3 = cowboy_req:reply(400, #{}, <<"Bad Request">>, Req2),
            {ok, Req3, State}
    end.

decode_transmission_payload(Body) ->
    try
        List = uri_string:dissect_query(Body),
        maps:from_list(List)
    catch _:_ -> #{} end.