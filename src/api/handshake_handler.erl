%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(handshake_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================


init(Req, State) ->
    Method = cowboy_req:method(Req),
    {ok, ReqBody, Req2} = cowboy_req:read_body(Req),

    case {Method, decode_payload(ReqBody)} of
        {<<"POST">>, #{<<"handshake">> := <<"response">>, <<"module_id">> := Mid, <<"response">> := Response}} ->
            handle_response(Mid, Response, Req2, State);
        
        {<<"POST">>, #{<<"handshake">> := <<"challenge">>, <<"module_id">> := Mid}} ->
            handle_challenge(Mid,Req2, State);
        
        {<<"POST">>, #{<<"handshake">> := <<"register">>, <<"chip_id">> := Cid, <<"hmac">> := Hmac}} ->
            handle_registration(Cid, Hmac, Req, State);

        _ ->
            Req3 = cowboy_req:reply(400, #{}, <<"Invalid Request Pattern">>, Req2),
            {ok, Req3, State}
    end.



decode_payload(Body) ->
    try jiffy:decode(Body, [return_maps]) catch _:_ -> #{} end.

%%%===================================================================
%%% Internal functions
%%%===================================================================


handle_registration(Chip_id, Hmac, Req, State) ->
    case database_handler:register_module(Chip_id, Hmac) of
        {ok, Module_id} ->
            Json = jiffy:encode(#{<<"module_id">> => Module_id, <<"status">> => <<"registered">>}),
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
            {ok, Req2, State};
        {error, _Reason} ->
            hsn_logger:send_log(?SERVER, "Module Registration has failed"),
            Req2 = cowboy_req:reply(500, #{}, <<"Registration Failed">>, Req),
            {ok, Req2, State}
    end.


handle_challenge(Module_id, Req, State) ->
    Challenge = crypto:strong_rand_bytes(64),

            %%% ADD UPDATES TO CHALLENGES?
    case module_cache:store_challenge(Challenge, Module_id) of
        {ok, _Challenge} ->
            EncodedChallenge = base64:encode(Challenge),
            Json = jiffy:encode(#{<<"challenge">> => EncodedChallenge}),
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
            {ok, Req2, State};
        _ ->
            Req2 = cowboy_req:reply(403, #{}, <<"Forbidden - Module Not Found">>, Req),
            {ok, Req2, State}
    end.

handle_response(Module_id, Response, Req, State) ->
    case module_cache:verify_response(Module_id, Response) of
        {ok, true} ->
            AuthToken = base64:encode(crypto:strong_rand_bytes(32)),
            Json = jiffy:encode(#{<<"auth_token">> => AuthToken}),
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
            {ok, Req2, State};
        _ ->
            Req2 = cowboy_req:reply(401, #{}, <<"Unauthorized">>, Req),
            {ok, Req2, State}
    end.