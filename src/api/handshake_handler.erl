%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(handshake_handler).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(handshake_handler_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    {ok, ReqBody, Req2} = cowboy_req:body(Req),

    case {Method, decode_payload(ReqBody)} of
        {<<"POST">>, #{<<"module_id">> := Mid, <<"chip_id">> := Cid, <<"response">> := Response}} ->
            handle_response(Mid, Cid, Response, Req2, State);

        {<<"POST">>, #{<<"module_id">> := Mid, <<"chip_id">> := Cid}} ->
            handle_challenge(Mid, Cid, Req2, State);

        _ ->
            {ok, cowboy_req:reply(400, Req2, <<"Invalid Request">>, State)}
    end.

terminate(_Reason, _Req, _State) -> ok.

handle_challenge(Module_id, Chip_id, Req, State) ->
    Challenge = crypto:rand_bytes(64),

    case module_cache:store_challenge(Challenge, Module_id, Chip_id) of
        {ok, _Challenge} ->
            Json = jiffy:encode(#{<<"challenge">> => Challenge}),
            {ok, cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req, Json), State};
        _ ->
            {ok, cowboy_req:reply(403, Req, <<"Forbidden">>, State)}
    end.

handle_response(Module_id, Chip_id, Response, Req, State) ->
    case module_cache:verify_response(Module_id, Chip_id, Response) of
        {ok, true} ->
            AuthToken = base64:encode(crypto:rand_bytes(32)),

            Json = jiffy:encode(#{<<"auth_token">> => AuthToken}),
            {ok, cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req, Json), State};
        _ ->
            {ok, cowboy_req:reply(403, Req, <<"Forbidden">>, State)}
    end.

decode_payload(Body) ->
    try jiffy:decode(Body, [return_maps]) catch _:_ -> #{} end.

handle_call(_Request, _From, State = #handshake_handler_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #handshake_handler_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #handshake_handler_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #handshake_handler_state{}) ->
    ok.

code_change(_OldVsn, State = #handshake_handler_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
