%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2025 22:15
%%%-------------------------------------------------------------------
-module(request_handler).
-author("Eldon").

%% API
-export([]).
-behaviour(cowboy_handler).
-define(SERVER, ?MODULE).

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


%%%===================================================================
%%% Internal functions
%%%===================================================================


request_type(get_general) ->
    

