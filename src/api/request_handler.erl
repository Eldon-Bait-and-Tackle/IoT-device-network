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
-export([init/2]).
-behaviour(cowboy_handler).
-define(SERVER, ?MODULE).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    {ok, ReqBody, Req2} = cowboy_req:read_body(Req),

    case {Method, decode_payload(ReqBody)} of

        {<<"GET">>, #{<<"request">> := <<"get_heuristics">>}} ->
            Results = heuristics_cache:get_all_results(),
            Json = jiffy:encode(#{<<"results">> => Results}),
            Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req2),
            {ok, Req3, State};

        {<<"GET">>, #{<<"request">> := <<"get_map">>}} ->
            Results = map_cache:get_map(),
            Json = jiffy:encode(#{<<"results">> => Results}),
            Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req2),
            {ok, Req3, State};

        {<<"GET">>, #{<<"request">> := <<"get_module">>, <<"module_id">> := Module_id}} ->
            case heuristics_cache:get_results_by_module(Module_id) of
                {ok, ResultMap} ->
                    Json = jiffy:encode(#{<<"results">> => ResultMap}),
                    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req2),
                    {ok, Req3, State};
                {error, _} ->
                    hsn_logger:send_log(?SERVER, "Module has not been found for some request"),
                    Req3 = cowboy_req:reply(404, #{}, <<"Module not found">>, Req2),
                    {ok, Req3, State}
            end;

        _ ->
            Req3 = cowboy_req:reply(400, #{}, <<"Invalid Request">>, Req2),
            {ok, Req3, State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================


decode_payload(Body) ->
    try jiffy:decode(Body, [return_maps]) catch _:_ -> #{} end.