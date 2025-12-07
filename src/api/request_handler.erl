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

    %% Standard CORS headers required by browsers
    Headers = #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
        <<"access-control-allow-headers">> => <<"content-type">>
    },

    case Method of
        <<"OPTIONS">> ->
            Req2 = cowboy_req:reply(200, Headers, <<>>, Req),
            {ok, Req2, State};

        <<"GET">> ->
            Params = maps:from_list(cowboy_req:parse_qs(Req)),
            handle_get(maps:get(<<"request">>, Params, undefined), Params, Req, Headers, State);
        <<"POST">> ->
            Params = maps:from_list(cowboy_req:parse_qs(Req)),
            RequestType = maps:get(<<"request">>, Params, undefined),

            {ok, BodyRaw, Req2} = cowboy_req:read_body(Req),
            Body = decode_payload(BodyRaw),

            handle_post(RequestType, Body, Req2, Headers, State);


        _ ->
            Req2 = cowboy_req:reply(405, Headers, <<"Method Not Allowed">>, Req),
            {ok, Req2, State}
    end.


%%%===================================================================
%%% Get
%%%===================================================================

handle_get(<<"get_heuristics">>, _Params, Req, Headers, State) ->
    Results = heuristics_cache:get_all_results(),
    Json = jiffy:encode(#{<<"results">> => Results}),
    Req2 = cowboy_req:reply(200, Headers, Json, Req),
    {ok, Req2, State};

handle_get(<<"get_map">>, _Params, Req, Headers, State) ->
    Results = map_cache:get_map(),
    Json = jiffy:encode(#{<<"results">> => Results}),
    Req2 = cowboy_req:reply(200, Headers, Json, Req),
    {ok, Req2, State};

handle_get(<<"get_module">>, Params, Req, Headers, State) ->
    RawId = maps:get(<<"module_id">>, Params, <<"0">>),
    ModuleId = try binary_to_integer(RawId) catch _:_ -> -1 end,

    case heuristics_cache:get_results_by_module(ModuleId) of
        {ok, ResultMap} ->
            Json = jiffy:encode(#{<<"results">> => ResultMap}),
            Req2 = cowboy_req:reply(200, Headers, Json, Req),
            {ok, Req2, State};
        {error, _} ->
            Req2 = cowboy_req:reply(404, Headers, <<"Module not found">>, Req),
            {ok, Req2, State}
    end;

handle_get(_, _Params, Req, Headers, State) ->
    Req2 = cowboy_req:reply(400, Headers, <<"Invalid Request">>, Req),
    {ok, Req2, State}.


%%%===================================================================
%%% Post
%%%===================================================================

handle_post(<<"claim_device">>, Body, Req, Headers, State) ->
    Secret = maps:get(<<"secret">>, Body, undefined),
    
    case Secret of
        undefined ->
            Req2 = cowboy_req:reply(400, Headers, <<"Failed">>, Req),
            {ok, Req2, State};
        _ ->
            UserId = <<"user_from_token">>,

            case database_handler:claim_module(UserId, Secret) of
                {ok, ModId} ->
                    Req2 = cowboy_req:reply(200, Headers, jiffy:encode(#{<<"module_id">> => ModId}), Req),
                    {ok, Req2, State};
                {error, _} ->
                    Req2 = cowboy_req:reply(400, Headers, <<"Claim Failed">>, Req),
                    {ok, Req2, State}
            end
    end.



%%%===================================================================
%%% Internal functions
%%%===================================================================


decode_payload(Body) ->
    try jiffy:decode(Body, [return_maps]) catch _:_ -> #{} end.