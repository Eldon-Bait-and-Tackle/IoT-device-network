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

-include("records.hrl").

init(Req, State) ->
    Method = cowboy_req:method(Req),

    %% Standard CORS headers required by browsers
    Headers = #{
        <<"access-control-allow-headers">> => <<"content-type, authorization">>,
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
        <<"access-control-allow-private-network">> => <<"true">>
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

handle_get(<<"get_modules">>, _Params, Req, Headers, State) ->
    %% Public endpoint - no auth required
    Results = module_cache:get_all_results(),
    Json = jiffy:encode(#{<<"results">> => Results}),
    Req2 = cowboy_req:reply(200, Headers, Json, Req),
    {ok, Req2, State};


handle_get(<<"get_heuristics">>, _Params, Req, Headers, State) ->
    %% Public endpoint - no auth required
    Results = heuristics_cache:get_all_results(),
    Json = jiffy:encode(#{<<"results">> => Results}),
    Req2 = cowboy_req:reply(200, Headers, Json, Req),
    {ok, Req2, State};

handle_get(<<"get_map">>, _Params, Req, Headers, State) ->
    %% Public endpoint - no auth required
    Results = map_cache:get_map(),
    Json = jiffy:encode(#{<<"results">> => Results}),
    Req2 = cowboy_req:reply(200, Headers, Json, Req),
    {ok, Req2, State};

handle_get(<<"get_module">>, Params, Req, Headers, State) ->
    %% Protected endpoint - requires valid auth token
    AuthToken = get_auth_token(Req, Params),
    
    case validate_auth_token(AuthToken) of
        {ok, _UserInfo} ->
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
        {error, _Reason} ->
            ErrorJson = jiffy:encode(#{<<"error">> => <<"Unauthorized">>, <<"message">> => <<"Invalid or missing auth token">>}),
            Req2 = cowboy_req:reply(401, Headers, ErrorJson, Req),
            {ok, Req2, State}
    end;

handle_get(<<"get_owned_modules">>, Params, Req, Headers, State) ->
    Auth = get_auth_token(Req, Params),
    hsn_logger:send_log(?MODULE, "User attempting to get_owned_modules $1", [Auth]),

    case validate_auth_token(Auth) of
        {ok, UserInfo} ->
            UserId = maps:get(<<"sub">>, UserInfo, undefined),

            case module_cache:get_all_users_modules(UserId) of
                {ok, Modules} ->
                    ModuleIds = [Id || #module{module_id = Id} <- Modules],

                    {ok, RawHeuristics} = heuristics_cache:get_results_by_list(ModuleIds),
                    Heuristics = lists:map(fun format_heuristic/1, RawHeuristics),

                    ModuleMaps = [module_to_map(M) || M <- Modules],

                    Response = #{
                        <<"modules">> => ModuleMaps,
                        <<"heuristics">> => Heuristics
                    },

                    Json = jiffy:encode(Response),
                    Req2 = cowboy_req:reply(200, Headers, Json, Req),
                    {ok, Req2, State};
                {error, Reason} ->
                    ErrorJson = jiffy:encode(#{
                        <<"error">> => <<"Failed to retrieve modules">>,
                        <<"message">> => atom_to_binary(Reason, utf8)
                    }),
                    Req2 = cowboy_req:reply(500, Headers, ErrorJson, Req),
                    {ok, Req2, State}
            end;
        {error, _Reason} ->
            ErrorJson = jiffy:encode(#{
                <<"error">> => <<"Unauthorized">>,
                <<"message">> => <<"Invalid or missing auth token">>
            }),
            Req2 = cowboy_req:reply(401, Headers, ErrorJson, Req),
            {ok, Req2, State}
    end;

            
handle_get(_, _Params, Req, Headers, State) ->
    Req2 = cowboy_req:reply(400, Headers, <<"Invalid Request">>, Req),
    {ok, Req2, State}.


%%%===================================================================
%%% Post
%%%===================================================================

handle_post(<<"update_location">>, Body, Req, Headers, State) ->
    AuthToken = get_auth_token(Req, Body),
    Module_id = maps:get(<<"module_id">>, Body, undefined),
    
    case Module_id of
        undefined ->
            ErrorJson = jiffy:encode(#{<<"error">> => <<"Missing module_id parameter">>}),
            Req2 = cowboy_req:reply(400, Headers, ErrorJson, Req),
            {ok, Req2, State};
        _ ->
            case validate_auth_token(AuthToken) of
                {ok, UserInfo} ->
                    UserId = maps:get(<<"sub">>, UserInfo, undefined),
                    case module_cache:verify_user_ownership(UserId, Module_id) of
                        {ok, true} ->
                            Lat = maps:get(<<"latitude">>, Body, undefined),
                            Long = maps:get(<<"longitude">>, Body, undefined),
                            case database_handler:update_module_location(Module_id, Lat, Long) of
                                {ok, _} ->
                                    SuccessJson = jiffy:encode(#{
                                        <<"module_id">> => Module_id,
                                        <<"status">> => <<"updated">>
                                    }),
                                    Req3 = cowboy_req:reply(200, Headers, SuccessJson, Req),
                                    {ok, Req3, State};
                                {error, Reason2} ->
                                    ErrorJson2 = jiffy:encode(#{<<"error">> => Reason2}),
                                    Req3 = cowboy_req:reply(400, Headers, ErrorJson2, Req),
                                    {ok, Req3, State};
                                _ ->
                                    ErrorJson2 = jiffy:encode(#{<<"error">> => <<"No Idea sport, good luck though :)">>}),
                                    Req3 = cowboy_req:reply(401, Headers, ErrorJson2, Req),
                                    {ok, Req3, State}
                            end;
                        _ ->
                            ErrorJson2 = jiffy:encode(#{<<"error">> => <<"An error of some kind has occured when attempting to validate ownership">>}),
                            Req3 = cowboy_req:reply(400, Headers, ErrorJson2, Req),
                            {ok, Req3, State}
                    end;
                {error, missing_token} ->
                    ErrorJson2 = jiffy:encode(#{<<"error">> => <<"Missing User Auth Token">>}),
                    Req2 = cowboy_req:reply(400, Headers, ErrorJson2, Req),
                    {ok, Req2, State}
            end
        end;

handle_post(<<"claim_device">>, Body, Req, Headers, State) ->
    AuthToken = get_auth_token(Req, Body),
    Secret = maps:get(<<"secret">>, Body, undefined),

    case Secret of
        undefined ->
            ErrorJson = jiffy:encode(#{<<"error">> => <<"Missing secret parameter">>}),
            Req2 = cowboy_req:reply(400, Headers, ErrorJson, Req),
            {ok, Req2, State};
        _ ->
            case validate_auth_token(AuthToken) of
                {ok, UserInfo} ->
                    UserId = maps:get(<<"sub">>, UserInfo, undefined),
                    case database_handler:claim_module(UserId, Secret) of
                        {ok, ModId} ->
                            SuccessJson = jiffy:encode(#{
                                <<"module_id">> => ModId,
                                <<"status">> => <<"claimed">>
                            }),
                            Req2 = cowboy_req:reply(200, Headers, SuccessJson, Req),
                            {ok, Req2, State};
                        {error, Reason} ->
                            ErrorJson = jiffy:encode(#{
                                <<"error">> => <<"Claim failed">>,
                                <<"message">> => atom_to_binary(Reason, utf8)
                            }),
                            Req2 = cowboy_req:reply(400, Headers, ErrorJson, Req),
                            {ok, Req2, State}
                    end;
                {error, _Reason} ->
                    ErrorJson = jiffy:encode(#{
                        <<"error">> => <<"Unauthorized">>,
                        <<"message">> => <<"Invalid or missing auth token">>
                    }),
                    Req2 = cowboy_req:reply(401, Headers, ErrorJson, Req),
                    {ok, Req2, State}
            end
    end;

handle_post(<<"exchange_token">>, Body, Req, Headers, State) ->
    Code = maps:get(<<"code">>, Body, undefined),
    RedirectUri = maps:get(<<"redirect_uri">>, Body, undefined),

    case {Code, RedirectUri} of
        {undefined, _} ->
            ErrorJson = jiffy:encode(#{<<"error">> => <<"Missing code parameter">>}),
            Req2 = cowboy_req:reply(400, Headers, ErrorJson, Req),
            {ok, Req2, State};
        {_, undefined} ->
            ErrorJson = jiffy:encode(#{<<"error">> => <<"Missing redirect_uri parameter">>}),
            Req2 = cowboy_req:reply(400, Headers, ErrorJson, Req),
            {ok, Req2, State};
        _ ->
            %% Exchange code for token with Keycloak
            TokenUrl = <<"https://auth.eldonbaitandtackle.net/realms/hsn_kc/protocol/openid-connect/token">>,
            ClientId = <<"public_client">>,

            Body2 = uri_string:compose_query([
                {<<"grant_type">>, <<"authorization_code">>},
                {<<"client_id">>, ClientId},
                {<<"code">>, Code},
                {<<"redirect_uri">>, RedirectUri}
            ]),

            case httpc:request(post, {binary_to_list(TokenUrl), [], "application/x-www-form-urlencoded", Body2}, [], [{body_format, binary}]) of
                {ok, {{_, 200, _}, _, ResponseBody}} ->
                    %% Forward the token response to the client
                    Req2 = cowboy_req:reply(200, Headers, ResponseBody, Req),
                    {ok, Req2, State};
                {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
                    hsn_logger:send_log(?MODULE, "Token exchange failed: ~p", [StatusCode]),
                    Req2 = cowboy_req:reply(StatusCode, Headers, ResponseBody, Req),
                    {ok, Req2, State};
                {error, Reason} ->
                    hsn_logger:send_log(?MODULE, "Token exchange error: ~p", [Reason]),
                    ErrorJson = jiffy:encode(#{
                        <<"error">> => <<"Token exchange failed">>,
                        <<"message">> => atom_to_binary(Reason, utf8)
                    }),
                    Req2 = cowboy_req:reply(500, Headers, ErrorJson, Req),
                    {ok, Req2, State}
            end
    end;

handle_post(_, _, Req, Headers, State) ->
    ErrorJson = jiffy:encode(#{<<"error">> => <<"Invalid request">>}),
    Req2 = cowboy_req:reply(400, Headers, ErrorJson, Req),
    {ok, Req2, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================


get_auth_token(Req, Params) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            maps:get(<<"auth_token">>, Params, undefined);
        AuthHeader ->
            case binary:split(AuthHeader, <<" ">>) of
                [_, Token] -> Token;
                [Token] -> Token
            end
    end.

decode_payload(Body) ->
    try jiffy:decode(Body, [return_maps]) catch _:_ -> #{} end.

validate_auth_token(undefined) ->
    {error, missing_token};
validate_auth_token(Token) ->
    user_handler:verify_user_by_auth(Token).


format_heuristic({Module_id, Self_temp, Avg_temp, Within_range, Deviation}) ->
    #{
        <<"module_id">> => Module_id,
        <<"self_temp">> => Self_temp,
        <<"avg_neighbor_temp">> => Avg_temp,
        <<"within_range">> => Within_range,
        <<"deviation">> => Deviation
    };
format_heuristic({Module_id, Self_temp, no_neighbors, Within_range}) ->
    #{
        <<"module_id">> => Module_id,
        <<"self_temp">> => Self_temp,
        <<"avg_neighbor_temp">> => <<"no_neighbors">>,
        <<"within_range">> => Within_range,
        <<"deviation">> => 0.0
    }.

module_to_map(#module{module_id = Id, location = Loc}) ->
    #{
        <<"module_id">> => Id,
        <<"location">> => case Loc of {Lat, Long} -> #{<<"lat">> => Lat, <<"long">> => Long}; _ -> null end
    }.