%%%-------------------------------------------------------------------
%%% @author eldon
%%% @copyright (C) 2025, <Eldon Bait and Tackle>
%%% @doc
%%% Handles user authentication validation against Keycloak.
%%% Provides token verification and user information extraction.
%%% THIS MODULE IS SLOP GENERATED!!!!!!!!!!!!! i have never done this before and was not about to do it now :)
%%% @end
%%%-------------------------------------------------------------------
-module(user_handler).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3,
    verify_user_by_auth/1,
    validate_token/1,
    get_user_info/1]).

-define(SERVER, ?MODULE).
-define(TOKEN_CACHE_TTL, 3600000).
-define(KEYCLOAK_TIMEOUT, 5000).

-record(user_handler_state, {
    token_cache = #{}, %% {Token => {UserInfo, Timestamp}}
    keycloak_url = <<"http://localhost:8080/auth">>,
    realm = <<"master">>,
    client_id = undefined,
    client_secret = undefined
}).

%%%===================================================================
%%% API functions
%%%===================================================================



verify_user_by_auth(Auth) ->
    gen_server:call(?SERVER, {user_token_auth, Auth}, ?KEYCLOAK_TIMEOUT).

validate_token(Token) ->
    gen_server:call(?SERVER, {validate_token, Token}).

get_user_info(Token) ->
    gen_server:call(?SERVER, {get_user_info, Token}).







%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    State = #user_handler_state{
        keycloak_url = get_keycloak_url(),
        realm = get_keycloak_realm(),
        client_id = get_client_id(),
        client_secret = get_client_secret()
    },
    {ok, State}.

handle_call({user_token_auth, Token}, _From, State = #user_handler_state{}) ->
    case validate_token_with_keycloak(Token, State) of
        {ok, UserInfo} ->
            NewState = cache_token(Token, UserInfo, State),
            {reply, {ok, UserInfo}, NewState};
        {error, Reason} ->
            hsn_logger:send_log(?SERVER, "Token validation failed: ~p", [Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({validate_token, Token}, _From, State = #user_handler_state{}) ->
    case get_cached_token(Token, State) of
        {ok, UserInfo} ->
            {reply, {ok, UserInfo}, State};
        {error, _} ->
            case validate_token_with_keycloak(Token, State) of
                {ok, UserInfo} ->
                    NewState = cache_token(Token, UserInfo, State),
                    {reply, {ok, UserInfo}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({get_user_info, Token}, _From, State = #user_handler_state{}) ->
    case get_cached_token(Token, State) of
        {ok, UserInfo} ->
            {reply, {ok, UserInfo}, State};
        {error, _} ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State = #user_handler_state{}) ->
    {reply, {error, invalid_request}, State}.

handle_cast(_Request, State = #user_handler_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #user_handler_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #user_handler_state{}) ->
    ok.

code_change(_OldVsn, State = #user_handler_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

validate_token_with_keycloak(Token, #user_handler_state{
    keycloak_url = KeycloakUrl,
    realm = Realm,
    client_id = ClientId,
    client_secret = ClientSecret
}) ->
    IntrospectUrl = binary_to_list(<<KeycloakUrl/binary, "/realms/", Realm/binary, 
                                     "/protocol/openid-connect/token/introspect">>),
    
    %% Prepare the request body
    Body = uri_string:compose_query([
        {"token", binary_to_list(Token)},
        {"client_id", binary_to_list(ClientId)},
        {"client_secret", binary_to_list(ClientSecret)}
    ]),
    
    case httpc:request(post, {IntrospectUrl, [], "application/x-www-form-urlencoded", Body}, 
                       [{timeout, ?KEYCLOAK_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jiffy:decode(ResponseBody, [return_maps]) of
                Response when is_map(Response) ->
                    case maps:get(<<"active">>, Response, false) of
                        true ->
                            %% Extract user information from the token response
                            UserInfo = extract_user_info(Response),
                            {ok, UserInfo};
                        false ->
                            {error, inactive_token}
                    end;
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, _}} ->
            hsn_logger:send_log(?SERVER, "Keycloak returned status: ~p", [StatusCode]),
            {error, keycloak_error};
        {error, Reason} ->
            hsn_logger:send_log(?SERVER, "HTTP request failed: ~p", [Reason]),
            {error, connection_failed}
    end.

extract_user_info(TokenResponse) ->
    #{
        user_id => maps:get(<<"sub">>, TokenResponse, undefined),
        username => maps:get(<<"preferred_username">>, TokenResponse, undefined),
        email => maps:get(<<"email">>, TokenResponse, undefined),
        name => maps:get(<<"name">>, TokenResponse, undefined),
        active => maps:get(<<"active">>, TokenResponse, false),
        exp => maps:get(<<"exp">>, TokenResponse, undefined),
        iat => maps:get(<<"iat">>, TokenResponse, undefined),
        raw_response => TokenResponse
    }.

cache_token(Token, UserInfo, State = #user_handler_state{token_cache = Cache}) ->
    Timestamp = erlang:system_time(millisecond),
    NewCache = Cache#{Token => {UserInfo, Timestamp}},
    State#user_handler_state{token_cache = NewCache}.

get_cached_token(Token, #user_handler_state{token_cache = Cache}) ->
    case maps:find(Token, Cache) of
        {ok, {UserInfo, Timestamp}} ->
            CurrentTime = erlang:system_time(millisecond),
            case (CurrentTime - Timestamp) < ?TOKEN_CACHE_TTL of
                true ->
                    {ok, UserInfo};
                false ->
                    %% Cache expired, remove it
                    {error, cache_expired}
            end;
        error ->
            {error, not_in_cache}
    end.

get_keycloak_url() ->
    case application:get_env(hsn_app, keycloak_url) of
        {ok, Url} -> Url;
        undefined -> <<"http://localhost:8080/auth">>
    end.

get_keycloak_realm() ->
    case application:get_env(hsn_app, keycloak_realm) of
        {ok, Realm} -> Realm;
        undefined -> <<"master">>
    end.

get_client_id() ->
    case application:get_env(hsn_app, keycloak_client_id) of
        {ok, ClientId} -> ClientId;
        undefined -> undefined
    end.

get_client_secret() ->
    case application:get_env(hsn_app, keycloak_client_secret) of
        {ok, Secret} -> Secret;
        undefined -> undefined
    end.
