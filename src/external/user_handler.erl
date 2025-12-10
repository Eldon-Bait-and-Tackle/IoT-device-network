%%%-------------------------------------------------------------------
%%% @author eldon
%%% @copyright (C) 2025, <Eldon Bait and Tackle>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(user_handler).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([verify_user_by_auth/1, refresh_keys/0]).

-define(SERVER, ?MODULE).

-define(DEFAULT_KC_URL, <<"http://localhost:8080">>).
-define(DEFAULT_REALM, <<"master">>).
-define(DEFAULT_CLIENT_ID, <<"hsn_client_public">>).

-define(TABLE, ?MODULE).

-record(state, {
    public_keys = [],
    keycloak_internal_url,
    realm,
    client_id,
    issuer
}).

%%%===================================================================
%%% API functions
%%%===================================================================



start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

verify_user_by_auth(Token) when is_binary(Token) ->
    gen_server:call(?SERVER, {verify_token, Token}).

refresh_keys() ->
    gen_server:cast(?SERVER, refresh_keys).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================


init([]) ->

    PublicUrl = get_env(keycloak_url, ?DEFAULT_KC_URL),
    Realm = get_env(keycloak_realm, ?DEFAULT_REALM),
    ClientID = get_env(keycloak_client_id, ?DEFAULT_CLIENT_ID),

    InternalUrl = get_env(keycloak_internal_url, PublicUrl),

    Cleaned_Url = string:trim(PublicUrl, trailing, "/"),
    Issuer = <<Cleaned_Url/binary, "/realms/", Realm/binary>>,

    State = #state{
        keycloak_internal_url = InternalUrl,
        realm = Realm,
        client_id = ClientID,
        issuer = Issuer
    },

    NewState = fetch_and_update_keys(State),
    {ok, NewState}.

handle_call({verify_token, Token}, _From, State = #state{public_keys = Keys}) ->
    Result = validate_jwt(Token, Keys, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(refresh_keys, State) ->
    NewState = fetch_and_update_keys(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

validate_jwt(_Token, [], _State) ->
    io:format("[KEYCLOAK ERROR] No public keys loaded or all keys failed.~n"),
    {error, invalid_token};

validate_jwt(Token, [JWK | Rest], State) ->
    try jose_jwt:verify(JWK, Token) of
        {true, {jose_jwt, Claims}, _JWS} ->
            verify_claims(Claims, State);
        {false, _, _} ->
            validate_jwt(Token, Rest, State);
        {error, _} ->
            validate_jwt(Token, Rest, State)
    catch
        _:_ ->
            hsn_logger:send_log(?SERVER, "[KEYCLOAK REJECT] Malformed/Garbage Token detected.~n"),
            validate_jwt(Token, Rest, State)
    end.

verify_claims(Claims, #state{issuer = ExpectedIssuer, client_id = ExpectedAud}) ->
    Exp = maps:get(<<"exp">>, Claims, 0),
    Iss = maps:get(<<"iss">>, Claims, undefined),
    Aud = maps:get(<<"aud">>, Claims, undefined),
    Azp = maps:get(<<"azp">>, Claims, undefined),

    CurrentTime = os:system_time(seconds),

    if
        Exp < CurrentTime ->
            hsn_logger:send_log(?SERVER, "Token expired at ~p, current time is ~p", [Exp, CurrentTime]),
            {error, token_expired};
        Iss =/= ExpectedIssuer ->
            hsn_logger:send_log(?SERVER, "Issuer mismatch. Expected: ~p, Got: ~p", [ExpectedIssuer, Iss]),
            {error, invalid_issuer};
        true ->
            case check_audience(Aud, ExpectedAud) of
                true -> {ok, Claims};
                false -> 
                    case check_audience(Azp, ExpectedAud) of
                        true -> {ok, Claims};
                        false ->
                            hsn_logger:send_log(?SERVER, "Audience mismatch. Expected: ~p, Got: ~p / ~p", [ExpectedAud, Aud, Azp]),
                            {error, invalid_audience}
                    end
            end
    end.

check_audience(Aud, Expected) when is_binary(Aud) ->
    Aud =:= Expected;
check_audience(AudList, Expected) when is_list(AudList) ->
    lists:member(Expected, AudList);
check_audience(_, _) ->
    false.

fetch_and_update_keys(State = #state{keycloak_internal_url = Url, realm = Realm}) ->
    CertsUrl = binary_to_list(<<Url/binary, "/realms/", Realm/binary, "/protocol/openid-connect/certs">>),

    case httpc:request(get, {CertsUrl, []}, [{timeout, 5000}], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            try
                JSON = jiffy:decode(Body, [return_maps]),
                KeysJSON = maps:get(<<"keys">>, JSON, []),

                ParsedKeys = lists:map(fun(K) -> jose_jwk:from(K) end, KeysJSON),

                hsn_logger:send_log(?SERVER, "Successfully fetched ~p public keys", [length(ParsedKeys)]),
                State#state{public_keys = ParsedKeys}
            catch
                _:Error ->
                    hsn_logger:send_log(?SERVER, "Failed to parse keys: ~p", [Error]),
                    State
            end;
        Error ->
            hsn_logger:send_log(?SERVER, "Failed to fetch keys from Keycloak: ~p", [Error]),
            State
    end.

get_env(Key, Default) ->
    EnvName = string:to_upper(atom_to_list(Key)),
    case os:getenv(EnvName) of
        false ->
            application:get_env(hsn_app, Key, Default);
        Value ->
            list_to_binary(Value)
    end.