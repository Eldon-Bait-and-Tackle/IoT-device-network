%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <Eldon Bait and Tackle>
%%% @doc
%%%
%%% @end
%%% Created : 05. Nov 2025 23:15
%%%-------------------------------------------------------------------
-module(hsn_api_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [test_handshake_challenge].

init_per_suite(Config) ->
    application:start(hsn_app),
    inets:start(),
    Config.

end_per_suite(Config) ->
    inets:stop(),
    application:stop(hsn_app),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

test_handshake_challenge(_Config) ->
    Url = "http://localhost:8082/handshake",

    ReqBody = jiffy:encode(#{
        <<"module_id">> => <<"TEST_MID_001">>,
        <<"chip_id">> => <<"TEST_CID_A1B2">>
    }),

    Options = [{body_format, binary}],

    {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(post, {Url, [], "application/json", ReqBody}, Options, []),

    ?assert(StatusCode == 200),

    {ok, Json} = jiffy:decode(Body, [return_maps]),

    ?assert(maps:is_key(<<"challenge">>, Json)),

    ok.