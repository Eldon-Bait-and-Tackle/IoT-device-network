%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Oct 2025 00:14
%%%-------------------------------------------------------------------
-module(vmq_auth_plugin).
-behaviour(vernemq_plugin).
-behaviour(vernemq_auth_hook).

-export([start/2, stop/1]).
-export([auth_on_register/6]).

start(_Type, _Args) ->
    vmq_plugin:register_hook(auth_on_register, fun ?MODULE:auth_on_register/6, []).

stop(_State) ->
    ok.

auth_on_register(_ClientId, AuthToken, _Password, _CleanSession, _MaxQos, _Mountpoint) ->

    case module_cache:verify_auth_token(AuthToken) of
        {ok, true} ->
            {ok, {vmq_acl:all_access, vmq_acl:all_access}};
        _ ->
            {error, bad_username_or_password}
    end.
