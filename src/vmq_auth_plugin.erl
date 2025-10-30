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

auth_on_register(_ClientId, Module_id, HMAC, _CleanSession, _MaxQos, _Mountpoint) ->

    ChipId = <<>>,

    case module_cache:verify_module(HMAC, ChipId, Module_id) of
        {ok, true} ->
            % Authentication successful: grant access to all topics
            {ok, {vmq_acl:all_access, vmq_acl:all_access}};
        _ ->
            % Authentication failed
            {error, bad_username_or_password}
    end.
