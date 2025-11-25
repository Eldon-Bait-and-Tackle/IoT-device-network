%%%-------------------------------------------------------------------
%%% @author eldon
%%% @copyright (C) 2025, <Eldon Bait and Tackle>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(user_handler).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3,
    verify_user_by_auth/1]).

-define(SERVER, ?MODULE).

-record(user_handler_state, {}).

%%%===================================================================
%%% API functions
%%%===================================================================


%%% returns {ok, _}, and {error, Reason} as conditions

verify_user_by_auth(Auth) ->
    gen_server:call({user_token_auth, Auth}).







%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link(?SERVER, {local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #user_handler_state{}}.


%%% Dont know how to impliment the logic for AWS user verification, will need to add this later - eldon
handle_call({user_token_auth, _Auth} , _From, State = #user_handler_state{}) ->

    case ok of
        ok ->
            {reply, {ok, null}, State};
        error ->
            hsn_logger:send_log(?SERVER, "user handler has failed to verify or confirm a user"),
            {reply, error, State}
        end
;
handle_call(_Request, _From, State = #user_handler_state{}) ->
    {reply, ok, State}.

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

