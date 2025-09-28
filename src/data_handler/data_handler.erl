%%%-------------------------------------------------------------------
%%% @author Eldon
%%% @copyright (C) 2025, <Eldon Bait and Tackle>
%%% @doc
%%%
%%% @end
%%% Created : 26. Sept 2025 13:45
%%%-------------------------------------------------------------------
-module(data_handler).
-author("Eldon").

-include("records.hrl").
-behavior(gen_server).


%% API
-export([]).

init([]) ->
    State = #{},
    {ok, State}.

terminate() ->
    ok
    .
    
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
    
    
add_transmission(Rec) ->
    gen_server:cast(?MODULE, {})
    
    
    
    .
    

