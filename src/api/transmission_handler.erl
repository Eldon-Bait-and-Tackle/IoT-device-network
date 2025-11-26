-module(transmission_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-include("records.hrl").

init(Req, State) ->
    Method = cowboy_req:method(Req),
    {ok, Body, Req2} = cowboy_req:read_body(Req),

    case {Method, decode_transmission_payload(Body)} of
        {<<"POST">>, #{<<"module_id">> := Mid, <<"secret">> := Secret, <<"temperature">> := Temp, <<"moisture">> := Moisture, <<"battery">> := Bat} = _Data} ->
            Payload = {Mid, Temp, Moisture, Bat},
            handle_transmission(Mid, Secret, Payload, Req2, State);
        _ ->
            hsn_logger:send_log(?MODULE, "Invalid Transmission Request"),
            {ok, cowboy_req:reply(400, <<"Invalid Transmission Request">>, Req2, State)}
    end.

decode_transmission_payload(Body) ->
    try
        List = uri_string:dissect_query(Body),
        maps:from_list(List)
    catch _:_ -> #{} end.


module_validation(Module_id, Secret) ->
    module_cache:verify_response(Module_id, Secret).


handle_transmission(Module_id, Secret, Payload, Req, State) ->
    case module_validation(Module_id, Secret) of
        {ok, true} ->
            
            %%% we have to add to the database first before we add to the cahce beccause of the unique number system. 
            %%% this is to say that this funciton will add the new payload into cache after this cast is called.
            database_handler:new_transmission(Payload),
            

            %%% What does this state function actually do? we should not be returning this outside the api even if it not used?

            {ok, cowboy_req:reply(200, <<"OK">>, Req, State)};
        {error, invalid_secret} ->
            {ok, cowboy_req:reply(401, <<"Unauthorized: Invalid SECRET">>, Req,  State)};
        _ ->
            {ok, cowboy_req:reply(403, <<"Forbidden">>, Req,  State)}
    end.