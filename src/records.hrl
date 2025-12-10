
-record(
    transmission, {
    
        transmission_id,
        module_id,
    
        %% payload
        time,
        temperature,
        moisture,
        battery

}).


-record(    
    module, {
        module_id,
        secret_key,
        location, %%%% {lat, long}
        owner_id,
        is_claimed,


        challenge %% Not in database, temporary value
}).

-record(
	node, {
		id,
        location,
		neighbors %%% [Module Ids]
	}
).

-record(
    user, {
        user_id
}).
