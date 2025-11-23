
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
        chip_id,
        hmac,
        location, %%%% {lat, long}
        challenge %% Not in database, temporary value
}).

-record(
	node, {
		
		id,
        location,
		number_connections,
		neighbors %%% [Module Ids]
	}
).

-record(
    user, {
        user_id,
        modules %%% [Module Ids]
}).
