
-record(
    transmission, {
    
        % headers
        hmac,
        module_id,
        chip_id,
        
        
        %% payload
        temperature,
        moisture,
        battery
    
}).

-record(
    transmission_record, {
    
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
        user_id,
        hmac,
        location,
        challenge
        
        
}).

-record(
	node, {
		
		id,
		number_connections,
		neighbors,
		long,
		lat
		
	}
).

-record(
    user, {
        user_id,
        user_auth,
        modules
}).
