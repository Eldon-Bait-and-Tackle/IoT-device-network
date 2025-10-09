
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
    
        %% payload
        time,
        temperature,
        moisture,
        battery

}).


-record(    
    module_data, {
        
        module_id,
        chip_id,
        module_mac,
        user_id
        
}).
