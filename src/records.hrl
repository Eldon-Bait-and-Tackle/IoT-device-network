
-record(
    transmission, {
    
        temperature,
        moisture,
        battery,
        time
    
}).

-record(    
    module_data, {
        
        module_id,
        chip_id,
        module_mac,
        user_id
        
}).

-record(
    node, {

        neighbors,
        data %% module data... ^
    
}).

-record(
    transmission_cache_gen_state, {
        
        transmissions = #{}
        %%% Key = Module_id
        %%% Value = {module_data, [transmissions]
        
}).
