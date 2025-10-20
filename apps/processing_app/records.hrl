
-record(
	neighbor, {
		
		id,
		distance
		
	}
	
).

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
	pre_node, {
		
		id, 
		long,
		lat
		
}).

-record(
	network, {
		
		nodes
		
	}
).





