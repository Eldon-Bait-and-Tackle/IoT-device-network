
-record(
	neighbor, {
		
		id,
		distance
		
	}
	
).

-record(
	node, {
		
		id,
		neighbors,
		location
		
	}
).


-record(
	network, {
		
		nodes
		
	}
).





