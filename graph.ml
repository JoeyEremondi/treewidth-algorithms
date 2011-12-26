
module Graph =
	struct
		type graph = {n:int; m:int; e:int list array}
		let empty = {n=0; m=0; e=[||]}
		
		let neighborhood g u =
			g.e.(u)
	end;;
