module Graph =
	struct
		type graph = {mutable n: int; mutable e:int list array}
		let empty = {n=0; e=[||]}
		
		let neighborhood g u =
			g.e.(u)
	end;;
	