
module Treewidth =
	struct
		let rec hgr2bdt h =
			if (HyperGraph.is_one_vertex h)
			then
				let a,b = HyperGraph.get_sole_vertex h in
				Leaf(a,b)
			else
				let hl, hr = HyperGraph.partition h in
				Node(hgr2bdt hl, hdr2bdt hr)
	
		let treewidth g =
			let h = HyperGraph.from_graph g in
			let dt = hgr2bdt h in
			Dtree.size dt
	end;;
	