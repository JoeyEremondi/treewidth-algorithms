
open Set
open Graph

module Dtree =
	struct
		module VarSet = Set.Make(
			struct
				type t=int
		    let compare = Pervasives.compare
			end)
			
	  type dtree = 
			  Leaf of VarSet.elt * VarSet.elt
      | Node of dtree * dtree

		let varSet_init l =
			List.fold_right VarSet.add l VarSet.empty
			
		(**
		 * fun dtree -> varSet
		 * variables of a dtree
		 *)
		let rec vars = function
			| Leaf(a,b) -> varSet_init [a;b]
      | Node(l,r) -> VarSet.union (vars l) ( vars r )

		(**
		 * fun graph -> dtree -> varSet
		 * { u | \exists (u,v) \in E s.t. v \notin vars G(t) }
	 	 * TODO faster version with a while loop
	   *)
		let context g t =
			let vars_of_t = vars t in
			let is_there_an_edge_outside g u =
				let is_outside v = not (VarSet.mem v vars_of_t) in
				let edges_outsides = List.filter is_outside (Graph.neighborhood g u) in
				not ( edges_outsides = [] )
				in
			VarSet.filter (is_there_an_edge_outside g) vars_of_t
			
		(**
		 * fun varSet -> dtree -> varSet
		 * vertices common to G(tl) and G(tr) that are not in the context of t
		 *)
		let cutset ctxt = function
			| Leaf(a,b) -> VarSet.empty
			| Node(l,r) ->
				let intersection = VarSet.inter (vars l) (vars r) in
				VarSet.diff intersection ctxt		
		
		(**
		 * fun graph -> dtree -> int
		 * size of largest cluster -1
		 *)
		let size g t =
			let rec largest_cluster = function
				| Leaf(a,b) as leaf -> VarSet.cardinal (vars leaf)
				| Node(l,r) as t ->
					let ctxt = context g t in
					let cutset = cutset ctxt t in
					let cluster_sz = VarSet.cardinal ( VarSet.union ctxt cutset ) in
					max cluster_sz (max (largest_cluster l) (largest_cluster r))
				in
			(largest_cluster t) - 1 
				
   end;;
