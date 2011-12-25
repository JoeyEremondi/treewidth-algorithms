module Dtree =
	struct
	  type dtree = 
			  Leaf of int * int
      | Node of dtree * dtree

		module VarSet = Set.Make( 
  		struct
 			  let compare = Pervasives.compare
    	  type t = int
		  end )
		let varSet_init l = 
			List.fold_right VarSet.add l VarSet.empty
			
		(**
		 * fun dtree -> varSet
		 * variables of a dtree
		 *)
		let rec vars = function
			| Leaf(a,b) -> varSet_init [a;b]
      | Node(l, r) -> VarSet.union (vars l) ( vars r )

		(**
		 * fun graph -> dtree -> varSet
		 * { u | \exists (u,v) \in E s.t. v \notin vars G(t) }
	   *)
		let context g t =
			let vars_of_t = vars t in
			let is_there_an_edge_outside g u =
				(* TODO faster version with a while loop *)
				let edges_outsides = VarSet.filter (VarSet.mem v vars_of_t) (Graph.edges g u) in
				VarSet.is_empty edges_outsides
				in
			VarSet.filter (is_there_an_edge_outside g) vars_of_t
			
		(**
		 * fun graph -> varSet -> varSet
		 * vertices common to G(tl) and G(tr) that are not in the context of t
		 *)
		let cutset g ctxt = function
			| Leaf(a,b) -> []
			| Node(l,r) ->
				let intersection = VarSet.inter (vars l) (vars r) in
				VarSet.inter intersection ctxt		
			 
   end;;
