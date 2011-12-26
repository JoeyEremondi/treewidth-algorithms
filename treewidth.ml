
open Dtree
open Hypergraph

module Treewidth =
	struct
    (**
     * fun hyperGraph -> dtree
     * build a dtree following the recursive partitions of the hypergraph
     *)
    let rec hgr2bdt h =
	    match Hypergraph.get_sole_vertex h with
          (* h has only one vertex *)
        | Some(a,b) -> Dtree.Leaf(a,b)
        | None ->
	        let hl, hr = Hypergraph.partition h in
	        Dtree.Node(hgr2bdt hl, hgr2bdt hr)

    (**
     * fun graph -> int
     * treewidth approximation algorithm
     *)
    let treewidth g =
	    let h = Hypergraph.from_graph g in
	    let dt = hgr2bdt h in
      Dtree.size g dt
	end;;
    