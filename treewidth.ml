
open Dtree
open Hypergraph
open Printf

module Treewidth =
  struct

    (**
     * fun hyperGraph -> int -> dtree
     * build a dtree following the recursive partitions of the hypergraph
     *)
    let rec hgr2bdt h ubFactor =
      match Hypergraph.get_sole_vertex h with
          (* h has only one vertex *)
        | Some(a,b) -> Dtree.Leaf(a,b)
        | None ->
          let hl, hr = Hypergraph.partition h ubFactor in
          Dtree.Node(hgr2bdt hl ubFactor, hgr2bdt hr ubFactor)

    (**
     * fun graph -> int -> int
     * treewidth approximation algorithm
     *)
    let treewidth g nbTrials =
      let h = Hypergraph.from_graph g in
      let min_sz = ref 0 in
        
      for ubPower=0 to 4 do
        let ubFactor = int_of_float(2. ** (float_of_int ubPower)) in
        printf "ub:%d\n" ubFactor; flush stdout;
        
        for trial=1 to nbTrials do
          let dt = hgr2bdt h ubFactor in
            
          let sz = Dtree.size g dt in
          printf "%d\r" sz; flush stdout;
          min_sz := if !min_sz = 0 then sz else min !min_sz sz
        done
      done;
      
      !min_sz  
  end;;
    
