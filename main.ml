open Dtree
open Graph
open Treewidth
open Hypergraph
open Printf

let g = Graph.read_dgf Sys.argv.(1)
let nbTrials = try
	int_of_string Sys.argv.(2)
with _ -> 5

let tw = Treewidth.treewidth g nbTrials
let () = printf "\napproximated treewidth : %d\n" tw

