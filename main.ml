open Dtree
open Graph
open Treewidth
open Hypergraph

let g = {
  Graph.n = 5;
  m = 5;
  e = [|
    [1;2];
    [0;2;3];
    [0;1;3;4];
    [1;2];
    [2]
    |] }

let dt =
  Dtree.Node(
    Dtree.Node(
      Dtree.Leaf(0,1),
      Dtree.Leaf(0,2) ),
    Dtree.Node(
      Dtree.Leaf(2,4),
      Dtree.Node(
        Dtree.Leaf(1,2),
        Dtree.Node(
          Dtree.Leaf(2,3),
          Dtree.Leaf(1,3) ) ) ) )

let ctxt = Dtree.context g dt
let cs = Dtree.cutset ctxt dt
let dt_s = Dtree.size g dt
let () = print_int dt_s; print_newline ()

let tw = Treewidth.treewidth g
let () = print_int tw; print_newline ()

