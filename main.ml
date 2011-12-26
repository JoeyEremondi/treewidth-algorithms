open Dtree
open Graph
open Treewidth

let g = {
	n = 5;
	e = [|
		[1;2];
		[0;2;3];
		[0;1;3;4];
		[1;2];
		[2]
		|] }

let dt =
  Node(
		Node(
			Leaf(0,1),
			Leaf(0,2) ),
		Node(
			Leaf(2,4),
			Node(
				Leaf(1,2),
				Node(
					Leaf(2,3),
					Leaf(1,3) ) ) ) )

let ctxt = Dtree.context g dt
let cs = Dtree.cutset ctxt dt
let dt_s = Dtree.size g dt
let () = print_int dt_s; print_newline ()

let tw = Treewidth.treewidth g
let () = print_int tw; print_newline ()

