open Dtree
open Graph


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

(* VarSet.elements ctxt *)
(* VarSet.elements cs *)