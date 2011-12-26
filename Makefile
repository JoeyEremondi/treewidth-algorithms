all:
	ocamlc unix.cma graph.ml hypergraph.ml dtree.ml treewidth.ml main.ml -o treewidth

