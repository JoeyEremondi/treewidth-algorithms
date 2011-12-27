all:
	ocamlc unix.cma str.cma graph.ml hypergraph.ml dtree.ml treewidth.ml main.ml -o treewidth

