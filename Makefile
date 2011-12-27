all:
	ocamlc unix.cma str.cma graph.ml hypergraph.ml dtree.ml treewidth.ml main.ml -o treewidth && pdflatex report.tex && bibtex report.aux && pdflatex report.tex && pdflatex report.tex
code:
	ocamlc unix.cma str.cma graph.ml hypergraph.ml dtree.ml treewidth.ml main.ml -o treewidth
report:
	pdflatex report.tex && bibtex report.aux && pdflatex report.tex && pdflatex report.tex