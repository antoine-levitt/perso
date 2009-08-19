all: figures rapport.pdf
rapport.pdf: rapport.tex
	pdflatex rapport.tex; pdflatex rapport.tex
figures:
	make -C figs
clean:
	rm -f rapport.aux
	rm -f rapport.log
	rm -f rapport.lof
	rm -f rapport.lot
	rm -f rapport.out
	rm -f rapport.toc
	rm -f rapport.pdf
