OBJ=$(wildcard *.plt)
imagespdf: $(OBJ:plt=pdf)
%.pdf: %.plt
	gnuplot $<
	epstopdf `basename $< plt`pdf
