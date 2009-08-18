OBJ=$(wildcard *.plt)
imagespdf: $(OBJ:plt=pdf)
%.pdf: %.plt %.txt
	gnuplot $<
	epstopdf `basename $< plt`pdf
