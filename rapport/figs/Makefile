EPSS=$(wildcard *.plt)
imagespdf: $(EPSS:plt=pdf)
%.pdf: %.plt
	gnuplot $<
	epstopdf `basename $< plt`pdf
