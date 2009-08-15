EPSS=$(wildcard *.plt)
imagespdf: $(EPSS:plt=pdf)
%.pdf: %.plt %.txt
	gnuplot $<
	epstopdf `basename $< plt`pdf
