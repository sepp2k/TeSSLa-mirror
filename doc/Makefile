MAIN = main
LATEX = pdflatex -halt-on-error

all:
	rm -f $(MAIN).tdo
	$(LATEX) $(MAIN).tex
#	bibtex main.aux
#	$(LATEX) $(MAIN).tex

clean: 
	rm -f *.toc *.aux *~ *.dvi *.out *.log $(MAIN).ps *.idx *.snm *.nav *.tdo *.bbl *.blg main.vtc

 
bib: 
#  rm -f *.toc *.aux *~ *.dvi *.out *.log $(MAIN).ps *.idx *.snm *.nav *.tdo
	$(LATEX) $(MAIN)
	bibtex $(MAIN)
	$(LATEX) $(MAIN)
	$(LATEX) $(MAIN)

