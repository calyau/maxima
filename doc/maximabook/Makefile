#
# Makefile to do all printouts:
#
# Printer where things will go to:

PRINTER = yours_here

.SUFFIXES:	.tex

all:	ps pdf

clean:
		rm -f *~ *.aux *.log *.dvi *.bbl *.blg *.brf *.idx *.ilg *.ing \
		*.ind *.lof *.toc *.tpt *.out *.png *.pdf *.ps thumbdta.tex

.tex:
		latex $<

printoutdvi:
		dvips -P$(PRINTER) maxima.dvi

printoutpdf:
		lpr -h -P$(PRINTER) maxima.pdf

problems:	
		latex all_pro

sharpr:
	shar -m -w ../P > MonsterPr

sharsol:
	shar -m -w ../S > MonsterSol

index:
	makeindex -o maxima.ind maxima.idx

dvi:
		latex maxima
		makeindex -o maxima.ind maxima.idx
		bibtex maxima
		latex maxima
		latex maxima

ps:	
		latex maxima
		makeindex -o maxima.ind maxima.idx
		bibtex maxima
		latex maxima
		latex maxima
		dvips -o maxima.ps maxima.dvi

ps4page:
		latex maxima
		makeindex -o maxima.ind maxima.idx
		bibtex maxima
		latex maxima
		latex maxima
		a2ps -4 -o maxima4pagesperpage.ps maxima.dvi
			
pdf:
		pdflatex maxima
		makeindex -o maxima.ind maxima.idx
		bibtex maxima
		pdflatex maxima
		pdflatex maxima

pdf-final:
		pdflatex maxima
		makeindex -o maxima.ind maxima.idx
		bibtex maxima
		thumbpdf maxima
		pdflatex maxima
		pdflatex maxima
