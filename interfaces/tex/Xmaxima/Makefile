# -*-mode: makefile; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#	$Id: Makefile,v 1.2 2006-06-27 16:52:59 villate Exp $
#

# TEXINPUTS=../texinputs:${TEXINPUTS}

HTML = ../../html/Maxima

stys = maxima.sty

#perls = maxima.perl


default:	pdf

#html:	${HTML}/Xmaxima/Xmaxima.html 

pdf:	Xmaxima.pdf

Xmaxima.pdf:	Xmaxima.tex $(stys)
	pdflatex Xmaxima.tex
	makeindex Xmaxima
	pdflatex Xmaxima.tex

dvi:	Xmaxima.dvi

Xmaxima.dvi:	Xmaxima.tex $(stys)
	latex Xmaxima.tex
	makeindex Xmaxima
	latex Xmaxima.tex
	latex Xmaxima.tex

touch::
	-chmod 644 Xmaxima.tex
	touch Xmaxima.tex

#${HTML}/Xmaxima/Xmaxima.html:	Xmaxima.tex perls
#	rm -rf ${HTML}/Xmaxima
#	mkdir ${HTML}/Xmaxima
#	latex2html.sh -reuse 1 -split 4 -link 2 -info 0 \
#		-dir $(HTML)/Xmaxima \
#		-noaddress -top_navigation \
#		 Xmaxima.tex 
#	cp ${HTML}/Xmaxima/Xmaxima.html ${HTML}/Xmaxima/index.html


clean:
#	rm -rf ${HTML}/Xmaxima
	rm -f *.dvi core

veryclean:
	rm -f *.log *.aux *.idx *.ind *.ilg *.log *.out *.toc

