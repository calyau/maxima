

all:
	echo '(proclaim (quote(optimize (safety 0))))(in-package "MAXIMA")(load "compile.lsp")' \
	| maxima


all:
	echo 'load("compile.lsp");' |  ../src/saved_maxima


doc:
	latex docsym.tex

clean:
	rm -f *.aux *.dvi *.toc *.log
	rm -f *.o

tar:
	(cd .. ; tar cvf - sym/makefile sym/*.lsp sym/*.tex sym/doconline sym/*.mac src/max_ext.lisp doc/macsym.doc doc/macsym-index.LISP ) | gzip -c > sym.tgz



