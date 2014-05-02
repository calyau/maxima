all-local: info

info: logic.info logic-index.lisp

logic.info: logic.texi
	makeinfo --force logic.texi

logic-index.lisp: logic.info
	perl ../../doc/info/build_index.pl $< > $@
