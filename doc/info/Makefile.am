if LANG_ES
  ES_DIR = es
endif

if LANG_ES_UTF8
  ES_DIR_UTF8 = es.utf8
endif

if LANG_PT
  PT_DIR = pt
endif

if LANG_PT_UTF8
  PT_DIR_UTF8 = pt.utf8
endif

if LANG_PT_BR
  PT_BR_DIR = pt_BR
endif

if LANG_PT_BR_UTF8
  PT_BR_DIR_UTF8 = pt_BR.utf8
endif

SUBDIRS = $(ES_DIR) $(ES_DIR_UTF8) $(PT_DIR) $(PT_DIR_UTF8) $(PT_BR_DIR) $(PT_BR_DIR_UTF8)

info_TEXINFOS = maxima.texi
maxima_TEXINFOS = Introduction.texi Help.texi Command.texi Operators.texi \
  Expressions.texi Simplification.texi Plotting.texi Input.texi \
  Floating.texi Contexts.texi Polynomials.texi Constants.texi \
  Logarithms.texi Trigonometric.texi Special.texi \
  Limits.texi Differentiation.texi Integration.texi Equations.texi \
  Differential.texi Numerical.texi \
  Statistics.texi Arrays.texi Matrices.texi Affine.texi Itensor.texi \
  Ctensor.texi Atensor.texi Series.texi Number.texi Symmetries.texi \
  Groups.texi Runtime.texi Miscellaneous.texi Rules.texi Lists.texi \
  Function.texi Program.texi Debugging.texi Indices.texi Elliptic.texi \
  Bugs.texi nset.texi zeilberger.texi orthopoly.texi linearalgebra.texi \
  solve_rec.texi stringproc.texi numericalio.texi descriptive.texi \
  distrib.texi simplex.texi simplifications.texi unit.texi makeOrders.texi \
  mnewton.texi lsquares.texi plotdf.texi lindstedt.texi impdiff.texi \
  ggf.texi f90.texi eval_string.texi diag.texi cholesky.texi dynamics.texi\
  bode.texi augmented_lagrangian.texi opsubst.texi stats.texi stirling.texi\
  implicit_plot.texi interpol.texi lbfgs.texi romberg.texi

figurefiles = \
figures/dynamics1.gif\
figures/dynamics2.gif\
figures/dynamics3.gif\
figures/dynamics4.gif\
figures/dynamics5.gif\
figures/dynamics6.gif\
figures/dynamics7.gif\
figures/dynamics8.gif\
figures/dynamics9.gif\
figures/dynamics10.gif\
figures/implicit_plot.gif\
figures/plotdf1.gif\
figures/plotdf2.gif\
figures/plotdf3.gif\
figures/plotdf4.gif\
figures/plotdf5.gif\
figures/plotdf6.gif\
figures/plotting1.gif\
figures/plotting2.gif\
figures/plotting3.gif\
figures/plotting4.gif\
figures/plotting5.gif\
figures/plotting6.gif\
figures/plotting7.gif\
figures/plotting8.gif\
figures/plotting9.gif\
figures/plotting10.gif\
figures/plotting11.gif\
figures/plotting12.gif\
figures/plotting13.gif\
figures/plotting14.gif\
figures/plotting15.gif\
figures/plotting16.gif\
figures/plotting17.gif\
figures/plotting18.gif\
figures/plotting19.gif\
figures/plotting20.gif\
figures/plotting21.gif\
figures/plotting22.gif\
figures/plotting23.gif\
figures/plotting24.gif\
figures/plotting25.gif\
figures/orthopoly1.gif

pdffigures = \
figures/dynamics1.pdf\
figures/dynamics2.pdf\
figures/dynamics3.pdf\
figures/dynamics4.pdf\
figures/dynamics5.pdf\
figures/dynamics6.pdf\
figures/dynamics7.pdf\
figures/dynamics8.pdf\
figures/dynamics9.pdf\
figures/dynamics10.pdf\
figures/implicit_plot.pdf\
figures/plotdf1.pdf\
figures/plotdf2.pdf\
figures/plotdf3.pdf\
figures/plotdf4.pdf\
figures/plotdf5.pdf\
figures/plotdf6.pdf\
figures/plotting1.pdf\
figures/plotting2.pdf\
figures/plotting3.pdf\
figures/plotting4.pdf\
figures/plotting5.pdf\
figures/plotting6.pdf\
figures/plotting7.pdf\
figures/plotting8.pdf\
figures/plotting9.pdf\
figures/plotting10.pdf\
figures/plotting11.pdf\
figures/plotting12.pdf\
figures/plotting13.pdf\
figures/plotting14.pdf\
figures/plotting15.pdf\
figures/plotting16.pdf\
figures/plotting17.pdf\
figures/plotting18.pdf\
figures/plotting19.pdf\
figures/plotting20.pdf\
figures/plotting21.pdf\
figures/plotting22.pdf\
figures/plotting23.pdf\
figures/plotting24.pdf\
figures/plotting25.pdf\
figures/orthopoly1.pdf

all-local: maxima-index.lisp maxima.html contents.hhc

maxima-index.lisp: maxima.info build_index.pl
	perl ./build_index.pl maxima.info ':crlf' > maxima-index.lisp

maxima.html: maxima.texi $(maxima_TEXINFOS)
	perl ./texi2html -split_chapter --lang=en --output=. --css-include=./manual.css --init-file ./texi2html.init maxima.texi 

contents.hhc: maxima.html
	perl ./create_index

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)
genericdirDATA = $(figurefiles) \
contents.hhc index.hhk header.hhp maxima.hhp

htmlname = maxima
htmlinstdir = $(dochtmldir)
include $(top_srcdir)/common-html.mk

all-local: warn_texinfo
dist-hook: check_texinfo

install-data-local: install-maxima-index
uninstall-local: uninstall-maxima-index

install-maxima-index: maxima-index.lisp
	test -z "$(infodir)" || mkdir -p -- "$(DESTDIR)$(infodir)"
	$(INSTALL_DATA) maxima-index.lisp "$(DESTDIR)$(infodir)/maxima-index.lisp"

uninstall-maxima-index: maxima-index.lisp
	rm -f "$(DESTDIR)$(infodir)/maxima-index.lisp"

clean-local: clean-info
clean-local: clean-html

clean-info:
	rm -f maxima.info*
	rm -f maxima-index.lisp

clean-html:
	rm -f maxima*.html
	rm -f contents.hhc
	rm -f index.hhk

EXTRA_DIST = texi2html manual.css texi2html.init \
    $(genericdirDATA) $(pdffigures) \
    create_index maxima-index.lisp \
    check_crlf check_tab fix_crlf fix_tab build_index.pl

check_texinfo:
	pattern=`printf "\r$$"` ; \
	bad_files=`find . -name '*.texi' -print | xargs grep -l -e "$$pattern"` ; \
	[ -z  "$$bad_files" ] || ( echo "ERROR: The following files have DOS-style EOLs: $$bad_files" ; \
	echo "Run make distclean and then /doc/info/fix_crlf to fix the problem." ; exit 1 )
	pattern=`printf "\t"` ; \
	bad_files=`find . -name '*.texi' -print | xargs grep -l -e "$$pattern"` ; \
	[ -z  "$$bad_files" ] || ( echo "ERROR: The following files have unexpanded Tabs: $$bad_files" ; \
	echo "Run make distclean and then /doc/info/fix_tab to fix the problem." ; exit 1 )

warn_texinfo:
	pattern=`printf "\r$$"` ; \
	bad_files=`find . -name '*.texi' -print | xargs grep -l -e "$$pattern"` ; \
	[ -z  "$$bad_files" ] || ( echo "WARNING: The following files have DOS-style EOLs: $$bad_files" ; \
	echo "Run /doc/info/fix_crlf to fix the problem." )
	pattern=`printf "\t"` ; \
	bad_files=`find . -name '*.texi' -print | xargs grep -l -e "$$pattern"` ; \
	[ -z  "$$bad_files" ] || ( echo "WARNING: The following files have unexpanded Tabs: $$bad_files" ; \
	echo "Run /doc/info/fix_tab to fix the problem." )

.PHONY: check_texinfo warn_texinfo

