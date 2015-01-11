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

if LANG_DE
  DE_DIR = de
endif

if LANG_DE_UTF8
  DE_DIR_UTF8 = de.utf8
endif

SUBDIRS = $(ES_DIR) $(ES_DIR_UTF8) $(PT_DIR) $(PT_DIR_UTF8) $(PT_BR_DIR) $(PT_BR_DIR_UTF8) $(DE_DIR) $(DE_DIR_UTF8)

info_TEXINFOS = maxima.texi
maxima_TEXINFOS = category-macros.texi Introduction.texi \
 Bugs.texi Help.texi Command.texi DataTypes.texi Constants.texi Lists.texi \
 Arrays.texi defstruct.texi Expressions.texi Operators.texi Evaluation.texi \
 Simplification.texi MathFunctions.texi Database.texi Plotting.texi \
 Input.texi Polynomials.texi Special.texi Elliptic.texi Limits.texi \
 Differentiation.texi Integration.texi Equations.texi Differential.texi \
 Numerical.texi Matrices.texi Affine.texi Itensor.texi Ctensor.texi \
 Atensor.texi Series.texi Number.texi Symmetries.texi Groups.texi \
 Runtime.texi Miscellaneous.texi Rules.texi nset.texi Function.texi \
 Program.texi Debugging.texi alt-display.texi asympa.texi \
 augmented_lagrangian.texi bernstein.texi bode.texi clebsch_gordan.texi \
 cobyla.texi contrib_ode.texi descriptive.texi diag.texi distrib.texi \
 draw.texi drawdf.texi dynamics.texi ezunits.texi f90.texi finance.texi \
 fractals.texi ggf.texi graphs.texi grobner.texi impdiff.texi interpol.texi \
 lapack.texi lbfgs.texi lindstedt.texi linearalgebra.texi lsquares.texi \
 minpack.texi makeOrders.texi mnewton.texi numericalio.texi opsubst.texi \
 orthopoly.texi romberg.texi simplex.texi simplifications.texi solve_rec.texi \
 stats.texi stirling.texi stringproc.texi to_poly_solve.texi unit.texi \
 zeilberger.texi

figurefiles = \
figures/introduction1.gif\
figures/introduction2.gif\
figures/introduction3.gif\
figures/dynamics1.png\
figures/dynamics2.png\
figures/dynamics3.png\
figures/dynamics4.png\
figures/dynamics5.png\
figures/dynamics6.png\
figures/dynamics7.png\
figures/dynamics8.png\
figures/plotdf1.gif\
figures/plotdf2.gif\
figures/plotdf3.gif\
figures/plotdf4.gif\
figures/plotdf5.gif\
figures/plotdf6.gif\
figures/plotting1.png\
figures/plotting2.png\
figures/plotting3.png\
figures/plotting4.png\
figures/plotting5.png\
figures/plotting6.png\
figures/plotting7.png\
figures/plotting8.png\
figures/plotting9.png\
figures/plotting10.png\
figures/plotting11.png\
figures/plotting12.png\
figures/plotting13.png\
figures/plotting14.png\
figures/plotting15.png\
figures/plotting16.png\
figures/plotting17.png\
figures/plotting18.png\
figures/plotting19.png\
figures/plotting20.png\
figures/plotting21.png\
figures/plotting22.png\
figures/plotting23.png\
figures/plotting24.png\
figures/plotting25.png\
figures/plotting26.png\
figures/orthopoly1.gif\
figures/graphs01.gif\
figures/graphs02.gif\
figures/graphs03.gif\
figures/graphs04.gif\
figures/graphs05.gif\
figures/graphs06.gif\
figures/graphs07.gif\
figures/graphs08.gif\
figures/graphs09.gif\
figures/graphs10.gif\
figures/graphs11.gif\
figures/graphs12.gif\
figures/graphs13.gif\
figures/graphs14.gif\
figures/graphs15.gif\
figures/favicon.ico\
figures/bodegain1.gif\
figures/bodegain2.gif\
figures/bodegain3.gif\
figures/bodegain4.gif\
figures/bodegain5.gif\
figures/bodegain6.gif\
figures/bodegain7.gif\
figures/bodegain8.gif\
figures/bodephase1.gif\
figures/bodephase2.gif\
figures/bodephase3.gif\
figures/bodephase4.gif\
figures/bodephase5.gif\
figures/bodephase6.gif\
figures/bodephase7.gif\
figures/bodephase8.gif\
figures/bodephase9.gif\
figures/scene1.png\
figures/scene2.png\
figures/color_terminal.png

pdffigures = \
figures/introduction1.pdf\
figures/introduction2.pdf\
figures/introduction3.pdf\
figures/dynamics1.pdf\
figures/dynamics2.pdf\
figures/dynamics3.pdf\
figures/dynamics4.pdf\
figures/dynamics5.pdf\
figures/dynamics6.pdf\
figures/dynamics7.pdf\
figures/dynamics8.pdf\
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
figures/plotting26.pdf\
figures/orthopoly1.pdf\
figures/bodegain1.pdf\
figures/bodegain2.pdf\
figures/bodegain3.pdf\
figures/bodegain4.pdf\
figures/bodegain5.pdf\
figures/bodegain6.pdf\
figures/bodegain7.pdf\
figures/bodegain8.pdf\
figures/bodephase1.pdf\
figures/bodephase2.pdf\
figures/bodephase3.pdf\
figures/bodephase4.pdf\
figures/bodephase5.pdf\
figures/bodephase6.pdf\
figures/bodephase7.pdf\
figures/bodephase8.pdf\
figures/bodephase9.pdf\
figures/scene1.pdf\
figures/scene2.pdf\
figures/color_terminal.pdf

if CHM
MAXIMA_CHM = maxima.chm
INSTALL_CHM = install-chm
UNINSTALL_CHM = uninstall-chm
CLEAN_CHM = clean-chm
endif

all-local: maxima.info maxima-index.lisp $(MAXIMA_CHM)

maxima.info : maxima.texi
	$(MAKEINFO) --split-size=1000000 maxima.texi

html: maxima.html contents.hhc

maxima-index.lisp: maxima.info build_index.pl
	perl ./build_index.pl maxima.info ':crlf' > maxima-index.lisp

maxima.html: maxima.texi $(maxima_TEXINFOS)
	sh extract_categories.sh maxima

maxima.pdf: maxima.texi $(maxima_TEXINFOS)
	$(TEXI2PDF) $(AM_V_texinfo) -o maxima.pdf maxima.texi
	rm maxima.fns maxima.vr maxima.tp maxima.pg maxima.ky maxima.cp \
	maxima.toc maxima.fn maxima.aux maxima.log

contents.hhc index.hhk: maxima.html
	perl ./create_index

# Squash error on exit from hhc
maxima.chm: maxima.hhp contents.hhc index.hhk
	-"$(HHC)" maxima.hhp

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)
genericdirDATA = $(figurefiles) \
contents.hhc index.hhk header.hhp maxima.hhp

htmlname = maxima
htmlinstdir = $(dochtmldir)
include $(top_srcdir)/common-html.mk

all-local: warn_texinfo
dist-hook: check_texinfo html

install-data-local: install-maxima-index $(INSTALL_CHM)
uninstall-local: uninstall-maxima-index $(UNINSTALL_CHM)

install-maxima-index: maxima-index.lisp
	test -z "$(infodir)" || mkdir -p -- "$(DESTDIR)$(infodir)"
	$(INSTALL_DATA) maxima-index.lisp "$(DESTDIR)$(infodir)/maxima-index.lisp"

uninstall-maxima-index: maxima-index.lisp
	rm -f "$(DESTDIR)$(infodir)/maxima-index.lisp"

clean-local: clean-info clean-html $(CLEAN_CHM)

clean-info:
	rm -f maxima.info*
	rm -f maxima-index.lisp

clean-html:
	rm -f maxima*.html
	rm -f contents.hhc
	rm -f index.hhk
	rm -f maxima.chm

if CHM
install-chm: maxima.chm
	test -z "$(DESTDIR)$(docchmdir)" || mkdir -p -- "$(DESTDIR)$(docchmdir)"
	$(INSTALL_DATA) maxima.chm "$(DESTDIR)$(docchmdir)/maxima.chm"

uninstall-chm:
	rm -f "$(DESTDIR)$(docchmdir)"

clean-chm:
	rm -f maxima.chm
	rm -rf chm
endif

EXTRA_DIST = texi2html manual.css texi2html.init \
    $(genericdirDATA) $(pdffigures) \
    create_index build_index.pl maxima-index.lisp  \
    extract_categories1.awk  extract_categories1.sed  extract_categories.sh

check_texinfo:
	@pattern=`printf "\t"` ; \
	bad_files=`find . -name '*.texi' -print | xargs $(EGREP) -l -e "$$pattern"` ; \
	[ -z  "$$bad_files" ] || ( echo "ERROR: The following files have unexpanded Tabs: $$bad_files" ; \
	echo "Run make distclean and then /doc/info/fix_tab to fix the problem." ; exit 1 )

warn_texinfo:
	@pattern=`printf "\t"` ; \
	bad_files=`find . -name '*.texi' -print | xargs $(EGREP) -l -e "$$pattern"` ; \
	[ -z  "$$bad_files" ] || ( echo "WARNING: The following files have unexpanded Tabs: $$bad_files" ; \
	echo "Run /doc/info/fix_tab to fix the problem." )

.PHONY: check_texinfo warn_texinfo

