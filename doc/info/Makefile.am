if LANG_ES
  ES_DIR = es
endif

if LANG_PT
  PT_DIR = pt
endif

if LANG_PT_BR
  PT_BR_DIR = pt_BR
endif

if LANG_DE
  DE_DIR = de
endif

if LANG_JA
  JA_DIR = ja
endif

if LANG_RU
  RU_DIR = ru
endif

SUBDIRS = $(ES_DIR) $(PT_DIR) $(PT_BR_DIR) $(DE_DIR) $(JA_DIR) $(RU_DIR)

info_TEXINFOS = #maxima.texi
maxima_TEXINFOS = include-maxima.texi category-macros.texi Introduction.texi \
 Bugs.texi Help.texi Command.texi DataTypes.texi Constants.texi Lists.texi \
 Arrays.texi defstruct.texi Expressions.texi Operators.texi Evaluation.texi \
 Simplification.texi MathFunctions.texi Database.texi Plotting.texi \
 Input.texi Polynomials.texi extensions.texi Special.texi Elliptic.texi Limits.texi \
 Differentiation.texi Integration.texi Equations.texi Differential.texi \
 Numerical.texi Matrices.texi Affine.texi Itensor.texi Ctensor.texi \
 Atensor.texi Series.texi Number.texi Symmetries.texi Groups.texi \
 Runtime.texi Miscellaneous.texi Rules.texi nset.texi Function.texi \
 Program.texi Debugging.texi alt-display.texi asympa.texi \
 augmented_lagrangian.texi bitwise.texi bernstein.texi bode.texi cartan.texi celine.texi \
 clebsch_gordan.texi combinatorics.texi odepack.texi \
 cobyla.texi colnew.texi contrib_ode.texi descriptive.texi diag.texi distrib.texi \
 draw.texi drawdf.texi dynamics.texi engineering-format.texi \
 wrstcse.texi ezunits.texi f90.texi finance.texi fractals.texi gentran.texi \
 ggf.texi graphs.texi grobner.texi hompack.texi impdiff.texi interpol.texi \
 lapack.texi levin.texi lbfgs.texi lindstedt.texi linearalgebra.texi lsquares.texi \
 minpack.texi makeOrders.texi mnewton.texi numericalio.texi opsubst.texi \
 operatingsystem.texi orthopoly.texi pytranslate.texi quantum_computing.texi \
 ratpow.texi rk_adaptive.texi romberg.texi simplex.texi simplifications.texi \
 solve_rec.texi stats.texi stirling.texi stringproc.texi to_poly_solve.texi \
 unit.texi zeilberger.texi errormessages.texi commandline-options.texi \
 pslq.texi trigtools.texi format.texi

figurefiles = $(wildcard figures/*.png)

pdffigures = $(wildcard figures/*.pdf)

macfiles = $(wildcard figures/*.mac)

INFOLANG ?= en

# We need the html docu to be generated on "make all" because if we fail
# to do so a "sudo make install" will generate the html docu as read-only
# fails conflicting with a subsequent "make html".
all-local: html maxima.info maxima-info.info maxima-index.lisp maxima-index-html.lisp

infoname = maxima
include $(top_srcdir)/common-info.mk
maxima.info : maxima.texi $(maxima_TEXINFOS)
	$(MAKEINFO) -D textmanual $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) \
	  --split-size=1000000 $(srcdir)/maxima.texi

maxima-info.info : maxima.texi $(maxima_TEXINFOS)
	$(MAKEINFO) -o maxima-info.info $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) \
	  --split-size=1000000 $(srcdir)/maxima.texi
html: index.html

maxima-index.lisp: build_index.pl maxima.info
	/usr/bin/env perl $^ > $@

# Really depends on all the individual html files, but let's assume
# that if index.html is done, we have all the remaining html
# files.
#
# Load build-html-index.lisp and run the builder to create
# maxima-index-html.lisp.  In a clean directory, there won't be a
# maxima-index-html.lisp, so we don't want to try to verify the html
# index to prevent spurious warnings.  Then after it's done, run
# maxima to verify the index.
maxima-index-html.lisp : index.html build-html-index.lisp
	$(MAKE) -C $(top_builddir)/src
	../../maxima-local --no-init --preload=$(top_srcdir)/doc/info/build-html-index.lisp --batch-string='build_and_dump_html_index("./*.html");'
	../../maxima-local --no-init --verify-html-index --batch-string="quit();"

DOLLAR_SIGN = $$

maxima_singlepage.html index.html: maxima.texi $(maxima_TEXINFOS) $(figurefiles) manual.css texi2html.init
	./build_html.sh

maxima.epub: maxima_singlepage.html
	ebook-convert maxima_singlepage.html maxima.epub --breadth-first "--title=Maxima @manual_version@ Manual" "--authors=Maxima Team" --language=en

maxima.pdf: maxima.texi $(maxima_TEXINFOS) $(pdffigures)
	$(TEXI2PDF) $(AM_V_texinfo) -o maxima.pdf $<
	rm -f maxima.fns maxima.vr maxima.tp maxima.pg maxima.ky maxima.cp \
	maxima.toc maxima.fn maxima.aux maxima.log


include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)
genericdirDATA = $(figurefiles)

htmlname = *
htmlinstdir = $(dochtmldir)
include $(top_srcdir)/common-html.mk

all-local: warn_texinfo
dist-hook: check_texinfo html
pdf-local: maxima.pdf

install-data-local: install-maxima-index
uninstall-local: uninstall-maxima-index

install-maxima-index: maxima-index.lisp maxima-index-html.lisp
	$(MKDIR_P) "$(DESTDIR)$(infodir)"
	$(INSTALL_DATA) maxima-index.lisp "$(DESTDIR)$(infodir)/maxima-index.lisp"
	$(INSTALL_DATA) maxima-index-html.lisp "$(DESTDIR)$(infodir)/maxima-index-html.lisp"

uninstall-maxima-index: maxima-index.lisp maxima-index-html.lisp
	rm -f "$(DESTDIR)$(infodir)/maxima-index.lisp"
	rm -f "$(DESTDIR)$(infodir)/maxima-index-html.lisp"

clean-local: clean-info clean-html

clean-info:
	rm -f $(wildcard maxima.info* maxima-info.info* maxima-index.lisp maxima-index-html.lisp)
	rm -f maxima.fns build-html-index.log
	rm -f $(patsubst $(srcdir)/%.texi.m4,%.texi,$(wildcard $(srcdir)/*.texi.m4))

clean-html:
	rm -f $(wildcard *.html) \
	      $(wildcard $(addprefix $(srcdir),*.html))

EXTRA_DIST = manual.css texi2html.init maxima.texi dlmf-ref-macros.texi \
    $(genericdirDATA) $(pdffigures) $(macfiles) $(maxima_TEXINFOS) $(figurefiles) \
    create_index build_index.pl maxima-index.lisp maxima-index-html.lisp docdraw.mac \
    extract_categories1.awk extract_categories1.sed check_tab fix_tab \
    figures/Makefile.am figures/Makefile.in figures/README \
    maxima-init.lisp README.update_examples maxima-index-html.lisp build-html-index.lisp \
    figures/orthopoly1.ps.gz figures/plotdf1.ps.gz figures/plotdf2.ps.gz figures/plotdf3.ps.gz figures/plotdf4.ps.gz figures/plotdf5.ps.gz figures/plotdf6.ps.gz

check_texinfo:
	@pattern=`printf "\t"` ; \
	bad_files=`find . -name '*.texi' -print0 | xargs -0 $(EGREP) -l -e "$$pattern"` ; \
	[ -z  "$$bad_files" ] || ( echo "ERROR: The following files have unexpanded Tabs: $$bad_files" ; \
	echo "Run make distclean and then doc/info/fix_tab to fix the problem." ; exit 1 )

warn_texinfo:
	@pattern=`printf "\t"` ; \
	bad_files=`find . -name '*.texi' -print0 | xargs -0 $(EGREP) -l -e "$$pattern"` ; \
	[ -z  "$$bad_files" ] || ( echo "WARNING: The following files have unexpanded Tabs: $$bad_files" ; \
	echo "Run doc/info/fix_tab to fix the problem." )

distclean-local:
	rm -f figures/Makefile


.PHONY: check_texinfo warn_texinfo

# Preprocess the files with m4 to handle math equations.
%.texi : math.m4 %.texi.m4
	m4 --prefix-builtins $^ > $@
