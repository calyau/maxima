include $(top_srcdir)/common.mk
MAXIMA = $(abs_top_builddir)/maxima-local
MAXIMA_LAYOUT_AUTOTOOLS = true
export

TESTS =
CLEANFILES = 

if CLISP
TESTS += clisp-test.sh
CLEANFILES += clisp-test.sh clisp.log clisp_share.log
clisp-test.sh: test.sh.in
	sed <"$<" >"$@"                                          \
	  -e 's#!LOCAL_MAXIMA!#$(MAXIMA)#'                       \
	  -e 's#!LISPNAME!#clisp#'                               \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/clisp.log#'   \
	  -e 's#!OUTPUT_SHARE_LOG!#$(top_builddir)/tests/clisp_share.log#'
	chmod a+x "$@"
endif

if CMUCL
TESTS += cmucl-test.sh
CLEANFILES += cmucl-test.sh cmucl.log cmucl_share.log
cmucl-test.sh: test.sh.in
	sed <"$<" >"$@"                                          \
	  -e 's#!LOCAL_MAXIMA!#$(MAXIMA)#'                       \
	  -e 's#!LISPNAME!#cmucl#'                               \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/cmucl.log#'   \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/cmucl_share.log#'
	chmod a+x "$@"
endif

if SCL
TESTS += scl-test.sh
CLEANFILES += scl-test.sh scl.log scl_share.log
scl-test.sh: test.sh.in
	sed <"$<" >"$@"                                          \
	  -e 's#!LOCAL_MAXIMA!#$(MAXIMA)#'                       \
	  -e 's#!LISPNAME!#scl#'                                 \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/scl.log#'     \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/scl_share.log#'
	chmod a+x "$@"
endif

if SBCL
TESTS += sbcl-test.sh
CLEANFILES += sbcl-test.sh sbcl.log sbcl_share.log
sbcl-test.sh: test.sh.in
	sed <"$<" >"$@"                                          \
	  -e 's#!LOCAL_MAXIMA!#$(MAXIMA)#'                       \
	  -e 's#!LISPNAME!#sbcl#'                                \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/sbcl.log#'    \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/sbcl_share.log#'
	chmod a+x "$@"
endif

if GCL
TESTS += gcl-test.sh
CLEANFILES += gcl-test.sh gcl.log gcl_share.log
gcl-test.sh: test.sh.in
	sed <"$<" >"$@"                                          \
	  -e 's#!LOCAL_MAXIMA!#$(MAXIMA)#'                       \
	  -e 's#!LISPNAME!#gcl#'                                 \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/gcl.log#'     \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/gcl_share.log#'
	chmod a+x "$@"
endif

if ACL
TESTS += acl-test.sh
CLEANFILES += acl-test.sh acl.log acl_share.log
acl-test.sh: test.sh.in
	sed <"$<" >"$@"                                          \
	  -e 's#!LOCAL_MAXIMA!#$(MAXIMA)#'                       \
	  -e 's#!LISPNAME!#acl#'                                 \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/acl.log#'     \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/acl_share.log#'
	chmod a+x "$@"
endif

if OPENMCL
TESTS += openmcl-test.sh
CLEANFILES += openmcl-test.sh openmcl.log openmcl_share.log
openmcl-test.sh: test.sh.in
	sed <"$<" >"$@"                                          \
	  -e 's#!LOCAL_MAXIMA!#$(MAXIMA)#'                       \
	  -e 's#!LISPNAME!#openmcl#'                             \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/openmcl.log#' \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/openmcl_share.log#'
	chmod a+x "$@"
endif

if CCL64
TESTS += ccl64-test.sh
CLEANFILES += ccl64-test.sh ccl64.log ccl64_share.log
ccl64-test.sh: test.sh.in
	sed <"$<" >"$@"                                          \
	  -e 's#!LOCAL_MAXIMA!#$(MAXIMA)#'                       \
	  -e 's#!LISPNAME!#ccl64#'                               \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/ccl64.log#'   \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/ccl64_share.log#'
	chmod a+x "$@"
endif

if ECL
TESTS += ecl-test.sh
CLEANFILES += ecl-test.sh ecl.log ecl_share.log
ecl-test.sh: test.sh.in
	sed <"$<" >"$@"                                          \
	  -e 's#!LOCAL_MAXIMA!#$(MAXIMA)#'                       \
	  -e 's#!LISPNAME!#ecl#'                                 \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/ecl.log#'     \
	  -e 's#!OUTPUT_LOG!#$(top_builddir)/tests/ecl_share.log#'
	chmod a+x "$@"
endif

genericdir = $(insttestsdir)
genericdirDATA = $(wildcard $(addprefix $(srcdir)/, README.how-to mcond_display_elseif.out *.mac *.lisp *.wxm wester_problems/*))

EXTRA_DIST = $(genericdirDATA) test.sh.in
