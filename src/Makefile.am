include $(top_srcdir)/common.mk

if WIN32
bin_SCRIPTS = maxima-command.ico maxima.bat set_lang.vbs
else
bin_SCRIPTS = maxima rmaxima
endif

## In instsrcdir, we install the lisp source files in src/ (including
## the numerical subdirectories).
## TODO: Should we also be installing the .system and .asd files here?
real_lisp_sources := $(shell echo *.lisp numerical/*.lisp numerical/slatec/*.lisp)
nobase_dist_instsrc_DATA = $(real_lisp_sources)
EXTRA_DIST = \
  maxima.asd maxima.system \
  numerical/slatec/fortran \
  maxima-build.lisp maxima-command.ico set_lang.vbs \
  lisp \
  share-subdirs.lisp

## If we can, we use Automake's blah_SCRIPTS targets to install
## stuff. When doing this, we have to populate EXTRA_SCRIPTS, so we
## use += and initialise here.
EXTRA_SCRIPTS =

## A debugging tool. If you're trying to understand the value of the
## variable $(foo) when make runs, call "make echo_foo" and it will be
## printed.
echo_%:
	@echo "$(subst echo_,,$@)=$($(subst echo_,,$@))"
	@echo "origin $(subst echo_,,$@) returns $(origin $(subst echo_,,$@))"

## "all" depends on sharefiles.mk and whatever files we choose to
## build. These are listed in BUILT_FILES, which is populated in the
## implementation-dependent sections below.
BUILT_FILES =
all-local: $(BUILT_FILES)

## The "clean" rule always just deletes some files and they are
## specified in the conditional sections by adding
## implementation-specific targets to the list CLEAN_TARGETS.
CLEAN_TARGETS =
clean-local: $(CLEAN_TARGETS)

## Tell make clean to delete all the <foo>-depends.mk files.
DEPENDS_FILES = \
  clisp-depends.mk cmucl-depends.mk scl-depends.mk acl-depends.mk \
  sbcl-depends.mk gcl-depends.mk openmcl-depends.mk ecl-depends.mk
CLEANFILES = $(DEPENDS_FILES) sys-proclaim.lisp

## Similarly, we do something hacky with the install rule. Most of the
## time, we can actually use Automake's tools for this, but for the
## crazy "install another copy of the implementation" approach that we
## use with SCL and CLISP, we need to do it by hand.
WEIRD_INSTALL_TARGETS =
WEIRD_UNINSTALL_TARGETS =
install-exec-local: $(WEIRD_INSTALL_TARGETS)
uninstall-hook: $(WEIRD_UNINSTALL_TARGETS)

## A rule to build binary directories of the form binary-<lispname>
## and subdirectories ./numerical and ./numerical/slatec
binary_subdirs = / /numerical /numerical/slatec
lisps_enabled = @lisps_enabled@
.PHONY: bd
bd:
	for l in $(lisps_enabled); do for d in $(binary_subdirs); do $(MKDIR_P) binary-$$l$$d; done; done

## Some hunks of lisp that get used by more than one implementation
LOADDEFSYSTEM = (load "$(top_srcdir)/lisp-utils/defsystem.lisp")
LOADMAKEDEPENDS = (load "$(top_srcdir)/lisp-utils/make-depends.lisp")
DS_OOS = funcall (intern (symbol-name :operate-on-system) :mk) "maxima"
if QUIET_BUILD
QUIET=(setf *compile-print* nil)
DEFSYSTEMCOMPILE = ($(DS_OOS) :compile :verbose nil)
DEFSYSTEMLOAD = ($(DS_OOS) :load :verbose nil)
else
DEFSYSTEMCOMPILE = ($(DS_OOS) :compile :verbose t)
DEFSYSTEMLOAD = ($(DS_OOS) :load :verbose t)
endif
DEFSYSTEMTESTLOAD = ($(DS_OOS) :load :verbose t :test t)

## A function that takes: $(1) = the target name or names (either a
## string or a list of strings); $(2) = <foo>-depends.mk; $(3) = any
## postscript that should be added in the progn. It outputs lisp code
## that, when run, should create the dependency Makefile fragments.
MAKE_DEPENDS = '(progn $(LOADDEFSYSTEM) $(LOADMAKEDEPENDS) (funcall (intern "CREATE-DEPENDENCY-FILE" :mk) $(1) "$(2)") $(3))'

## CLISP #######################################################################
if CLISP

EXTRA_SCRIPTS += $(CLISP_MAXIMA)

if CLISP_EXEC
CLISP_MAXIMA = binary-clisp/maxima$(EXEEXT)
clisplib_SCRIPTS = $(CLISP_MAXIMA)
clispexeflag = :EXECUTABLE t
else
CLISP_MAXIMA = binary-clisp/maxima.mem
clisplib_DATA = $(CLISP_MAXIMA)
clispexeflag =
## Rather crazily, we install an extra copy of clisp. Maybe there's a
## more sensible approach than this?!
WEIRD_INSTALL_TARGETS += install-clisp-copy
WEIRD_UNINSTALL_TARGETS += uninstall-clisp-copy
install-clisp-copy:
	$(mkinstalldirs) $(DESTDIR)$(clisplibdir)
	$(INSTALL_PROGRAM) @CLISP_RUNTIME_PATH@ "$(DESTDIR)$(clisplibdir)/@CLISP_RUNTIME@"
uninstall-clisp-copy:
	rm -f "$(DESTDIR)$(clisplibdir)/@CLISP_RUNTIME@"
endif

EXECUTECLISP = $(CLISP_NAME) -norc -q -x
clisplibdir = $(verpkglibdir)/binary-clisp

BUILT_FILES += $(CLISP_MAXIMA)
CLEAN_TARGETS += clean-clisp

clisp: $(CLISP_MAXIMA)

$(CLISP_MAXIMA):
	$(MAKE) bd
	$(EXECUTECLISP) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' && \
	$(EXECUTECLISP) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:saveinitmem "$@" :init-function (function cl-user::run) $(clispexeflag)))'

clean-clisp:
	rm -rf binary-clisp

clisp-depends.mk: maxima.system
	$(EXECUTECLISP) $(call MAKE_DEPENDS,"$(CLISP_MAXIMA)",clisp-depends.mk)

-include clisp-depends.mk
endif CLISP

## CMUCL #######################################################################
if CMUCL

EXTRA_SCRIPTS += $(CMU_MAXIMA) lisp

if CMUCL_EXEC
CMU_MAXIMA = binary-cmucl/maxima
cmucllib_SCRIPTS = $(CMU_MAXIMA)
cmuflag = :executable t :init-function '\''cl-user::run
else
CMU_MAXIMA = binary-cmucl/maxima.core
cmucllib_SCRIPTS = lisp
cmucllib_DATA = $(CMU_MAXIMA)
cmuflag =
endif

# Newer versions of CMUCL have an INTL package that is compatible with
# maxima's.  We just bind intl::*default-domain* here so that when we
# compile the files, we will get appropriate translations.  (Otherwise
# we have to put (intl:textdomain "maxima") in each Lisp file.
INITINTL = (when (find-package "INTL") (set (find-symbol "*DEFAULT-DOMAIN*" "INTL") "maxima"))
CMU_COMPILE = echo '(progn $(QUIET) $(INITINTL) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' | $(EXECUTECMUCL)
CMU_WRITE = echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:save-lisp "$(CMU_MAXIMA)" $(cmuflag)))' | $(EXECUTECMUCL)
CMU_BUILD = ($(CMU_COMPILE)) && ($(CMU_WRITE))
EXECUTECMUCL = $(CMUCL_NAME) -noinit -batch

cmucllibdir = $(verpkglibdir)/binary-cmucl
BUILT_FILES += $(CMU_MAXIMA)
CLEAN_TARGETS += clean-cmucl

cmucl: $(CMU_MAXIMA)

$(CMU_MAXIMA):
	$(MAKE) bd
	$(CMU_BUILD)

clean-cmucl:
	rm -rf binary-cmucl

cmucl-depends.mk: maxima.system
	echo $(call MAKE_DEPENDS,"$(CMU_MAXIMA)",cmucl-depends.mk) | $(EXECUTECMUCL)

-include cmucl-depends.mk
endif

## SCL #########################################################################
if SCL

scllibdir = $(verpkglibdir)/binary-scl

EXECUTESCL = $(SCL_NAME) -noinit -batch
BUILT_FILES += binary-scl/maxima.core
CLEAN_TARGETS += clean-scl
scllib_DATA = binary-scl/maxima.core

scl: binary-scl/maxima.core

binary-scl/maxima.core:
	$(MAKE) bd
	(echo '(progn $(QUIET) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' | $(EXECUTESCL)) && \
	(echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:save-lisp "binary-scl/maxima.core"))' | $(EXECUTESCL))

## Just like with clisp, we install an extra copy of SCL. No, I don't
## understand either. Also, I just read the SCL license agreement. Is
## the user even allowed to do this?!
WEIRD_INSTALL_TARGETS += install-scl-copy
WEIRD_UNINSTALL_TARGETS += uninstall-scl-copy
install-scl-copy:
	$(mkinstalldirs) $(DESTDIR)$(scllibdir)
	$(INSTALL_PROGRAM) @SCL_RUNTIME_PATH@ "$(DESTDIR)$(scllibdir)/@SCL_RUNTIME@"
uninstall-scl-copy:
	rm -f "$(DESTDIR)$(scllibdir)/@SCL_RUNTIME@"

clean-scl:
	rm -rf binary-scl

scl-depends.mk: maxima.system
	echo $(call MAKE_DEPENDS,"binary-scl/maxima.core",scl-depends.mk) | $(EXECUTESCL)

-include scl-depends.mk

endif

## ACL #########################################################################
if ACL

acllibdir = $(verpkglibdir)/binary-acl

EXECUTEACL = $(ACL_NAME) -batch
BUILT_FILES += binary-acl/maxima.dxl
CLEAN_TARGETS += clean-acl

acllib_DATA = binary-acl/maxima.dxl

acl: binary-acl/maxima.dxl

binary-acl/maxima.dxl:
	$(MAKE) bd
	(echo '(progn $(QUIET) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' | $(EXECUTEACL)) && \
	(echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (excl:dumplisp :name "binary-acl/maxima.dxl"))' | $(EXECUTEACL))

clean-acl:
	rm -rf binary-acl

acl-depends.mk: maxima.system
	echo $(call MAKE_DEPENDS,"binary-acl/maxima.dxl",acl-depends.mk) | $(EXECUTEACL)

-include acl-depends.mk
endif

## SBCL ########################################################################
if SBCL

sbcllibdir = $(verpkglibdir)/binary-sbcl

EXTRA_SCRIPTS += $(SBCL_MAXIMA)

if SBCL_EXEC
SBCL_MAXIMA = binary-sbcl/maxima$(EXEEXT)
sbcllib_SCRIPTS = $(SBCL_MAXIMA)
sb_slad = (sb-ext:save-lisp-and-die "$@" :executable t)
else
SBCL_MAXIMA = binary-sbcl/maxima.core
sbcllib_DATA = $(SBCL_MAXIMA)
sb_slad = (sb-ext:save-lisp-and-die "$@")
endif

EXECUTESBCL = "$(SBCL_NAME)" --noinform --noprint --eval
BUILT_FILES += $(SBCL_MAXIMA)
CLEAN_TARGETS += clean-sbcl

COMPILE_SBCL = $(EXECUTESBCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) (sb-ext:quit))'
WRITE_SBCL = $(EXECUTESBCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) $(sb_slad) (sb-ext:quit))'
BUILD_SBCL = $(COMPILE_SBCL) && $(WRITE_SBCL)

sbcl: $(SBCL_MAXIMA)

$(SBCL_MAXIMA):
	$(MAKE) bd
	$(BUILD_SBCL)

clean-sbcl:
	rm -rf binary-sbcl numerical/binary-sbcl numerical/slatec/binary-sbcl

sbcl-depends.mk: maxima.system
	$(EXECUTESBCL) $(call MAKE_DEPENDS,"$(SBCL_MAXIMA)",sbcl-depends.mk,(sb-ext:quit))

-include sbcl-depends.mk

endif

## GCL #########################################################################
if GCL

gcllibdir = $(verpkglibdir)/binary-gcl

EXTRA_SCRIPTS += binary-gcl/maxima
gcllib_SCRIPTS = binary-gcl/maxima

EXECUTEGCL = $(GCL_NAME) -batch -eval
BUILT_FILES += binary-gcl/maxima
CLEAN_TARGETS += clean-gcl

sys_proc_dependency = sys-proclaim.lisp
gcl_depends_targets = (list "binary-gcl/maxima" "sys-proclaim.lisp")

sys-proclaim.lisp:
	rm -rf binary-gcl
	touch sys-proclaim.lisp
	$(MAKE) bd
	$(EXECUTEGCL) '(progn (load "generate-sys-proclaim.lisp"))'
	rm -rf binary-gcl

gcl: binary-gcl/maxima

binary-gcl/maxima: $(sys_proc_dependency)
	$(MAKE) bd
if GCL_ALT_LINK
	$(EXECUTEGCL) '(progn $(QUIET) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' && \
	$(EXECUTEGCL) '(let ((com (quote (progn (defvar compiler::*gazonk-prefix* "gazonk") (defun compiler::gazonk-name (&aux tem)(dotimes (i 1000) (unless (probe-file (setq tem (merge-pathnames (format nil "~d~d.lsp" compiler::*gazonk-prefix* i))))(return-from compiler::gazonk-name (pathname tem))))(error "1000 gazonk names used already!"))(let ((compiler::*gazonk-prefix* "maxima_gazonk")(compiler::*keep-gaz* t))$(LOADDEFSYSTEM)$(DEFSYSTEMLOAD))))))(let ((si::*collect-binary-modules* t))(eval com)(let ((compiler::*default-system-p* t))(dolist (l (directory "maxima_gazonk*.lsp")) (compile-file l) (delete-file l)))(compiler::link si::*binary-modules* "binary-gcl/maxima" (format nil "~S" com) "" nil)(dolist (l (directory "maxima_gazonk*.lsp")) (delete-file l))))'
else
	$(EXECUTEGCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' && \
	$(EXECUTEGCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (when (fboundp (quote si::sgc-on))(si::sgc-on t)) (si:save-system "binary-gcl/maxima"))'
endif

clean-gcl:
	rm -rf binary-gcl

gcl-depends.mk: maxima.system
	$(EXECUTEGCL) $(call MAKE_DEPENDS,$(gcl_depends_targets),gcl-depends.mk)

-include gcl-depends.mk

endif

## OPENMCL / CCL ###############################################################
if OPENMCL

openmcllibdir = $(verpkglibdir)/binary-openmcl
EXTRA_SCRIPTS += $(OPENMCL_MAXIMA)

if OPENMCL_EXEC
OPENMCL_MAXIMA = binary-openmcl/maxima$(EXEEXT)
openmcllib_SCRIPTS = $(OPENMCL_MAXIMA)
omcl_flag = :prepend-kernel t
else
OPENMCL_MAXIMA = binary-openmcl/maxima.image
openmcllib_DATA = $(OPENMCL_MAXIMA)
omcl_flag =
endif

EXECUTEOPENMCL = $(OPENMCL_NAME) -e
BUILT_FILES += $(OPENMCL_MAXIMA)
CLEAN_TARGETS += clean-openmcl

COMPILE_OMCL = $(EXECUTEOPENMCL) \
 '(progn (require :defsystem)  $(DEFSYSTEMCOMPILE) (ccl::quit))'
WRITE_OMCL = $(EXECUTEOPENMCL) \
 '(progn (require :defsystem) $(DEFSYSTEMLOAD) (ccl:save-application "$@") (ccl::quit))'
BUILD_OMCL = $(COMPILE_OMCL) && $(WRITE_OMCL)

openmcl: $(OPENMCL_MAXIMA)

$(OPENMCL_MAXIMA):
	$(BUILD_OMCL)
	$(EXECUTEOPENMCL) \
	  '(progn (require :defsystem)  $(DEFSYSTEMCOMPILE) (ccl::quit))' && \
	$(EXECUTEOPENMCL) \
	  '(progn (require :defsystem) $(DEFSYSTEMLOAD) (ccl:save-application "$@" $(opcl_flag)) (ccl::quit))'

clean-openmcl:
	rm -rf binary-openmcl

openmcl-depends.mk: maxima.system
	$(EXECUTEOPENMCL) $(call MAKE_DEPENDS,"$(OPENMCL_MAXIMA)",openmcl-depends.mk,(ccl:quit))

-include openmcl-depends.mk

endif

## CCL64 #######################################################################

if CCL64

ccl64libdir = $(verpkglibdir)/binary-ccl64
EXTRA_SCRIPTS += $(OPENMCL_MAXIMA)

if CCL64_EXEC
CCL64_MAXIMA = binary-ccl64/maxima$(EXEEXT)
ccl64lib_SCRIPTS = $(CCL64_MAXIMA)
omcl_flag = :prepend-kernel t
else
CCL64_MAXIMA = binary-ccl64/maxima.image
ccl64lib_DATA = $(CCL64_MAXIMA)
omcl_flag =
endif

EXECUTECCL64 = $(CCL64_NAME) -e
BUILT_FILES += $(CCL64_MAXIMA)
CLEAN_TARGETS += clean-ccl64

COMPILE_CCL64 = $(EXECUTECCL64) \
 '(progn (require :defsystem)  $(DEFSYSTEMCOMPILE) (ccl::quit))'
WRITE_CCL64 = $(EXECUTECCL64) \
 '(progn (require :defsystem) $(DEFSYSTEMLOAD) (ccl:save-application "$@") (ccl::quit))'
BUILD_CCL64 = $(COMPILE_CCL64) && $(WRITE_CCL64)

ccl64: $(CCL64_MAXIMA)

$(CCL64_MAXIMA):
	$(BUILD_CCL64)
	$(EXECUTECCL64) \
	  '(progn (require :defsystem)  $(DEFSYSTEMCOMPILE) (ccl::quit))' && \
	$(EXECUTECCL64) \
	  '(progn (require :defsystem) $(DEFSYSTEMLOAD) (ccl:save-application "$@" $(opcl_flag)) (ccl::quit))'

clean-ccl64:
	rm -rf binary-ccl64

ccl64-depends.mk: maxima.system
	$(EXECUTECCL64) $(call MAKE_DEPENDS,"$(CCL64_MAXIMA)",ccl64-depends.mk,(ccl:quit))

-include ccl64-depends.mk


endif

## ECL #########################################################################
if ECL

ecllibdir = $(verpkglibdir)/binary-ecl
EXTRA_SCRIPTS += binary-ecl/maxima
ecllib_SCRIPTS = binary-ecl/maxima

EXECUTEECL = $(ECL_NAME) -norc
BUILT_FILES += binary-ecl/maxima
CLEAN_TARGETS += clean-ecl

ecl:binary-ecl/maxima

binary-ecl/maxima:
	$(MAKE) bd
	$(EXECUTEECL) \
           -eval '(progn $(QUIET) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) (build-maxima-lib))' \
           -eval '(ext:quit)'

clean-ecl:
	rm -rf binary-ecl libmaxima.a

ecl-depends.mk: maxima.system
	$(EXECUTEECL) -eval $(call MAKE_DEPENDS,"binary-ecl/maxima",ecl-depends.mk,(quit))

-include ecl-depends.mk

endif
