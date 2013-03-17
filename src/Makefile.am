include $(top_srcdir)/common.mk

if WIN32
bin_SCRIPTS = maxima maxima-command.ico maxima.bat set_lang.vbs
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
  sharefiles.mk sys-proclaim.lisp

## A debugging tool. If you're trying to understand the value of the
## variable $(foo) when make runs, call "make echo_foo" and it will be
## printed.
echo_%:
	@echo "$(subst echo_,,$@)=$($(subst echo_,,$@))"
	@echo "origin $(subst echo_,,$@) returns $(origin $(subst echo_,,$@))"

## Create the list of files that will be installed from share. The
## make_share_list script outputs to a temporary file and only
## overwrites sharefiles.mk if there is a change. As such,
## share/Makefile is only updated (and share rebuilt) when something
## changes.
##
## TODO: Why is this in src/ not in share/ ?
.PHONY: sharefiles.mk
sharefiles.mk:
	../admin/make_share_list

## "all" depends on sharefiles.mk and whatever files we choose to
## build. These are listed in BUILT_FILES, which is populated in the
## implementation-dependent sections below.
BUILT_FILES =
all-local: sharefiles.mk $(BUILT_FILES)

## The "clean" rule always just deletes some files and they are
## specified in the conditional sections by adding
## implementation-specific targets to the list CLEAN_TARGETS.
CLEAN_TARGETS =
clean-local: $(CLEAN_TARGETS)

## A rule to build binary directories of the form binary-<lispname>
## and subdirectories ./numerical and ./numerical/slatec
binary_subdirs = / /numerical /numerical/slatec
bd_%:
	$(MKDIR_P) $(addprefix $(subst bd_,binary-,$@),$(binary_subdirs))

## Some hunks of lisp that get used by more than one implementation
LOADDEFSYSTEM = (load "$(top_srcdir)/lisp-utils/defsystem.lisp")
LOADMAKEDEPENDS = (load "$(top_srcdir)/lisp-utils/make-depends.lisp")
DS_OOS = funcall (intern (symbol-name :operate-on-system) :mk) "maxima"
DEFSYSTEMCOMPILE = ($(DS_OOS) :compile :verbose t)
DEFSYSTEMLOAD = ($(DS_OOS) :load :verbose t)
DEFSYSTEMTESTLOAD = ($(DS_OOS) :load :verbose t :test t)

## A function that takes: $(1) = the target name; $(2) =
## <foo>-depends.mk; $(3) = any postscript that should be added in the
## progn. It outputs lisp code that, when run, should create the
## dependency Makefile fragments.
MAKE_DEPENDS = '(progn $(LOADDEFSYSTEM) $(LOADMAKEDEPENDS) (funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "$(1)" "$(2)") $(3))'

## CLISP #######################################################################
if CLISP

EXTRA_SCRIPTS = $(CLISP_MAXIMA)

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
install-exec-local: install-clisp-copy
install-clisp-copy:
	$(mkinstalldirs) $(clisplibdir)
	$(INSTALL_PROGRAM) @CLISP_RUNTIME_PATH@ "$(clisplibdir)/@CLISP_RUNTIME@"
uninstall-hook: uninstall-clisp-copy
uninstall-clisp-copy:
	rm -f "$(clisplibdir)/@CLISP_RUNTIME@"
endif

EXECUTECLISP = $(CLISP_NAME) -norc -q -x
clisplibdir = $(verpkglibdir)/binary-clisp

BUILT_FILES += $(CLISP_MAXIMA)
CLEAN_TARGETS += clean-clisp

clisp: $(CLISP_MAXIMA)

$(CLISP_MAXIMA):
	make bd_clisp
	$(EXECUTECLISP) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' && \
	$(EXECUTECLISP) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:saveinitmem "$@" :init-function (function cl-user::run) $(clispexeflag)))'

clean-clisp:
	rm -rf binary-clisp

clisp-depends.mk: maxima.system
	$(EXECUTECLISP) $(call MAKE_DEPENDS,$(CLISP_MAXIMA),clisp-depends.mk)

-include clisp-depends.mk
endif CLISP

if CMUCL_EXEC
CMU_MAXIMA = binary-cmucl/maxima
CMU_BUILD = (echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:save-lisp "binary-cmucl/maxima" :executable t :init-function '\''cl-user::run))' | $(EXECUTECMUCL))
CMU_INST_BIN = true
CMU_INST_MAXIMA = $(INSTALL_PROGRAM) $(CMU_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(CMU_MAXIMA)"
else
CMU_MAXIMA = binary-cmucl/maxima.core
CMU_BUILD = (echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:save-lisp "binary-cmucl/maxima.core"))' | $(EXECUTECMUCL))
CMU_INST_BIN = $(INSTALL_PROGRAM) lisp "$(DESTDIR)$(verpkglibdir)/binary-cmucl/lisp"
CMU_INST_MAXIMA = $(INSTALL_DATA) $(CMU_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(CMU_MAXIMA)"
endif

if CMUCL
EXECUTECMUCL = $(CMUCL_NAME) -noinit -batch
# Newer versions of CMUCL have an INTL package that is compatible with
# maxima's.  We just bind intl::*default-domain* here so that when we
# compile the files, we will get appropriate translations.  (Otherwise
# we have to put (intl:textdomain "maxima") in each Lisp file.
INITINTL = (when (find-package "INTL") (set (find-symbol "*DEFAULT-DOMAIN*" "INTL") "maxima"))
BUILT_FILES += $(CMU_MAXIMA)
install-exec-local: install-cmucl
uninstall-hook: uninstall-cmucl
CLEAN_TARGETS += clean-cmucl

cmucl: $(CMU_MAXIMA)

$(CMU_MAXIMA):
	make bd_cmucl
	(echo '(progn $(INITINTL) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' | $(EXECUTECMUCL)) && \
	$(CMU_BUILD)

install-cmucl:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-cmucl"
	$(CMU_INST_MAXIMA)
	$(CMU_INST_BIN)

uninstall-cmucl:
	rm -f "$(DESTDIR)$(verpkglibdir)/$(CMU_MAXIMA)"
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-cmucl/@CMUCL_RUNTIME@"

clean-cmucl:
	rm -rf binary-cmucl

cmucl-depends.mk: maxima.system
	echo $(call MAKE_DEPENDS,$(CMU_MAXIMA),cmucl-depends.mk) | $(EXECUTECMUCL)

-include cmucl-depends.mk
endif


if SCL
EXECUTESCL = $(SCL_NAME) -noinit -batch
BUILT_FILES += binary-scl/maxima.core
install-exec-local: install-scl
uninstall-hook: uninstall-scl
CLEAN_TARGETS += clean-scl

scl: binary-scl/maxima.core

binary-scl/maxima.core:
	make bd_scl
	(echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' | $(EXECUTESCL)) && \
	(echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:save-lisp "binary-scl/maxima.core"))' | $(EXECUTESCL))

install-scl:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-scl"
	$(INSTALL_DATA) binary-scl/maxima.core "$(DESTDIR)$(verpkglibdir)/binary-scl/maxima.core"
	$(INSTALL_PROGRAM) @SCL_RUNTIME_PATH@ "$(DESTDIR)$(verpkglibdir)/binary-scl/@SCL_RUNTIME@"

uninstall-scl:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-scl/maxima.core"
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-scl/@SCL_RUNTIME@"

clean-scl:
	rm -rf binary-scl

scl-depends.mk: maxima.system
	echo $(call MAKE_DEPENDS,binary-scl/maxima.core,scl-depends.mk) | $(EXECUTESCL)

-include scl-depends.mk
endif


if ACL
EXECUTEACL = $(ACL_NAME) -batch
BUILT_FILES += binary-acl/maxima.dxl
install-exec-local: install-acl
uninstall-hook: uninstall-acl
CLEAN_TARGETS += clean-acl

acl:binary-acl/maxima.dxl

binary-acl/maxima.dxl:
	make bd_acl
	(echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' | $(EXECUTEACL)) && \
	(echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (excl:dumplisp :name "binary-acl/maxima.dxl"))' | $(EXECUTEACL))

install-acl:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-acl"
	$(INSTALL_DATA) binary-acl/maxima.dxl "$(DESTDIR)$(verpkglibdir)/binary-acl/maxima.dxl"

uninstall-acl:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-acl/maxima.dxl"

clean-acl:
	rm -rf binary-acl

acl-depends.mk: maxima.system
	echo $(call MAKE_DEPENDS,binary-acl/maxima.dxl,acl-depends.mk) | $(EXECUTEACL)

-include acl-depends.mk
endif

if SBCL_EXEC
SBCL_MAXIMA = binary-sbcl/maxima$(EXEEXT)
SBCL_INST_BIN = true
SBCL_INST_MAXIMA = $(INSTALL_PROGRAM) $(SBCL_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(SBCL_MAXIMA)"
else
SBCL_MAXIMA = binary-sbcl/maxima.core
## FIXME:  This rule can't work.  Need to replace "lisp" with the 
## sbcl_runtime, after defining that in configure.in
## See install-clisp or install-scl.  DB 2012-06-07
SBCL_INST_BIN = $(INSTALL_PROGRAM) lisp "$(DESTDIR)$(verpkglibdir)/sbcl/lisp"
SBCL_INST_MAXIMA = $(INSTALL_DATA) $(SBCL_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(SBCL_MAXIMA)"
endif

if SBCL
EXECUTESBCL = "$(SBCL_NAME)" --noinform --noprint --eval
BUILT_FILES += $(SBCL_MAXIMA)
install-exec-local: install-sbcl
uninstall-hook: uninstall-sbcl
CLEAN_TARGETS += clean-sbcl

sbcl:binary-sbcl/maxima.core

binary-sbcl/maxima.core:
	make bd_sbcl
	$(EXECUTESBCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) (sb-ext:quit))' && \
	$(EXECUTESBCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (sb-ext:save-lisp-and-die "$@") (sb-ext:quit))'

binary-sbcl/maxima$(EXEEXT):
	make bd_sbcl
	$(EXECUTESBCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) (sb-ext:quit))' && \
	$(EXECUTESBCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (sb-ext:save-lisp-and-die "$@" :executable t) (sb-ext:quit))'

install-sbcl:
	$(mkinstalldirs) $(DESTDIR)$(verpkglibdir)/binary-sbcl
	$(SBCL_INST_MAXIMA)

uninstall-sbcl:
	rm -f $(DESTDIR)$(verpkglibdir)/$(SBCL_MAXIMA)

clean-sbcl:
	rm -rf binary-sbcl
	rm -f numerical/binary-sbcl numerical/slatec/binary-sbcl

sbcl-depends.mk: maxima.system
	$(EXECUTESBCL) $(call MAKE_DEPENDS,$(SBCL_MAXIMA),sbcl-depends.mk,(sb-ext:quit))

-include sbcl-depends.mk
endif

if GCL
EXECUTEGCL = $(GCL_NAME) -batch -eval
BUILT_FILES += binary-gcl/maxima
install-exec-local: install-gcl
uninstall-hook: uninstall-gcl
CLEAN_TARGETS += clean-gcl

sys-proclaim.lisp:
	rm -rf binary-gcl
	touch sys-proclaim.lisp
	make bd_gcl
	$(EXECUTEGCL) '(progn (load "generate-sys-proclaim.lisp"))'
	rm -rf binary-gcl

gcl: binary-gcl/maxima

binary-gcl/maxima: sys-proclaim.lisp
	make bd_gcl
if GCL_ALT_LINK
	$(EXECUTEGCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' && \
	$(EXECUTEGCL) '(let ((com (quote (progn (defvar compiler::*gazonk-prefix* "gazonk") (defun compiler::gazonk-name (&aux tem)(dotimes (i 1000) (unless (probe-file (setq tem (merge-pathnames (format nil "~d~d.lsp" compiler::*gazonk-prefix* i))))(return-from compiler::gazonk-name (pathname tem))))(error "1000 gazonk names used already!"))(let ((compiler::*gazonk-prefix* "maxima_gazonk")(compiler::*keep-gaz* t))$(LOADDEFSYSTEM)$(DEFSYSTEMLOAD))))))(let ((si::*collect-binary-modules* t))(eval com)(let ((compiler::*default-system-p* t))(dolist (l (directory "maxima_gazonk*.lsp")) (compile-file l) (delete-file l)))(compiler::link si::*binary-modules* "binary-gcl/maxima" (format nil "~S" com) "" nil)(dolist (l (directory "maxima_gazonk*.lsp")) (delete-file l))))'
else
if WIN32
	$(EXECUTEGCL) '(progn (compiler::emit-fn t))' -eval '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) $(LOADMAKEPROCLAIM) (compiler::make-all-proclaims "*/*.fn" "*/*/*/*.fn"))' && \
	$(EXECUTEGCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (when (fboundp (quote si::sgc-on))(si::sgc-on t)) (si:save-system "binary-gcl/maxima"))'
else
	$(EXECUTEGCL) '(progn $(LOADDEFSYSTEM) (compiler::emit-fn t) $(DEFSYSTEMCOMPILE) $(LOADMAKEPROCLAIM) (compiler::make-all-proclaims "*/*.fn" "*/*/*/*.fn"))' && \
	$(EXECUTEGCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (when (fboundp (quote si::sgc-on))(si::sgc-on t)) (si:save-system "binary-gcl/maxima"))'
endif	
endif

install-gcl:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-gcl"
	$(INSTALL_PROGRAM) binary-gcl/maxima "$(DESTDIR)$(verpkglibdir)/binary-gcl/maxima"

uninstall-gcl:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-gcl/maxima"
clean-gcl:
	rm -rf binary-gcl

gcl-depends.mk: maxima.system
	$(EXECUTEGCL) $(call MAKE_DEPENDS,binary-gcl/maxima,gcl-depends.mk)

-include gcl-depends.mk
endif

if OPENMCL_EXEC
OPENMCL_MAXIMA = binary-openmcl/maxima$(EXEEXT)
OPENMCL_INST_BIN = true
OPENMCL_INST_MAXIMA = $(INSTALL_PROGRAM) $(OPENMCL_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(OPENMCL_MAXIMA)"
else
OPENMCL_MAXIMA = binary-openmcl/maxima.image
## FIXME:  This rule can't work.  Need to replace "lisp" with the 
## openmcl_runtime, after defining that in configure.in
## See install-clisp or install-scl.  DB 2012-06-07
OPENMCL_INST_BIN = $(INSTALL_PROGRAM) lisp "$(DESTDIR)$(verpkglibdir)/binary-opencl/lisp"
OPENMCL_INST_MAXIMA = $(INSTALL_DATA) $(OPENMCL_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(OPENMCL_MAXIMA)"
endif

if OPENMCL
EXECUTEOPENMCL = $(OPENMCL_NAME) -e
BUILT_FILES += $(OPENMCL_MAXIMA)
install-exec-local: install-openmcl
uninstall-hook: uninstall-openmcl
CLEAN_TARGETS += clean-openmcl

openmcl:$(OPENMCL_MAXIMA)

binary-openmcl/maxima.image:
	$(EXECUTEOPENMCL) \
	  '(progn (require :defsystem)  $(DEFSYSTEMCOMPILE) (ccl::quit))' && \
	$(EXECUTEOPENMCL) \
	  '(progn (require :defsystem) $(DEFSYSTEMLOAD) (ccl:save-application "$@") (ccl::quit))'

binary-openmcl/maxima$(EXEEXT):
	$(EXECUTEOPENMCL) \
	  '(progn (require :defsystem)  $(DEFSYSTEMCOMPILE) (ccl::quit))' && \
	$(EXECUTEOPENMCL) \
	  '(progn (require :defsystem) $(DEFSYSTEMLOAD) (ccl:save-application "$@" :prepend-kernel t) (ccl::quit))'

install-openmcl: $(OPENMCL_MAXIMA)
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-openmcl"
	$(OPENMCL_INST_MAXIMA)

uninstall-openmcl:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-openmcl/$(OPENMCL_MAXIMA)"

clean-openmcl:
	rm -rf binary-openmcl

openmcl-depends.mk: maxima.system
	$(EXECUTEOPENMCL) $(call MAKE_DEPENDS,$(OPENMCL_MAXIMA),openmcl-depends.mk,(ccl:quit))

-include openmcl-depends.mk
endif

if ECL
EXECUTEECL = $(ECL_NAME) -norc
BUILT_FILES += binary-ecl/maxima
install-exec-local: install-ecl
uninstall-hook: uninstall-ecl
CLEAN_TARGETS += clean-ecl

ecl:binary-ecl/maxima

binary-ecl/maxima:
	make bd_ecl
	$(EXECUTEECL) -eval '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) (build-maxima-lib))' -eval '(ext:quit)'

install-ecl:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-ecl"
	$(INSTALL_PROGRAM) binary-ecl/maxima "$(DESTDIR)$(verpkglibdir)/binary-ecl/maxima"

uninstall-ecl:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-ecl/maxima"

clean-ecl:
	rm -rf binary-ecl libmaxima.a

ecl-depends.mk: maxima.system
	$(EXECUTEECL) -eval $(call MAKE_DEPENDS,binary-ecl/maxima,ecl-depends.mk,(quit))

-include ecl-depends.mk
endif
