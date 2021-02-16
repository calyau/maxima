# On debian-based systems this makes us use the right gcl version out-of-the box.
# On other systems it doesn't hurt.
export GCL_ANSI=t

if ENABLE_GETTEXT
   LOCALE_DIR = locale
endif

SUBDIRS = admin crosscompile-windows src lisp-utils tests doc interfaces share demo plotting desktopintegration $(LOCALE_DIR)

EXTRA_DIST = \
  common.mk maxima.iss.in \
  README-lisps.md README.rpms README.i18n README.external README.developers-howto \
  INSTALL.win32 INSTALL.lisp INSTALL.git INSTALL.win32nogcl \
  configure.lisp  maxima-local.in xmaxima-local.in \
  macosx/Makefile macosx/Readme.md macosx/maxima.sh macosx/script \
  macosx/xmaxima.sh macosx/appIcon.icns macosx/How_to_install.rtf \
  changelogs ChangeLog LICENSES/getopt-license.txt\
  snap/snapcraft.yaml

include common.mk

extradocinstall:
	test -d "$(DESTDIR)$(docdir)" || mkdir -p "$(DESTDIR)$(docdir)"
	for file in AUTHORS COPYING INSTALL README README-lisps.md ; \
            do \
            $(INSTALL_DATA) $$file "$(DESTDIR)$(docdir)" ; \
            done

$(distdir).tar.gz: Makefile.am
	$(MAKE) dist

# windows installer rules
if WIN32

GNUPLOTDIR = "/c/programs/gnuplot"
WXMAXIMADIR = "/c/programs/wxMaxima"

# Install the gnuplot binary files
gnuplot: 
	mkdir -p $(prefix)/gnuplot
	cp -rf $(GNUPLOTDIR) $(prefix)/

# Install wxMaxima
wxmaxima:
	test -d "$(prefix)/bin" || mkdir -p "$(prefix)/bin"
	cp -rf $(WXMAXIMADIR) $(prefix)/wxMaxima

cclcopy:
	test -d "$(prefix)/bin" || mkdir -p "$(prefix)/bin"
	cp "$(OPENMCL_NAME)" "$(prefix)/bin/"

sbclcopy:
	test -d "$(prefix)/bin" || mkdir -p "$(prefix)/bin"
	cp "$(SBCL_NAME)" "$(prefix)/bin/"

RUNTIMECOPY = 
# Install files for lisp runtime support
if GCL
RUNTIMECOPY += gcccopy
endif

if OPENMCL
RUNTIMECOPY += cclcopy
endif

if SBCL
RUNTIMECOPY += sbclcopy
endif

# Install files for lisp runtime support
win32-lisp-runtime: $(RUNTIMECOPY)

MINGW=/mingw
if GCC331
GCCPREFIX=/usr/local
# Old gcc/mingw/msys install
gcccopy:
	test -d "$(prefix)/bin" || mkdir -p "$(prefix)/bin"
	test -d "$(prefix)/include" || mkdir -p "$(prefix)/include"
	test -d "$(prefix)/include/sys" || mkdir -p "$(prefix)/include/sys"
	test -d "$(prefix)/lib/gcc-lib/mingw32/$(GCCVER)/include" \
		|| mkdir -p "$(prefix)/lib/gcc-lib/mingw32/$(GCCVER)/include"
	cp $(GCCPREFIX)/bin/gcc.exe $(prefix)/bin
	cp $(MINGW)/bin/mingwm10.dll $(prefix)/bin
	cp $(MINGW)/bin/tclpip84.dll $(prefix)/bin
	cp $(MINGW)/include/stdio.h $(prefix)/include
	cp $(MINGW)/include/stdlib.h $(prefix)/include
	cp $(MINGW)/include/setjmp.h $(prefix)/include
	cp $(MINGW)/include/_mingw.h $(prefix)/include
	cp $(MINGW)/include/math.h $(prefix)/include
	cp $(MINGW)/include/unistd.h $(prefix)/include
	cp $(MINGW)/include/io.h $(prefix)/include
	cp $(MINGW)/include/process.h $(prefix)/include
	cp $(MINGW)/include/getopt.h $(prefix)/include
	cp $(MINGW)/include/stdint.h $(prefix)/include
	cp $(MINGW)/include/sys/*.h $(prefix)/include/sys
	cp $(MINGW)/include/varargs.h $(prefix)/include
	cp $(MINGW)/include/stddef.h $(prefix)/include
	cp $(GCCPREFIX)/lib/gcc-lib/mingw32/$(GCCVER)/cc1.exe \
		$(prefix)/lib/gcc-lib/mingw32/$(GCCVER)
	cp $(MINGW)/bin/as.exe \
		$(prefix)/lib/gcc-lib/mingw32/$(GCCVER)
	cp $(GCCPREFIX)/lib/gcc-lib/mingw32/$(GCCVER)/specs  \
		$(prefix)/lib/gcc-lib/mingw32/$(GCCVER)
	cp $(GCCPREFIX)/lib/gcc-lib/mingw32/$(GCCVER)/include/*.h  \
		$(prefix)/lib/gcc-lib/mingw32/$(GCCVER)/include

else # GCC331

GCCPREFIX=$(MINGW)
# New (November 2013) gcc/mingw/msys install using gcc-4.8.1
gcccopy:
	test -d "$(prefix)/bin" || mkdir -p "$(prefix)/bin"
	test -d "$(prefix)/include" || mkdir -p "$(prefix)/include"
	test -d "$(prefix)/include/sys" || mkdir -p "$(prefix)/include/sys"
	test -d "$(prefix)/lib/gcc/mingw32/$(GCCVER)/include" \
		|| mkdir -p "$(prefix)/lib/gcc/mingw32/$(GCCVER)/include"
	test -d "$(prefix)/libexec/gcc/mingw32/$(GCCVER)" \
		|| mkdir -p "$(prefix)/libexec/gcc/mingw32/$(GCCVER)"
	cp $(GCCPREFIX)/bin/gcc.exe $(prefix)/bin
	cp $(MINGW)/bin/mingwm10.dll $(prefix)/bin
	cp $(GCCPREFIX)/bin/libgmp-10.dll $(prefix)/bin
	cp $(GCCPREFIX)/bin/libmpc-3.dll $(prefix)/bin
	cp $(GCCPREFIX)/bin/libmpfr-4.dll $(prefix)/bin
	cp /c/tcl/bin/tclpip85.dll $(prefix)/bin
	cp $(MINGW)/include/stdio.h $(prefix)/include
	cp $(MINGW)/include/stdlib.h $(prefix)/include
	cp $(MINGW)/include/setjmp.h $(prefix)/include
	cp $(MINGW)/include/_mingw.h $(prefix)/include
	cp $(MINGW)/include/math.h $(prefix)/include
	cp $(MINGW)/include/unistd.h $(prefix)/include
	cp $(MINGW)/include/io.h $(prefix)/include
	cp $(MINGW)/include/process.h $(prefix)/include
	cp $(MINGW)/include/getopt.h $(prefix)/include
	cp $(MINGW)/include/stdint.h $(prefix)/include
	cp $(MINGW)/include/sys/*.h $(prefix)/include/sys
	cp $(GCCPREFIX)/libexec/gcc/mingw32/$(GCCVER)/cc1.exe \
		$(prefix)/libexec/gcc/mingw32/$(GCCVER)
	cp $(GCCPREFIX)/libexec/gcc/mingw32/$(GCCVER)/liblto_plugin-0.dll \
		$(prefix)/libexec/gcc/mingw32/$(GCCVER)
	cp $(MINGW)/bin/as.exe \
		$(prefix)/libexec/gcc
	cp $(GCCPREFIX)/lib/gcc/mingw32/$(GCCVER)/include/*.h  \
		$(prefix)/lib/gcc/mingw32/$(GCCVER)/include

endif # GCC331

iss: win32-lisp-runtime wxmaxima gnuplot extradocinstall

endif # WIN32

rpm: maxima.spec dist-gzip
	rpmbuild -ba $(top_srcdir)/maxima.spec --define "_sourcedir $(abs_top_builddir)" --define "_rpmdir $(abs_top_builddir)"
	rpmbuild -bb $(top_srcdir)/maxima.spec --define "_sourcedir $(abs_top_builddir)" --define "_rpmdir $(abs_top_builddir)"

clean-local:
	rm -rf bin
	rm -rf binary

check-local: all
