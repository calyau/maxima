SUBDIRS = src lisp-utils tests doc interfaces share demo plotting
EXTRA_DIST = common.mk maxima-local xmaxima-local README.lisps maxima.iss.in README.rpms INSTALL.win32

include common.mk

extradocinstall:
	test -d "$(docdir)" || mkdir -p "$(docdir)"
	for file in AUTHORS COPYING INSTALL README README.lisps ; \
            do \
            $(INSTALL_DATA) $$file "$(docdir)" ; \
            done

$(distdir).tar.gz: Makefile.am
	$(MAKE) dist

GCCPREFIX=/mingw
gcccopy:
	test -d "$(prefix)/bin" || mkdir -p "$(prefix)/bin"
	test -d "$(prefix)/include" || mkdir -p "$(prefix)/include"
	test -d "$(prefix)/include/sys" || mkdir -p "$(prefix)/include/sys"
	test -d "$(prefix)/lib/gcc-lib/mingw32/3.2" || mkdir -p "$(prefix)/lib/gcc-lib/mingw32/3.2"
	test -d "$(prefix)/lib/gcc-lib/mingw32/3.2/include" || mkdir -p "$(prefix)/lib/gcc-lib/mingw32/3.2/include"
	cp $(GCCPREFIX)/bin/gcc.exe $(prefix)/bin
	cp $(GCCPREFIX)/bin/as.exe $(prefix)/bin
	cp $(GCCPREFIX)/bin/mingwm10.dll $(prefix)/bin
	cp $(GCCPREFIX)/include/stdio.h $(prefix)/include
	cp $(GCCPREFIX)/include/varargs.h $(prefix)/include
	cp $(GCCPREFIX)/include/setjmp.h $(prefix)/include
	cp $(GCCPREFIX)/include/stddef.h $(prefix)/include
	cp $(GCCPREFIX)/include/_mingw.h $(prefix)/include
	cp $(GCCPREFIX)/include/math.h $(prefix)/include
	cp $(GCCPREFIX)/include/unistd.h $(prefix)/include
	cp $(GCCPREFIX)/include/io.h $(prefix)/include
	cp $(GCCPREFIX)/include/process.h $(prefix)/include
	cp $(GCCPREFIX)/include/sys/*.h $(prefix)/include/sys
	cp $(GCCPREFIX)/lib/gcc-lib/mingw32/3.2/cc1.exe $(prefix)/lib/gcc-lib/mingw32/3.2
	cp $(GCCPREFIX)/lib/gcc-lib/mingw32/3.2/cpp0.exe $(prefix)/lib/gcc-lib/mingw32/3.2
	cp $(GCCPREFIX)/lib/gcc-lib/mingw32/3.2/specs  $(prefix)/lib/gcc-lib/mingw32/3.2
	cp $(GCCPREFIX)/lib/gcc-lib/mingw32/3.2/include/*.h  $(prefix)/lib/gcc-lib/mingw32/3.2/include

iss: extradocinstall gcccopy
	$(MAKE) -C interfaces/xmaxima/win32 install

# Build the Maxima rpms. Optionally, use
#     make rpm SUDO=sudo
# to use sudo to wrap the commands. The user must have sudo permissions
# for rpm and cp.
rpm: $(distdir).tar.gz
	$(SUDO) cp $(distdir).tar.gz /usr/src/redhat/SOURCES && \
	$(SUDO) rpm -ba maxima.spec

