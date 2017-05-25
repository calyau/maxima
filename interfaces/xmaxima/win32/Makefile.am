# -*-mode: makefile; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: $
#
AUTOMAKE_OPTIONS = no-dependencies
CC = gcc # to suppress automake warning

include $(top_srcdir)/common.mk

genericdir = $(prefix)
genericdirDATA = readme_en.txt readme_ru.txt readme_fr.txt \
  readme_de.txt readme_pt.txt readme_pt_BR.txt readme_it.txt readme_es.txt

bin_PROGRAMS =
if XMAXIMA_EXE
bin_PROGRAMS += xmaxima$(EXEEXT) winkill$(EXEEXT)
else
if WINKILL_EXE
bin_PROGRAMS += winkill$(EXEEXT)
endif
endif

bin_SCRIPTS =
if XMAXIMA_EXE
bin_SCRIPTS += winkill_lib.dll win_signals.lisp
else
if WINKILL_EXE
bin_SCRIPTS += winkill_lib.dll win_signals.lisp
endif
endif

xmaxima_SOURCES =

../xmaxima:; $(MAKE) -C .. xmaxima

xmaxima.tcl: ../xmaxima; cp ../xmaxima xmaxima.tcl

xmaxima$(EXEEXT): xmaxima.tcl
	-rm -rf xmaxima.vfs img.vfs
	$(TCLKITSH) $(SDXKIT) qwrap xmaxima.tcl && \
	$(TCLKITSH) $(SDXKIT) unwrap xmaxima.kit && \
	$(TCLKITSH) $(SDXKIT) unwrap $(IMGKIT) && \
	cp -r img.vfs/lib/Img xmaxima.vfs/lib && \
	$(TCLKITSH) $(SDXKIT) wrap xmaxima.exe \
	-runtime $(TCLKIT_RUNTIME) xmaxima.kit	

winkill$(EXEEXT): winkill.c winkill_lib.dll
	gcc -Wall -o $@ winkill.c

winkill_lib.dll: winkill_lib.c
	gcc -Wall  -shared winkill_lib.c -o winkill_lib.dll

InfoBefore = InfoBefore.txt InfoBefore_ru.txt InfoBefore_fr.txt \
  InfoBefore_de.txt InfoBefore_pt.txt InfoBefore_pt_BR.txt \
  InfoBefore_it.txt InfoBefore_es.txt
EXTRA_DIST = ChangeLog $(genericdirDATA) $(InfoBefore) winkill.c winkill_lib.c win_signals.lisp

CLEANFILES = 
if XMAXIMA_EXE
CLEANFILES += tclwinkill.dll xmaxima.tcl xmaxima.kit 
endif
if WINKILL_EXE
CLEANFILES += winkill$(EXEEXT) winkill_lib.dll
endif

clean-local:; -rm -rf  xmaxima.vfs img.vfs

FORCE:
