AUTOMAKE_OPTIONS = no-dependencies
CC = gcc # to supress automake warning

include $(top_srcdir)/common.mk

verpkglibexec_SCRIPTS = mgnuplot
verpkglibexec_PROGRAMS =

##### Windows only #####
if XMAXIMA_EXE
verpkglibexec_PROGRAMS += mgnuplot$(EXEEXT)

mgnuplot.exe: mgnuplot.vfs mgnuplot.kit
	$(TCLKITSH) $(SDXKIT) wrap $@ -runtime $(TCLKIT_RUNTIME) mgnuplot.kit
endif

mgnuplot.kit: mgnuplot
	$(TCLKITSH) $(SDXKIT) qwrap  $<

mgnuplot.vfs: mgnuplot.kit
	-rm -rf $@
	$(TCLKITSH) $(SDXKIT) unwrap $<

##########################

CLEANFILES = mgnuplot.kit mgnuplot.exe

clean-local:
	-rm -rf mgnuplot.vfs
