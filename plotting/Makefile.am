include $(top_srcdir)/common.mk

verpkglibexec_SCRIPTS = mgnuplot

##### Windows only ####
if XMAXIMA_EXE
verpkglibexec_SCRIPTS += mgnuplot.exe
endif

mgnuplot.exe: mgnuplot.vfs mgnuplot.kit
	$(TCLKITSH) $(SDXKIT) wrap $@ -runtime $(TCLKIT_RUNTIME) mgnuplot.kit

mgnuplot.kit: mgnuplot
	$(TCLKITSH) $(SDXKIT) qwrap  $<

mgnuplot.vfs: mgnuplot.kit
	-rm -rf $@
	$(TCLKITSH) $(SDXKIT) unwrap $<

CLEANFILES = mgnuplot.kit mgnuplot.exe

clean-local:
	-rm -rf mgnuplot.vfs
