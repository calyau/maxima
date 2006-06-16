include $(top_srcdir)/common.mk

SUBDIRS = msgs

if WIN32
TCLKITSH = tclkitsh.exe
TCLKITDIR = ../../programs/star
SDXDIR = $(TCLKITDIR)
WIN32_EXE = omplotdata.exe
endif

verpkglibexec_SCRIPTS = omplotdata mgnuplot $(WIN32_EXE)

EXTRA_DIST = header.tcl footer.tcl paths.tcl

OMPLOTDATASOURCES = \
	header.tcl \
	../interfaces/xmaxima/autoconf-variables.tcl \
	paths.tcl \
	../interfaces/xmaxima/Tkmaxima/Constants.tcl \
	../interfaces/xmaxima/Tkmaxima/Macros.tcl \
	../interfaces/xmaxima/Tkmaxima/Plotting.tcl \
	../interfaces/xmaxima/Tkmaxima/Private.tcl \
	../interfaces/xmaxima/Tkmaxima/Getopt.tcl \
	../interfaces/xmaxima/Tkmaxima/Parse.tcl \
	../interfaces/xmaxima/Tkmaxima/Textinsert.tcl \
	../interfaces/xmaxima/Tkmaxima/Printops.tcl \
	../interfaces/xmaxima/Tkmaxima/Push.tcl \
	../interfaces/xmaxima/Tkmaxima/Plotconf.tcl \
	../interfaces/xmaxima/Tkmaxima/Adams.tcl \
	../interfaces/xmaxima/Tkmaxima/Rk.tcl \
	../interfaces/xmaxima/Tkmaxima/Plotdf.tcl \
	../interfaces/xmaxima/Tkmaxima/Plot2d.tcl \
	../interfaces/xmaxima/Tkmaxima/Matrix.tcl \
	../interfaces/xmaxima/Tkmaxima/Plot3d.tcl \
	../interfaces/xmaxima/Tkmaxima/NPlot3d.tcl \
	footer.tcl 

omplotdata: $(OMPLOTDATASOURCES)
	rm -f omplotdata && \
		cat $(OMPLOTDATASOURCES) > omplotdata && \
		chmod +x omplotdata

omplotdata.tcl: omplotdata
	cp omplotdata omplotdata.tcl

omplotdata.exe: omplotdata.tcl
	rm -rf omplotdata.vfs
	$(TCLKITDIR)/$(TCLKITSH) $(SDXDIR)/sdx.kit qwrap omplotdata.tcl && \
	$(TCLKITDIR)/$(TCLKITSH) $(SDXDIR)/sdx.kit unwrap omplotdata.kit && \
	$(TCLKITDIR)/$(TCLKITSH) $(SDXDIR)/sdx.kit wrap omplotdata.exe \
		-runtime $(TCLKITDIR)/tclkit.exe omplotdata.kit


