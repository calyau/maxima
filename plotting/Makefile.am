include $(top_srcdir)/common.mk

SUBDIRS = msgs

verpkglibexec_SCRIPTS = omplotdata mgnuplot

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
