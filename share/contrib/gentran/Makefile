
# this compiles all objects

VOBJDIR=
CMPLR=nliszt
OBJS = global.o intrfc.o lspfor.o \
opt.o output.o parser.o pre.o segmnt.o templt.o vaxlsp.o \
init.o lsprat.o lspc.o gtload.o
OFILES= convmac.l gtfix.l

.SUFFIXES: .lisp
.l.o:
	${CMPLR} $< > '#errs'

gentran: $(OBJS)
	@echo gentran compiled

cpobj:
	cp $(OBJS) $(OFILES) $(VOBJDIR)
