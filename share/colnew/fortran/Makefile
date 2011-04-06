# All of the fortran sources, except compat.f, which is needed by f2cl
# to do the conversion correctly.

SRCS = approx.f \
	appsln.f \
	colnew.f \
	consts.f \
	contrl.f \
	daxpy.f \
	ddot.f \
	dgefa.f \
	dgesl.f \
	dmzsol.f \
	dscal.f \
	errchk.f \
	factrb.f \
	fcblok.f \
	gblock.f \
	gderiv.f \
	horder.f \
	idamax.f \
	lsyslv.f \
	newmsh.f \
	rkbas.f \
	sbblok.f \
	shiftb.f \
	skale.f \
	subbak.f \
	subfor.f \
	vmonde.f \
	vwblok.f

libcolnew.a : $(SRCS:.f=.o)
	$(AR) cr $@ $^