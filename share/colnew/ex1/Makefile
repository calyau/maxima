SRCS = dfsub.f \
	dgsub.f \
	exact.f \
	fsub.f \
	gsub.f \
	prob1.f

prob1 : $(SRCS:.f=.o) 
	(cd ../fortran; $(MAKE))
	$(FC) -o $@  $(LDFLAGS) $^ ../fortran/libcolnew.a
