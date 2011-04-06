SRCS = dfsub.f \
	dgsub.f \
	solutn.f \
	fsub.f \
	gsub.f \
	prob3.f

prob3 : $(SRCS:.f=.o) 
	(cd ../fortran; $(MAKE))
	$(FC) -o $@  $(LDFLAGS) $^ ../fortran/libcolnew.a
