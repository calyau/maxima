SRCS = dfsub.f \
	dgsub.f \
	solutn.f \
	fsub.f \
	gsub.f \
	prob2.f

prob2 : $(SRCS:.f=.o) 
	(cd ../fortran; $(MAKE))
	$(FC) -o $@  $(LDFLAGS) $^ ../fortran/libcolnew.a
