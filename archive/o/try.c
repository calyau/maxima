
#include "const.h"
#include "plot.h"


#undef TRUE
#undef FALSE
#undef CALL


#include <stdio.h>
#ifdef AIX3
#include <sys/select.h>
#endif
#include <rpc/rpc.h>
#define MAX_ARRAY(x) (x ? x :  20000)
#define CHECK(x) if (!x) {fprintf(stderr,"xdr failed"); exit(1);}


struct value_function
{ char * body ;
  int length;
  char *name;
  struct value *constants;
};

struct value*
complex(a,r,i)
     double r,i;
     struct value *a;
{ a->type= CMPLX;
  a->v.cmplx_val.real = r;
  a->v.cmplx_val.imag = i;
  return a;
}
struct value*
integer(a,i)
     int i;
     struct value *a;
{ a->type= INT;
  a->v.int_val = i;
  return a;
}
  

struct value *
xdr_read_values(xdrsp,length,result)
     struct value **result;
     int *length;
     XDR *xdrsp;
{int n;
 struct value *ans;
 int type;
 CHECK(xdr_int(xdrsp,&n));
 *length = n;
 *result = ans = (void *)malloc(sizeof(struct value)*n);
 while (--n >= 0)
   { xdr_int(type,&type);
     if (type == CMPLX)
       { CHECK(xdr_double(xdrsp,&(ans->v.cmplx_val.real)));
	 CHECK(xdr_double(xdrsp,&(ans->v.cmplx_val.imag)));
	 ans->type = CMPLX;
       }
     else
     if (type == INT)
       { CHECK(xdr_int(xdrsp,&(ans->v.int_val)));
	 ans->type = INT;
       }
     else
     if (type == REAL)
       { CHECK(xdr_double(xdrsp,&(ans->v.cmplx_val.real)));
	 ans->v.cmplx_val.imag = 0.0 ;
	 ans->type = CMPLX;
       }
   }
 return 0;
}

main(argc,argv )
     char *argv[];

 {XDR xdrs;
  int invoked=0;
  int length;
  struct value a;
  xdrstdio_create(&xdrs, stdin, XDR_DECODE);
  {
      xdrstdio_create(&xdrs, stdin, XDR_DECODE);
DO_ARGS:
      { struct value_function f;
	f.body=0;
	f.constants=0;
      CHECK(xdr_bytes(&xdrs,&f.body,&f.length,MAX_ARRAY(0)));
      CHECK(xdr_read_values(&xdrs,&length,&f.constants));

      /* invoke the function */
      push(complex(&a,atod(argv[1]),atod(argv[2])));

      execute_fun(&f);

      exit(0);}}}

   


