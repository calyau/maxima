
#include <stdio.h>
#ifdef AIX3
#include <sys/select.h>
#endif
#include <rpc/rpc.h>
#define MAX_ARRAY(x) (x ? x :  20000)
#define CHECK(x) if (!x) {fprintf(stderr,"xdr failed"); exit(1);}

      main()
 {XDR xdrs;
  int invoked=0;
  xdrstdio_create(&xdrs, stdin, XDR_DECODE);
  {
      double *t= 0 ;
      u_int t_length = 0;
      int ldt;
      int n;
      double *det= 0 ;
      u_int det_length = 0;
      int job;
      int info;
      xdrstdio_create(&xdrs, stdin, XDR_DECODE);
DO_ARGS:
      CHECK(xdr_array(&xdrs,&t,&t_length, MAX_ARRAY(t_length),
        sizeof(double),xdr_double));
      CHECK(xdr_int(&xdrs,&ldt));
      CHECK(xdr_int(&xdrs,&n));
      CHECK(xdr_array(&xdrs,&det,&det_length, MAX_ARRAY(det_length),
        sizeof(double),xdr_double));
      if (det_length != 2)fprintf(stderr,"Wrong length for det ");
      CHECK(xdr_int(&xdrs,&job));
      CHECK(xdr_int(&xdrs,&info));
      /* invoke the function */
 
      dtrdi_(t,&ldt,&n,det,&job,&info);
   
      /* write the results out */
      xdrstdio_create(&xdrs, stdout, XDR_ENCODE);
      CHECK(xdr_array(&xdrs,&t,&t_length, MAX_ARRAY(t_length),
        sizeof(double),xdr_double));
      CHECK(xdr_array(&xdrs,&det,&det_length, MAX_ARRAY(det_length),
        sizeof(double),xdr_double));
      CHECK(xdr_int(&xdrs,&info));
      exit(0);}}