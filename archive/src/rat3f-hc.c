/* #include <xcmpinclude.h>   */

#ifdef SUN3
#define MC68000
#endif

#define MASK	0x7fffffff

object number_times(); 
object fixnum_times();
object mcmod();
object shift_integer();
object bignum2();
#define FIXNUMP(x) (type_of(x)==t_fixnum)

/* Note: the modulus is guaranteed > 0 */

#define FIX_MOD(X,MOD) {register int MOD_2; \
			     if (X > (MOD_2=(MOD >>1))) X=X-MOD; else \
			       if (X < -MOD_2)  X=X+MOD;}



#define MYmake_fixnum(doto,x) \
  {register int CMPt1; \
   doto \
   ((((CMPt1=(x))+1024)&-2048)==0?small_fixnum(CMPt1):make_fixnum(CMPt1));}
	  


    
int
dblrem(m,n,mod)
int m,n,mod;
{ asm("movl a6@(8),d1");
  asm("mulsl a6@(12),d0:d1");
  asm("divsl a6@(16),d0:d1");
}

/* adds m and n returning the remainder modulo mod */

plusrem(m,n,mod)
int m,n,mod; 
{ asm("movl a6@(0x8),d1");
  asm("addl a6@(0xc),d1");
  asm("bvs plus_overflow_case");
  asm("divsll a6@(16),d0:d1");
  asm("bra plus_endend");
  asm ("plus_overflow_case:");
  asm("bcs plus_neg_args");
  asm("clrl d0");
  asm("jra plus_end");
  asm("plus_neg_args:");
  asm("movl #-1,d0");
  asm("plus_end:");
  asm("divsl a6@(16),d0:d1");
  asm("plus_endend:");
}


/* subtracts n from m returning the remainder modulo mod */

subrem(m,n,mod)
int m,n,mod; 
{ asm("movl a6@(0x8),d1");
  asm("subl a6@(0xc),d1");
  asm("bvs sub_overflow_case");
  asm("divsll a6@(16),d0:d1");
  asm("bra sub_endend");
  asm ("sub_overflow_case:");
  asm("bcc sub_neg_args");
  asm("clrl d0");
  asm("jra sub_end");
  asm("sub_neg_args:");
  asm("movl #-1,d0");
  asm("sub_end:");
  asm("divsl a6@(16),d0:d1");
  asm("sub_endend:");
  }



/* like fixnum_times multiply to ints to get a t_fixnum or t_bignum ,
but utilize the ordinary mulsl for the common small case */

object
ftimes(m,n)
int m,n;
{register object res;
  asm("movl a6@(8),d0");
  asm("mulsl a6@(12),d0");
  asm("bvs ftimes_overflow");
  asm("movl d0,a6@(8)");
  MYmake_fixnum(res=,*&m);
  asm("bra ftimes_end");
  asm("ftimes_overflow:");
  res=fixnum_times(m,n);
  asm("ftimes_end:");
  return res;}


/*
int TOPhalf;

int
ftimes1(m,n)
int m,n;
{ asm("movl a6@(8),d0");
  asm("mulsl a6@(12),d1:d0");
  asm("movl d1, _TOPhalf");
}

*/


/* multiply fixnum objects m and n faster than number_times */
/*

object
ftimes(m,n)
object m,n;
{register int ans;
 ans=ftimes1(fix(m),fix(n));
 if (ans < 0)
   { if (TOPhalf==-1) return (CMPmake_fixnum(ans));
      else return (number_times(m,n));}
 else
   { if (TOPhalf==0) return (CMPmake_fixnum(ans));
      else return (number_times(m,n));}}

*/

object
ctimes(a,b,mod)
object a,b,mod;
{if (FIXNUMP(a) && FIXNUMP(b))
  {if (mod==Cnil) return ftimes(fix(a),fix(b));
   else if (FIXNUMP(mod))
     {register int res, m ;
      res=dblrem(fix(a),fix(b),m=fix(mod));
      FIX_MOD(res,m);
      MYmake_fixnum(return,res);}}
 return mcmod(number_times(a,b),mod);}

object	  
mcmod(x,mod)
object x,mod;
{if (mod==Cnil) return(x);
else
 if((type_of(mod)==t_fixnum && type_of(x)==t_fixnum))
    {register int xx,mm;
     mm=fix(mod);xx=(fix(x)%mm);
     FIX_MOD(xx,mm);
     MYmake_fixnum(return,xx);
   }
 else
   {object qp,rp,mod2;
    int compare;
    integer_quotient_remainder_1(x,mod,&qp,&rp);
    mod2=shift_integer(mod,-1);
    compare=number_compare(rp,mod2);
      if (compare > 0) rp=number_minus(rp,mod);
   return rp;}}


/* add two fixnums:  First add m and n, then if there is an overflow condition
 branch to construct bignum.  Otherwise set res = the result,
 and then act on it.  The use of *&m is to inhibit compilers from making
 the arg m a register, so that we would not know where it was. */
  

object
fplus(m,n)
int m,n; 
{object res;
  asm("movl a6@(0x8),d0");
  asm("addl a6@(0xc),d0");
  asm("bvs fplus_overflow_case");
  asm("movl d0,a6@(0x8)");
  asm("jra fplus_rest");
  asm ("fplus_overflow_case:");
  asm("movl d0,a6@(0x8)");
  res=((*&n>0)?bignum2(1, *&m & MASK):bignum2(-2, *&m & MASK));
  asm ("jra fplus_end");
  asm("fplus_rest:");
  MYmake_fixnum(res=,*&m);
  asm("fplus_end:");
    return res;
    }


/* subtract two fixnums:  
 First  m - n, then if there is an overflow condition
 branch to construct bignum.  Otherwise set res = the result,
 and then act on it.  The use of *&m is to inhibit compilers from making
 the arg m a register, so that we would not know where it was. */

object
fminus(m,n)
int m,n; 
{object res;
  asm("movl a6@(0x8),d0");
  asm("subl a6@(0xc),d0");
  asm("bvs fminus_overflow_case");
  asm("movl d0,a6@(0x8)");
  asm("jra fminus_rest");
  asm ("fminus_overflow_case:");
  asm("movl d0,a6@(0x8)");
  res=((*&n<0)?bignum2(1, *&m & MASK):bignum2(-2, *&m & MASK));
  asm ("jra fminus_end");
  asm("fminus_rest:");
  MYmake_fixnum(res=,*&m);
  asm("fminus_end:");
    return res;
    }




/* in fixnum case of m and mod put it into the right range. */

int
fmod(m,mod)
 int m,mod;
{int register res,m2;
 res=m%mod;
 m2= (mod >> 1);
 if (res > m2) return( m - mod);
 else if (res < -m2) return (res + mod);
 else return res;}

object
cdifference(a,b,mod)
object a,b,mod;
{if (FIXNUMP(mod))
   {register int res,m;
    res=((fix(a)-fix(b))%(m=fix(mod)));
    FIX_MOD(res,m);
    MYmake_fixnum(return,res);}
 else if (mod==Cnil)
     {if (FIXNUMP(a) && FIXNUMP(b))
	return fminus(fix(a),fix(b));
      else return(number_minus(a,b));
    }
 else return(mcmod(number_minus(a,b),mod));}



object
cplus(a,b,mod)
object a,b,mod;
{if (FIXNUMP(mod))
   {register int res,m;
    res=((fix(a)+fix(b))%(m=fix(mod)));
    FIX_MOD(res,m);
    MYmake_fixnum(return,res);}
 else
   if (mod==Cnil)
     {if (FIXNUMP(a) && FIXNUMP(b))
	return fplus(fix(a),fix(b));
      else return(number_plus(a,b));
    }
 else
   return(mcmod(number_plus(a,b),mod));}
     

/*
cdifference(a,b,mod)
object a,b,mod;
{if (FIXNUMP(a) && FIXNUMP(b))
  {if (mod==Cnil) return fplus(fix(a),(- fix(b))); 
   else if (FIXNUMP(mod))
     {register int res, m ;
      res=subrem(fix(a),fix(b),m=fix(mod));
      FIX_MOD(res,m);
      return (CMPmake_fixnum(res));}}
 return mcmod(number_minus(a,b),mod);}

    
object
cplus(a,b,mod)
object a,b,mod;
{if (FIXNUMP(a) && FIXNUMP(b))
  {if (mod==Cnil) return fplus(fix(a),fix(b));
   else if (FIXNUMP(mod))
     {register int res, m ,m2;
      res=plusrem(fix(a),fix(b),m=fix(mod));
      m2=(m >> 1);
      if (res > m2) res=res-m;
      else if (res < -m2) res=res+m;
      return (CMPmake_fixnum(res));}}
 return mcmod(number_plus(a,b),mod);}

*/



