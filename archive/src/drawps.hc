

/* 
number of ngons
ngons   {number of vertices to each polygon}
[i1,i2,i3,j1,j2,j3, ] 
[p1,p2,p3,p4,p5,p6,....]

*/



#define ZVAL(pts,verti,i) (pts)[3*verti[i]+2]  

static int n_vertices;
static double *pt_array;
  
static int pt_compare(p,q)
     short *p,*q;
{ double m = ZVAL(pt_array,p,n_vertices) -  ZVAL(pt_array,q,n_vertices);
  return (m < 0.0 ? -1 : m > 0.0 ? 1 : 0);
  }
	      

static int
sort_ngons(pts,vertex,ngons)
object vertex,pts;
int ngons;
{
sort_ngons1(pts->lfa.lfa_self,vertex->ust.ust_self,ngons,
  (vertex->ust.ust_fillp)/(ngons+1));

 return 0;
}
  
static int  
sort_ngons1(pts,vertex,ngons,number_ngons)
short *vertex;
double *pts;
int ngons,number_ngons;
{short *p = vertex;
 int i;

   for (i=0; i < number_ngons ; i++) {
  { short * maxz_at = p;
      {int n = ngons;
       while (--n > 0)
         if (ZVAL(pts,p,n) > ZVAL(pts,maxz_at,0))
           maxz_at = p+n;
       p[ngons] = maxz_at[0];
       p += (1+ ngons);
     }}}
  n_vertices = ngons;
  pt_array = pts;
  qsort(vertex,number_ngons,sizeof(short)*(ngons+1),pt_compare);
  return 0;
}

   
       
	     


