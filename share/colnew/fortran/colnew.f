c From research!csnet!CSNET-RELAY!mit-multics.arpa!UBC.mailnet!USER=NBAF Tue, 3 Feb 87 15:25:36 PST
C**********************************************************************
C  this package solves boundary value problems for
C  ordinary differential equations, as described below.
C
C  COLNEW is a modification of the package COLSYS by ascher,
C  christiansen and russell [1]. It incorporates a new basis
C  representation replacing b-splines, and improvements for
C  the linear and nonlinear algebraic equation solvers.
C  the package can be referenced as either COLNEW or COLSYS.
C**********************************************************************
C----------------------------------------------------------------------
C                            p a r t  1
C        main storage allocation and program control subroutines
C----------------------------------------------------------------------
C
      SUBROUTINE COLNEW (NCOMP, M, ALEFT, ARIGHT, ZETA, IPAR, LTOL,
     1                   TOL, FIXPNT, ISPACE, FSPACE, IFLAG,
     2                   FSUB, DFSUB, GSUB, DGSUB, GUESS)
C
C
C**********************************************************************
C
C     written by
C                  u. ascher,
C                            department of computer science,
C                            university of british columbia,
C                            vancouver, b. c., canada   v6t 1w5
C                  g. bader,
C                            institut f. angewandte mathematik
C                            university of heidelberg
C                            im neuenheimer feld 294
C                            d-6900 heidelberg 1
C
C**********************************************************************
C
C     purpose
C
C     this package solves a multi-point boundary value
C     problem for a mixed order system of ode-s given by
C
C          (m(i))
C         u       =  f  ( x; z(u(x)) )      i = 1, ... ,ncomp
C          i          i
C
C                                          aleft .lt. x .lt. aright,
C
C
C         g  ( zeta(j); z(u(zeta(j))) ) = 0   j = 1, ... ,mstar
C          j
C                                    mstar = m(1)+m(2)+...+m(ncomp),
C
C
C         where                          t
C               u = (u , u , ... ,u     )  is the exact solution vector
C                     1   2        ncomp
C
C                (mi)
C               u     is the mi=m(i) th  derivative of u
C                i                                      i
C
C                                  (1)        (m1-1)       (mncomp-1)
C               z(u(x)) = ( u (x),u  (x),...,u    (x),...,u      (x) )
C                            1     1          1            ncomp
C
C                f (x,z(u))   is a (generally) nonlinear function of
C                 i
C                             z(u)=z(u(x)).
C
C                g (zeta(j);z(u))  is a (generally) nonlinear function
C                 j
C                               used to represent a boundary condition.
C
C         the boundary points satisfy
C               aleft .le. zeta(1) .le. .. .le. zeta(mstar) .le. aright
C
C         the orders mi of the differential equations satisfy
C                            1 .le. m(i) .le. 4.
C
C
C**********************************************************************
C
C     method
C
C        the method used to approximate the solution u is
C     collocation at gaussian points, requiring m(i)-1 continuous
C     derivatives in the i-th component, i = 1, ..., ncomp.
C     here, k is the number of collocation points (stages) per
C     subinterval and is chosen such that k .ge. max m(i).
C     a runge-kutta-monomial solution representation is utilized.
C
C     references
C
C     [1] u. ascher, j. christiansen and r.d. russell,
C         collocation software for boundary-value odes,
C         acm trans. math software 7 (1981), 209-222.
C         this paper contains EXAMPLES where use of the code
C         is demonstrated.
C
C     [2] g. bader and u. ascher,
C         a new basis implementation for a mixed order
C         boundary value ode solver,
C         siam j. scient. stat. comput. (1987).
C
C     [3] u. ascher, j. christiansen and r.d. russell,
C         a collocation solver for mixed order
C         systems of boundary value problems,
C         math. comp. 33 (1979), 659-679.
C
C     [4] u. ascher, j. christiansen and r.d. russell,
C         colsys - a collocation code for boundary
C         value problems,
C         lecture notes comp.sc. 76, springer verlag,
C         b. childs et. al. (eds.) (1979), 164-185.
C
C     [5] c. deboor and r. weiss,
C         solveblok: a package for solving almost block diagonal
C         linear systems,
C         acm trans. math. software 6 (1980), 80-87.
C
C**********************************************************************
C
C     ***************     input to colnew     ***************
C
C     variables
C
C     ncomp - no. of differential equations   (ncomp .le. 20)
C
C     m(j) - order of the j-th differential equation
C            ( mstar = m(1) + ... + m(ncomp) .le. 40 )
C
C     aleft - left end of interval
C
C     aright - right end of interval
C
C     zeta(j) - j-th side condition point (boundary point). must
C               have  zeta(j) .le. zeta(j+1). all side condition
C               points must be mesh points in all meshes used,
C               see description of ipar(11) and fixpnt below.
C
C     ipar - an integer array dimensioned at least 11.
C            a list of the parameters in ipar and their meaning follows
C            some parameters are renamed in colnew; these new names are
C            given in parentheses.
C
C     ipar(1)     ( = nonlin )
C             = 0 if the problem is linear
C             = 1 if the problem is nonlinear
C
C     ipar(2) = no. of collocation points per subinterval  (= k )
C               where max m(i) .le.  k .le. 7 . if ipar(2)=0 then
C               colnew sets  k = max ( max m(i)+1, 5-max m(i) )
C
C     ipar(3) = no. of subintervals in the initial mesh  ( = n ).
C               if ipar(3) = 0 then colnew arbitrarily sets n = 5.
C
C     ipar(4) = no. of solution and derivative tolerances.  ( = ntol )
C               we require  0 .lt. ntol .le. mstar.
C
C     ipar(5) = dimension of fspace.     ( = ndimf )
C
C     ipar(6) = dimension of ispace.     ( = ndimi )
C
C     ipar(7) -  output control ( = iprint )
C              = -1 for full diagnostic printout
C              = 0 for selected printout
C              = 1 for no printout
C
C     ipar(8)     ( = iread )
C             = 0 causes colnew to generate a uniform initial mesh.
C             = 1 if the initial mesh is provided by the user.  it
C                 is defined in fspace as follows:  the mesh
C                 aleft=x(1).lt.x(2).lt. ... .lt.x(n).lt.x(n+1)=aright
C                 will occupy  fspace(1), ..., fspace(n+1). the
C                 user needs to supply only the interior mesh
C                 points  fspace(j) = x(j), j = 2, ..., n.
C             = 2 if the initial mesh is supplied by the user
C                 as with ipar(8)=1, and in addition no adaptive
C                 mesh selection is to be done.
C
C     ipar(9)     ( = iguess )
C             = 0 if no initial guess for the solution is
C                 provided.
C             = 1 if an initial guess is provided by the user
C                 in subroutine  guess.
C             = 2 if an initial mesh and approximate solution
C                 coefficients are provided by the user in  fspace.
C                 (the former and new mesh are the same).
C             = 3 if a former mesh and approximate solution
C                 coefficients are provided by the user in fspace,
C                 and the new mesh is to be taken twice as coarse;
C                 i.e.,every second point from the former mesh.
C             = 4 if in addition to a former initial mesh and
C                 approximate solution coefficients, a new mesh
C                 is provided in fspace as well.
C                 (see description of output for further details
C                 on iguess = 2, 3, and 4.)
C
C     ipar(10)= 0 if the problem is regular
C             = 1 if the first relax factor is =rstart, and the
C                 nonlinear iteration does not rely on past covergence
C                 (use for an extra sensitive nonlinear problem only).
C             = 2 if we are to return immediately upon  (a) two
C                 successive nonconvergences, or  (b) after obtaining
C                 error estimate for the first time.
C
C     ipar(11)= no. of fixed points in the mesh other than aleft
C               and aright. ( = nfxpnt , the dimension of fixpnt)
C               the code requires that all side condition points
C               other than aleft and aright (see description of
C               zeta ) be included as fixed points in fixpnt.
C
C     ltol  -  an array of dimension ipar(4). ltol(j) = l  specifies
C              that the j-th tolerance in  tol  controls the error
C              in the l-th component of z(u).   also require that
C              1.le.ltol(1).lt.ltol(2).lt. ... .lt.ltol(ntol).le.mstar
C
C     tol    - an array of dimension ipar(4). tol(j) is the
C              error tolerance on the ltol(j) -th component
C              of z(u). thus, the code attempts to satisfy
C              for j=1,...,ntol  on each subinterval
C              abs(z(v)-z(u))       .le. tol(j)*abs(z(u))       +tol(j)
C                            ltol(j)                     ltol(j)
C
C              if v(x) is the approximate solution vector.
C
C     fixpnt - an array of dimension ipar(11).   it contains
C              the points, other than aleft and aright, which
C              are to be included in every mesh.
C
C     ispace - an integer work array of dimension ipar(6).
C              its size provides a constraint on nmax,
C              the maximum number of subintervals. choose
C              ipar(6) according to the formula
C                      ipar(6)  .ge.  nmax*nsizei
C                where
C                      nsizei = 3 + kdm
C                with
C                      kdm = kd + mstar  ;  kd = k * ncomp ;
C                      nrec = no. of right end boundary conditions.
C
C
C     fspace - a real work array of dimension ipar(5).
C              its size provides a constraint on nmax.
C              choose ipar(5) according to the formula
C                      ipar(5)  .ge.  nmax*nsizef
C                where
C                      nsizef = 4 + 3 * mstar + (5+kd) * kdm +
C                              (2*mstar-nrec) * 2*mstar.
C
C
C     iflag - the mode of return from colnew.
C           = 1 for normal return
C           = 0 if the collocation matrix is singular.
C           =-1 if the expected no. of subintervals exceeds storage
C               specifications.
C           =-2 if the nonlinear iteration has not converged.
C           =-3 if there is an input data error.
C
C
C**********************************************************************
C
C     *************    user supplied subroutines   *************
C
C
C     the following subroutines must be declared external in the
C     main program which calls colnew.
C
C
C     fsub  - name of subroutine for evaluating f(x,z(u(x))) =
C                            t
C             (f ,...,f     )  at a point x in (aleft,aright).  it
C               1      ncomp
C             should have the heading
C
C                       subroutine fsub (x , z , f)
C
C             where f is the vector containing the value of fi(x,z(u))
C             in the i-th component and                            t
C                                       z(u(x))=(z(1),...,z(mstar))
C             is defined as above under  purpose .
C
C
C     dfsub - name of subroutine for evaluating the jacobian of
C             f(x,z(u)) at a point x.  it should have the heading
C
C                       subroutine dfsub (x , z , df)
C
C             where z(u(x)) is defined as for fsub and the (ncomp) by
C             (mstar) array df should be filled by the partial deriv-
C             atives of f, viz, for a particular call one calculates
C                                df(i,j) = dfi / dzj, i=1,...,ncomp
C                                                     j=1,...,mstar.
C
C
C     gsub  - name of subroutine for evaluating the i-th component of
C             g(x,z(u(x))) = g (zeta(i),z(u(zeta(i)))) at a point x =
C                             i
C             zeta(i) where 1.le.i.le.mstar. it should have the heading
C
C                       subroutine gsub (i , z , g)
C
C             where z(u) is as for fsub, and i and g=g  are as above.
C                                                     i
C             note that in contrast to f in  fsub , here
C             only one value per call is returned in g.
C
C
C     dgsub - name of subroutine for evaluating the i-th row of
C             the jacobian of g(x,u(x)).  it should have the heading
C
C                       subroutine dgsub (i , z , dg)
C
C             where z(u) is as for fsub, i as for gsub and the mstar-
C             vector dg should be filled with the partial derivatives
C             of g, viz, for a particular call one calculates
C                   dg(i,j) = dgi / dzj      j=1,...,mstar.
C
C
C     guess - name of subroutine to evaluate the initial
C             approximation for  z(u(x)) and for dmval(u(x))= vector
C             of the mj-th derivatives of u(x). it should have the
C             heading
C
C                       subroutine guess (x , z , dmval)
C
C             note that this subroutine is needed only if using
C             ipar(9) = 1, and then all  mstar  components of z
C             and  ncomp  components of  dmval  should be specified
C             for any x,  aleft .le. x .le. aright .
C
C
C**********************************************************************
C
C     ************   use of output from colnew   ************
C
C                 ***   solution evaluation   ***
C
C     on return from colnew, the arrays fspace and ispace
C     contain information specifying the approximate solution.
C     the user can produce the solution vector  z( u(x) )  at
C     any point x, aleft .le. x .le. aright, by the statement,
C
C           call appsln (x, z, fspace, ispace)
C
C     when saving the coefficients for later reference, only
C     ispace(1),...,ispace(7+ncomp)    and
C     fspace(1),...,fspace(ispace(7))    need to be saved as
C     these are the quantities used by appsln.
C
C
C                 ***   simple continuation   ***
C
C
C     a formerly obtained solution can easily be used as the
C     first approximation for the nonlinear iteration for a
C     new problem by setting   (iguess =) ipar(9) = 2, 3 or 4.
C
C     if the former solution has just been obtained then the
C     values needed to define the first approximation are
C     already in ispace and fspace.
C     alternatively, if the former solution was obtained in a
C     previous run and its coefficients were saved then those
C     coefficients must be put back into
C     ispace(1),..., ispace(7+ncomp)    and
C     fspace(1),..., fspace(ispace(7)).
C
C     for ipar(9) = 2 or 3 set ipar(3) = ispace(1) ( = the
C     size of the previous mesh ).
C
C     for ipar(9) = 4 the user specifies a new mesh of n subintervals
C     as follows.
C     the values in  fspace(1),...,fspace(ispace(7))  have to be
C     shifted by n+1 locations to  fspace(n+2),..,fspace(ispace(7)+n+1)
C     and the new mesh is then specified in fspace(1),..., fspace(n+1).
C     also set ipar(3) = n.
C
C
C**********************************************************************
C
C     ***************      package subroutines      ***************
C
C     the following description gives a brief overview of how the
C     procedure is broken down into the subroutines which make up
C     the package called  colnew . for further details the
C     user should refer to documentation in the various subroutines
C     and to the references cited above.
C
C     the subroutines fall into four groups:
C
C part 1 - the main storage allocation and program control subr
C
C     colnew - tests input values, does initialization and breaks up
C              the work areas, fspace and ispace, into the arrays
C              used by the program.
C     colsys - another name for colnew
C
C     contrl - is the actual driver of the package. this routine
C              contains the strategy for nonlinear equation solving.
C
C     skale  - provides scaling for the control
C              of convergence in the nonlinear iteration.
C
C
C part 2 - mesh selection and error estimation subroutines
C
C     consts - is called once by  colnew  to initialize constants
C              which are used for error estimation and mesh selection.
C
C     newmsh - generates meshes. it contains the test to decide
C              whether or not to redistribute a mesh.
C
C     errchk - produces error estimates and checks against the
C              tolerances at each subinterval
C
C
C part 3 - collocation system set-up subroutines
C
C     lsyslv - controls the set-up and solution of the linear
C              algebraic systems of collocation equations which
C              arise at each newton iteration.
C
C     gderiv - is used by lsyslv to set up the equation associated
C              with a side condition point.
C
C     vwblok - is used by lsyslv to set up the equation(s) associated
C              with a collocation point.
C
C     gblock - is used by lsyslv to construct a block of the global
C              collocation matrix or the corresponding right hand
C              side.
C
C
C part 4 - service subroutines
C
C     appsln - sets up a standard call to  approx .
C
C     approx - evaluates a piecewise polynomial solution.
C
C     rkbas  - evaluates the mesh independent runge-kutta basis
C
C     vmonde - solves a vandermonde system for given right hand
C              side
C
C     horder - evaluates the highest order derivatives of the
C              current collocation solution used for mesh refinement.
C
C
C part 5 - linear algebra  subroutines
C
C     to solve the global linear systems of collocation equations
C     constructed in part 3,  colnew  uses a column oriented version
C     of the package  solveblok originally due to de boor and weiss.
C
C     to solve the linear systems for static parameter condensation
C     in each block of the collocation equations, the linpack
C     routines  dgefa and  dgesl  are included. but these
C     may be replaced when solving problems on vector processors
C     or when solving large scale sparse jacobian problems.
C
C----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION M(1), ZETA(1), IPAR(1), LTOL(1), TOL(1), DUMMY(1),
     1          FIXPNT(1), ISPACE(1), FSPACE(1)
C
      COMMON /COLOUT/ PRECIS, IOUT, IPRINT
      COMMON /COLLOC/ RHO(7), COEF(49)
      COMMON /COLORD/ K, NC, MSTAR, KD, MMAX, MT(20)
      COMMON /COLAPR/ N, NOLD, NMAX, NZ, NDMZ
      COMMON /COLMSH/ MSHFLG, MSHNUM, MSHLMT, MSHALT
      COMMON /COLSID/ TZETA(40), TLEFT, TRIGHT, IZETA, IDUM
      COMMON /COLNLN/ NONLIN, ITER, LIMIT, ICARE, IGUESS
      COMMON /COLEST/ TTL(40), WGTMSH(40), WGTERR(40), TOLIN(40),
     1                ROOT(40), JTOL(40), LTTOL(40), NTOL
C
      EXTERNAL FSUB, DFSUB, GSUB, DGSUB, GUESS
C
C     this subroutine can be called either COLNEW or COLSYS
C
      ENTRY      COLSYS (NCOMP, M, ALEFT, ARIGHT, ZETA, IPAR, LTOL,
     1                   TOL, FIXPNT, ISPACE, FSPACE, IFLAG,
     2                   FSUB, DFSUB, GSUB, DGSUB, GUESS)
C
C*********************************************************************
C
C     the actual subroutine colnew serves as an interface with
C     the package of subroutines referred to collectively as
C     colnew. the subroutine serves to test some of the input
C     parameters, rename some of the parameters (to make under-
C     standing of the coding easier), to do some initialization,
C     and to break the work areas fspace and ispace up into the
C     arrays needed by the program.
C
C**********************************************************************
C
C...  specify machine dependent output unit  iout  and compute machine
C...  dependent constant  precis = 100 * machine unit roundoff
C
      IF ( IPAR(7) .LE. 0 )  WRITE(6,99)
  99  FORMAT(//,33H VERSION *COLNEW* OF COLSYS .    ,//)
C
      IOUT = 6
      PRECIS = 1.D0
   10 PRECIS = PRECIS / 2.D0
      PRECP1 = PRECIS + 1.D0
      IF ( PRECP1 .GT. 1.D0 )                       GO TO 10
      PRECIS = PRECIS * 100.D0
C
C...  in case incorrect input data is detected, the program returns
C...  immediately with iflag=-3.
C
      IFLAG = -3
      IF ( NCOMP .LT. 1 .OR. NCOMP .GT. 20 )        RETURN
      DO 20 I=1,NCOMP
         IF ( M(I) .LT. 1 .OR. M(I) .GT. 4 )        RETURN
   20 CONTINUE
C
C...  rename some of the parameters and set default values.
C
      NONLIN = IPAR(1)
      K = IPAR(2)
      N = IPAR(3)
      IF ( N .EQ. 0 )  N = 5
      IREAD = IPAR(8)
      IGUESS = IPAR(9)
      IF ( NONLIN .EQ. 0 .AND. IGUESS .EQ. 1 )  IGUESS = 0
      IF ( IGUESS .GE. 2 .AND. IREAD .EQ. 0 )   IREAD = 1
      ICARE = IPAR(10)
      NTOL = IPAR(4)
      NDIMF = IPAR(5)
      NDIMI = IPAR(6)
      NFXPNT = IPAR(11)
      IPRINT = IPAR(7)
      MSTAR = 0
      MMAX = 0
      DO  30 I = 1, NCOMP
         MMAX = MAX0 ( MMAX, M(I) )
         MSTAR = MSTAR + M(I)
         MT(I) = M(I)
   30 CONTINUE
      IF ( K .EQ. 0 )   K = MAX0( MMAX + 1 , 5 - MMAX )
      DO 40 I = 1, MSTAR
   40 TZETA(I) = ZETA(I)
      DO 50 I = 1, NTOL
         LTTOL(I) = LTOL(I)
   50 TOLIN(I) = TOL(I)
      TLEFT = ALEFT
      TRIGHT = ARIGHT
      NC = NCOMP
      KD = K * NCOMP
C
C...  print the input data for checking.
C
      IF ( IPRINT .GT. -1 )                         GO TO 80
      IF ( NONLIN .GT. 0 )                          GO TO 60
      WRITE (IOUT,260) NCOMP, (M(IP), IP=1,NCOMP)
      GO TO 70
   60 WRITE (IOUT,270) NCOMP, (M(IP), IP=1,NCOMP)
   70 WRITE (IOUT,280) (ZETA(IP), IP=1,MSTAR)
      IF ( NFXPNT .GT. 0 )
     1   WRITE (IOUT,340) NFXPNT, (FIXPNT(IP), IP=1,NFXPNT)
      WRITE (IOUT,290) K
      WRITE (IOUT,300) (LTOL(IP), IP=1,NTOL)
      WRITE (IOUT,310) (TOL(IP), IP=1,NTOL)
      IF (IGUESS .GE. 2) WRITE (IOUT,320)
      IF (IREAD .EQ. 2) WRITE (IOUT,330)
   80 CONTINUE
C
C...  check for correctness of data
C
      IF ( K .LT. 0 .OR. K .GT. 7 )                 RETURN
      IF ( N .LT. 0 )                               RETURN
      IF ( IREAD .LT. 0 .OR. IREAD .GT. 2 )         RETURN
      IF ( IGUESS .LT. 0 .OR. IGUESS .GT. 4 )       RETURN
      IF ( ICARE .LT. 0 .OR. ICARE .GT. 2 )         RETURN
      IF ( NTOL .LT. 0 .OR. NTOL .GT. MSTAR )       RETURN
      IF ( NFXPNT .LT. 0 )                          RETURN
      IF ( IPRINT .LT. (-1) .OR. IPRINT .GT. 1 )    RETURN
      IF ( MSTAR .LT. 0 .OR. MSTAR .GT. 40 )        RETURN
      IP = 1
      DO 100 I = 1, MSTAR
      IF ( DABS(ZETA(I) - ALEFT) .LT. PRECIS .OR.
     1     DABS(ZETA(I) - ARIGHT) .LT. PRECIS )     GO TO 100
   90 IF ( IP .GT. NFXPNT )                         RETURN
        IF ( ZETA(I) - PRECIS .LT. FIXPNT(IP) )     GO TO 95
        IP = IP + 1
      GO TO 90
   95 IF ( ZETA(I) + PRECIS .LT. FIXPNT(IP) )       RETURN
  100 CONTINUE
C
C...  set limits on iterations and initialize counters.
C...  limit = maximum number of newton iterations per mesh.
C...  see subroutine  newmsh  for the roles of  mshlmt , mshflg ,
C...  mshnum , and  mshalt .
C
      MSHLMT = 3
      MSHFLG = 0
      MSHNUM = 1
      MSHALT = 1
      LIMIT = 40
C
C...  compute the maxium possible n for the given sizes of
C...  ispace  and  fspace.
C
      NREC = 0
      DO 110 I = 1, MSTAR
           IB = MSTAR + 1 - I
           IF ( ZETA(IB) .GE. ARIGHT )  NREC = I
  110 CONTINUE
      NFIXI = MSTAR
      NSIZEI = 3 + KD + MSTAR
      NFIXF = NREC * (2*MSTAR) + 5 * MSTAR + 3
      NSIZEF = 4 + 3 * MSTAR + (KD+5) * (KD+MSTAR) +
     1(2*MSTAR-NREC) * 2*MSTAR
      NMAXF = (NDIMF - NFIXF) / NSIZEF
      NMAXI = (NDIMI - NFIXI) / NSIZEI
      IF ( IPRINT .LT. 1 )  WRITE(IOUT,350) NMAXF, NMAXI
      NMAX = MIN0( NMAXF, NMAXI )
      IF ( NMAX .LT. N )                            RETURN
      IF ( NMAX .LT. NFXPNT+1 )                     RETURN
      IF (NMAX .LT. 2*NFXPNT+2 .AND. IPRINT .LT. 1)  WRITE(IOUT,360)
C
C...  generate pointers to break up  fspace  and  ispace .
C
      LXI = 1
      LG = LXI + NMAX + 1
      LXIOLD = LG + 2*MSTAR * (NMAX * (2*MSTAR-NREC) + NREC)
      LW     = LXIOLD + NMAX + 1
      LV     = LW + KD**2 * NMAX
      LZ     = LV + MSTAR * KD * NMAX
      LDMZ   = LZ + MSTAR * (NMAX + 1)
      LDELZ  = LDMZ + KD * NMAX
      LDELDZ = LDELZ + MSTAR * (NMAX + 1)
      LDQZ   = LDELDZ + KD * NMAX
      LDQDMZ = LDQZ + MSTAR * (NMAX + 1)
      LRHS   = LDQDMZ + KD * NMAX
      LVALST = LRHS   + KD * NMAX + MSTAR
      LSLOPE = LVALST + 4 * MSTAR * NMAX
      LACCUM = LSLOPE + NMAX
      LSCL   = LACCUM + NMAX + 1
      LDSCL  = LSCL + MSTAR * (NMAX + 1)
      LPVTG = 1
      LPVTW = LPVTG + MSTAR * (NMAX + 1)
      LINTEG = LPVTW + KD * NMAX
C
C...  if  iguess .ge. 2, move  xiold, z, and  dmz  to their proper
C...  locations in  fspace.
C
      IF ( IGUESS .LT. 2 )                          GO TO 160
      NOLD = N
      IF (IGUESS .EQ. 4)  NOLD = ISPACE(1)
      NZ = MSTAR * (NOLD + 1)
      NDMZ = KD * NOLD
      NP1 = N + 1
      IF ( IGUESS .EQ. 4 )  NP1 = NP1 + NOLD + 1
      DO 120 I=1,NZ
  120 FSPACE( LZ+I-1 )  =  FSPACE( NP1+I )
      IDMZ = NP1 + NZ
      DO 125 I=1,NDMZ
  125 FSPACE( LDMZ+I-1 )  =  FSPACE( IDMZ+I )
      NP1 = NOLD + 1
      IF ( IGUESS .EQ. 4 )                          GO TO 140
      DO 130 I=1,NP1
  130 FSPACE( LXIOLD+I-1 )  =  FSPACE( LXI+I-1 )
      GO TO 160
  140 DO 150 I=1,NP1
  150 FSPACE( LXIOLD+I-1 )  =  FSPACE( N+1+I )
  160 CONTINUE
C
C...  initialize collocation points, constants, mesh.
C
      CALL CONSTS ( K, RHO, COEF )
      CALL NEWMSH (3+IREAD, FSPACE(LXI), FSPACE(LXIOLD), DUMMY,
     1             DUMMY, DUMMY, DUMMY, DUMMY, NFXPNT, FIXPNT)
C
C...  determine first approximation, if the problem is nonlinear.
C
      IF (IGUESS .GE. 2)                            GO TO 230
      NP1 = N + 1
      DO 210 I = 1, NP1
  210 FSPACE( I + LXIOLD - 1 ) = FSPACE( I + LXI - 1 )
      NOLD = N
      IF ( NONLIN .EQ. 0  .OR. IGUESS .EQ. 1 )      GO TO 230
C
C...  system provides first approximation of the solution.
C...  choose z(j) = 0  for j=1,...,mstar.
C
      DO 220 I=1, NZ
  220 FSPACE( LZ-1+I ) = 0.D0
      DO 225 I=1, NDMZ
  225 FSPACE( LDMZ-1+I ) = 0.D0
  230 CONTINUE
      IF (IGUESS .GE. 2)  IGUESS = 0
      CALL CONTRL (FSPACE(LXI),FSPACE(LXIOLD),FSPACE(LZ),FSPACE(LDMZ),
     1     FSPACE(LRHS),FSPACE(LDELZ),FSPACE(LDELDZ),FSPACE(LDQZ),
     2     FSPACE(LDQDMZ),FSPACE(LG),FSPACE(LW),FSPACE(LV),
     3     FSPACE(LVALST),FSPACE(LSLOPE),FSPACE(LSCL),FSPACE(LDSCL),
     4     FSPACE(LACCUM),ISPACE(LPVTG),ISPACE(LINTEG),ISPACE(LPVTW),
     5     NFXPNT,FIXPNT,IFLAG,FSUB,DFSUB,GSUB,DGSUB,GUESS )
C
C...  prepare output
C
      ISPACE(1) = N
      ISPACE(2) = K
      ISPACE(3) = NCOMP
      ISPACE(4) = MSTAR
      ISPACE(5) = MMAX
      ISPACE(6) = NZ + NDMZ + N + 2
      K2 = K * K
      ISPACE(7) = ISPACE(6) + K2 - 1
      DO 240 I = 1, NCOMP
  240 ISPACE(7+I) = M(I)
      DO 250 I = 1, NZ
  250 FSPACE( N+1+I ) = FSPACE( LZ-1+I )
      IDMZ = N + 1 + NZ
      DO 255 I = 1, NDMZ
  255 FSPACE( IDMZ+I ) = FSPACE( LDMZ-1+I )
      IC = IDMZ + NDMZ
      DO 258 I = 1, K2
  258 FSPACE( IC+I ) = COEF(I)
      RETURN
C----------------------------------------------------------------------
  260 FORMAT(/// 37H THE NUMBER OF (LINEAR) DIFF EQNS IS , I3/ 1X,
     1       16HTHEIR ORDERS ARE, 20I3)
  270 FORMAT(/// 40H THE NUMBER OF (NONLINEAR) DIFF EQNS IS , I3/ 1X,
     1       16HTHEIR ORDERS ARE, 20I3)
  280 FORMAT(27H SIDE CONDITION POINTS ZETA, 8F10.6, 4( / 27X, 8F10.6))
  290 FORMAT(37H NUMBER OF COLLOC PTS PER INTERVAL IS, I3)
  300 FORMAT(39H COMPONENTS OF Z REQUIRING TOLERANCES -,8(7X,I2,1X),
     1       4(/38X,8I10))
  310 FORMAT(33H CORRESPONDING ERROR TOLERANCES -,6X,8D10.2,
     1       4(/39X,8D10.2))
  320 FORMAT(44H INITIAL MESH(ES) AND Z,DMZ PROVIDED BY USER)
  330 FORMAT(27H NO ADAPTIVE MESH SELECTION)
  340 FORMAT(10H THERE ARE ,I5,27H FIXED POINTS IN THE MESH - ,
     1       10(6F10.6/))
  350 FORMAT(44H THE MAXIMUM NUMBER OF SUBINTERVALS IS MIN (, I4,
     1       23H (ALLOWED FROM FSPACE),,I4, 24H (ALLOWED FROM ISPACE) ))
  360 FORMAT(/53H INSUFFICIENT SPACE TO DOUBLE MESH FOR ERROR ESTIMATE)
      END
