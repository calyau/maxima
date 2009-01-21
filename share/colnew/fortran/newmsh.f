C----------------------------------------------------------------------
C                            p a r t  2
C          mesh selection, error estimation, (and related
C          constant assignment) routines -- see [3], [4]
C----------------------------------------------------------------------
C
      SUBROUTINE NEWMSH (MODE, XI, XIOLD, Z, DMZ, VALSTR,
     1                   SLOPE, ACCUM, NFXPNT, FIXPNT)
C
C**********************************************************************
C
C   purpose
C            select a mesh on which a collocation solution is to be
C            determined
C
C                           there are 5 possible modes of action:
C            mode = 5,4,3 - deal mainly with definition of an initial
C                           mesh for the current boundary value problem
C                 = 2,1   - deal with definition of a new mesh, either
C                           by simple mesh halving or by mesh selection
C            more specifically, for
C            mode = 5  an initial (generally nonuniform) mesh is
C                      defined by the user and no mesh selection is to
C                      be performed
C                 = 4  an initial (generally nonuniform) mesh is
C                      defined by the user
C                 = 3  a simple uniform mesh (except possibly for some
C                      fixed points) is defined; n= no. of subintervals
C                 = 1  the automatic mesh selection procedure is used
C                      (see [3] for details)
C                 = 2  a simple mesh halving is performed
C
C**********************************************************************
C
C   variables
C
C            n      = number of mesh subintervals
C            nold   = number of subintervals for former mesh
C            xi     - mesh point array
C            xiold  - former mesh point array
C            mshlmt - maximum no. of mesh selections which are permitted
C                     for a given n before mesh halving
C            mshnum - no. of mesh selections which have actually been
C                     performed for the given n
C            mshalt - no. of consecutive times ( plus 1 ) the mesh
C                     selection has alternately halved and doubled n.
C                     if mshalt .ge. mshlmt then  contrl  requires
C                     that the current mesh be halved.
C            mshflg = 1  the mesh is a halving of its former mesh
C                       (so an error estimate has been calculated)
C                   = 0  otherwise
C            iguess - ipar(9) in subroutine colnew.  it is used
C                     here only for mode=5 and 4, where
C                   = 2 the subroutine sets xi=xiold.  this is
C                       used e.g. if continuation is being per-
C                       formed, and a mesh for the old differen-
C                       tial equation is being used
C                   = 3 same as for =2, except xi uses every other
C                       point of xiold (so mesh xiold is mesh xi
C                       halved)
C                   = 4 xi has been defined by the user, and an old
C                       mesh xiold is also available
C                       otherwise, xi has been defined by the user
C                       and we set xiold=xi in this subroutine
C            slope  - an approximate quantity to be equidistributed for
C                     mesh selection (see [3]), viz,
C                             .                        (k+mj)
C                     slope(i)=     max   (weight(l) *u      (xi(i)))
C                               1.le.l.le.ntol         j
C
C                     where j=jtol(l)
C            slphmx - maximum of slope(i)*(xiold(i+1)-xiold(i)) for
C                     i = 1 ,..., nold.
C            accum  - accum(i) is the integral of  slope  from  aleft
C                     to  xiold(i).
C            valstr - is assigned values needed in  errchk  for the
C                     error estimate.
C**********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION D1(40), D2(40), SLOPE(1), ACCUM(1), VALSTR(1)
      DIMENSION XI(1), XIOLD(1), Z(1), DMZ(1), FIXPNT(1), DUMMY(1)
C
      COMMON /COLOUT/ PRECIS, IOUT, IPRINT
      COMMON /COLORD/ K, NCOMP, MSTAR, KD, MMAX, M(20)
      COMMON /COLAPR/ N, NOLD, NMAX, NZ, NDMZ
      COMMON /COLMSH/ MSHFLG, MSHNUM, MSHLMT, MSHALT
      COMMON /COLNLN/ NONLIN, ITER, LIMIT, ICARE, IGUESS
      COMMON /COLSID/  ZETA(40), ALEFT, ARIGHT, IZETA, IDUM
      COMMON /COLBAS/ B(28), ACOL(28,7), ASAVE(28,4)
      COMMON /COLEST/ TOL(40), WGTMSH(40), WGTERR(40), TOLIN(40),
     1                ROOT(40), JTOL(40), LTOL(40), NTOL
C
      NFXP1 = NFXPNT +1
      GO TO (180, 100, 50, 20, 10), MODE
C
C...  mode=5   set mshlmt=1 so that no mesh selection is performed
C
   10 MSHLMT = 1
C
C...  mode=4   the user-specified initial mesh is already in place.
C
   20 IF ( IGUESS .LT. 2 )                          GO TO 40
C
C...  iguess=2, 3 or 4.
C
      NOLDP1 = NOLD + 1
      IF (IPRINT .LT. 1)  WRITE(IOUT,360) NOLD, (XIOLD(I), I=1,NOLDP1)
      IF ( IGUESS .NE. 3 )                          GO TO 40
C
C...  if iread ( ipar(8) ) .ge. 1 and iguess ( ipar(9) ) .eq. 3
C...  then the first mesh is every second point of the
C...  mesh in  xiold .
C
      N = NOLD /2
      I = 0
      DO 30 J = 1, NOLD, 2
           I = I + 1
   30 XI(I) = XIOLD(J)
   40 CONTINUE
      NP1 = N + 1
      XI(1) = ALEFT
      XI(NP1) = ARIGHT
      GO TO 320
C
C...  mode=3   generate a (piecewise) uniform mesh. if there are
C...  fixed points then ensure that the n being used is large enough.
C
   50 IF ( N .LT. NFXP1 )  N = NFXP1
      NP1 = N + 1
      XI(1) = ALEFT
      ILEFT = 1
      XLEFT = ALEFT
C
C...  loop over the subregions between fixed points.
C
      DO 90 J = 1, NFXP1
           XRIGHT = ARIGHT
           IRIGHT = NP1
           IF ( J .EQ. NFXP1 )                      GO TO 60
           XRIGHT = FIXPNT(J)
C
C...       determine where the j-th fixed point should fall in the
C...       new mesh - this is xi(iright) and the (j-1)st fixed
C...       point is in xi(ileft)
C
           NMIN = (XRIGHT-ALEFT) / (ARIGHT-ALEFT) * DFLOAT(N) + 1.5D0
           IF (NMIN .GT. N-NFXPNT+J)  NMIN = N - NFXPNT + J
           IRIGHT = MAX0 (ILEFT+1, NMIN)
   60      XI(IRIGHT) = XRIGHT
C
C...       generate equally spaced points between the j-1st and the
C...       j-th fixed points.
C
           NREGN = IRIGHT - ILEFT - 1
           IF ( NREGN .EQ. 0 )                      GO TO 80
           DX = (XRIGHT - XLEFT) / DFLOAT(NREGN+1)
           DO 70 I = 1, NREGN
   70      XI(ILEFT+I) = XLEFT  +  DFLOAT(I) * DX
   80      ILEFT = IRIGHT
           XLEFT = XRIGHT
   90 CONTINUE
      GO TO 320
C
C...  mode=2  halve the current mesh (i.e. double its size)
C
  100 N2 = 2 * N
C
C...  check that n does not exceed storage limitations
C
      IF ( N2 .LE. NMAX )                           GO TO 120
C
C...  if possible, try with n=nmax. redistribute first.
C
      IF ( MODE .EQ. 2 )                            GO TO 110
      N = NMAX / 2
      GO TO 220
  110 IF ( IPRINT .LT. 1 )  WRITE(IOUT,370)
      N = N2
      RETURN
C
C...  calculate the old approximate solution values at
C...  points to be used in  errchk  for error estimates.
C...  if  mshflg  =1 an error estimate was obtained for
C...  for the old approximation so half the needed values
C...  will already be in  valstr .
C
  120 IF ( MSHFLG .EQ. 0 )                          GO TO 140
C
C...  save in  valstr  the values of the old solution
C...  at the relative positions 1/6 and 5/6 in each subinterval.
C
      KSTORE = 1
      DO 130 I = 1, NOLD
          HD6 = (XIOLD(I+1) - XIOLD(I)) / 6.D0
          X = XIOLD(I) + HD6
          CALL APPROX (I, X, VALSTR(KSTORE), ASAVE(1,1), DUMMY, XIOLD,
     1         NOLD, Z, DMZ, K, NCOMP, MMAX, M, MSTAR, 4, DUMMY, 0)
          X = X + 4.D0 * HD6
          KSTORE = KSTORE  +  3 * MSTAR
          CALL APPROX (I, X, VALSTR(KSTORE), ASAVE(1,4), DUMMY, XIOLD,
     1         NOLD, Z, DMZ, K, NCOMP, MMAX, M, MSTAR, 4, DUMMY, 0)
          KSTORE = KSTORE  +  MSTAR
  130 CONTINUE
      GO TO 160
C
C...  save in  valstr  the values of the old solution
C...  at the relative positions 1/6, 2/6, 4/6 and 5/6 in
C...  each subinterval.
C
  140 KSTORE = 1
      DO 150 I = 1, N
         X = XI(I)
         HD6 = (XI(I+1) - XI(I)) / 6.D0
         DO 150 J = 1, 4
           X = X + HD6
           IF ( J.EQ.3 )  X = X + HD6
           CALL APPROX (I, X, VALSTR(KSTORE), ASAVE(1,J), DUMMY, XIOLD,
     1          NOLD, Z, DMZ, K, NCOMP, MMAX, M, MSTAR, 4, DUMMY, 0)
           KSTORE = KSTORE  +  MSTAR
  150 CONTINUE
  160 MSHFLG = 0
      MSHNUM = 1
      MODE = 2
C
C...  generate the halved mesh.
C
      J = 2
      DO 170 I = 1, N
           XI(J) = (XIOLD(I) + XIOLD(I+1)) / 2.D0
           XI(J+1) = XIOLD(I+1)
  170 J = J + 2
      N = N2
      GO TO 320
C
C...  mode=1  we do mesh selection if it is deemed worthwhile
C
  180 IF ( NOLD .EQ. 1 )                            GO TO 100
      IF ( NOLD .LE. 2*NFXPNT )                     GO TO 100
C
C...  the first interval has to be treated separately from the
C...  other intervals (generally the solution on the (i-1)st and ith
C...  intervals will be used to approximate the needed derivative, but
C...  here the 1st and second intervals are used.)
C
      I = 1
      HIOLD = XIOLD(2) - XIOLD(1)
      CALL HORDER (1, D1, HIOLD, DMZ, NCOMP, K)
      HIOLD = XIOLD(3) - XIOLD(2)
      CALL HORDER (2, D2, HIOLD, DMZ, NCOMP, K)
      ACCUM(1) = 0.D0
      SLOPE(1) = 0.D0
      ONEOVH = 2.D0 / ( XIOLD(3) - XIOLD(1) )
      DO 190 J = 1, NTOL
           JJ = JTOL(J)
           JZ = LTOL(J)
  190 SLOPE(1) = DMAX1(SLOPE(1),(DABS(D2(JJ)-D1(JJ))*WGTMSH(J)*
     1           ONEOVH / (1.D0 + DABS(Z(JZ)))) **ROOT(J))
      SLPHMX = SLOPE(1) * (XIOLD(2) - XIOLD(1))
      ACCUM(2) = SLPHMX
      IFLIP = 1
C
C...  go through the remaining intervals generating  slope
C...  and  accum .
C
      DO 210 I = 2, NOLD
           HIOLD = XIOLD(I+1) - XIOLD(I)
           IF ( IFLIP .EQ. -1 )
     1                   CALL HORDER ( I, D1, HIOLD, DMZ, NCOMP, K)
           IF ( IFLIP .EQ. 1 )
     1                   CALL HORDER ( I, D2, HIOLD, DMZ, NCOMP, K)
           ONEOVH = 2.D0 / ( XIOLD(I+1) - XIOLD(I-1) )
           SLOPE(I) = 0.D0
C
C...       evaluate function to be equidistributed
C
           DO 200 J = 1, NTOL
             JJ = JTOL(J)
             JZ = LTOL(J)  +  (I-1) * MSTAR
  200      SLOPE(I) = DMAX1(SLOPE(I),(DABS(D2(JJ)-D1(JJ))*WGTMSH(J)*
     1                ONEOVH / (1.D0 + DABS(Z(JZ)))) **ROOT(J))
C
C...       accumulate approximate integral of function to be
C...       equidistributed
C
           TEMP = SLOPE(I) * (XIOLD(I+1)-XIOLD(I))
           SLPHMX = DMAX1(SLPHMX,TEMP)
           ACCUM(I+1) = ACCUM(I) + TEMP
  210 IFLIP = - IFLIP
      AVRG = ACCUM(NOLD+1) / DFLOAT(NOLD)
      DEGEQU = AVRG / DMAX1(SLPHMX,PRECIS)
C
C...  naccum=expected n to achieve .1x user requested tolerances
C
      NACCUM = ACCUM(NOLD+1) + 1.D0
      IF ( IPRINT .LT. 0 )  WRITE(IOUT,350) DEGEQU, NACCUM
C
C...  decide if mesh selection is worthwhile (otherwise, halve)
C
      IF ( AVRG .LT. PRECIS )                       GO TO 100
      IF ( DEGEQU .GE. .5D0 )                       GO TO 100
C
C...  nmx assures mesh has at least half as many subintervals as the
C...  previous mesh
C
      NMX = MAX0 ( NOLD+1, NACCUM ) / 2
C
C...  this assures that halving will be possible later (for error est)
C
      NMAX2 = NMAX / 2
C
C...  the mesh is at most halved
C
      N = MIN0 ( NMAX2, NOLD, NMX )
  220 NOLDP1 = NOLD + 1
      IF ( N .LT. NFXP1 )  N = NFXP1
      MSHNUM = MSHNUM + 1
C
C...  if the new mesh is smaller than the old mesh set mshnum
C...  so that the next call to  newmsh  will produce a halved
C...  mesh. if n .eq. nold / 2 increment mshalt so there can not
C...  be an infinite loop alternating between n and n/2 points.
C
      IF ( N .LT. NOLD )  MSHNUM = MSHLMT
      IF ( N .GT. NOLD/2 )  MSHALT = 1
      IF ( N .EQ. NOLD/2 )  MSHALT = MSHALT + 1
      MSHFLG = 0
C
C...  having decided to generate a new mesh with n subintervals we now
C...  do so, taking into account that the nfxpnt points in the array
C...  fixpnt must be included in the new mesh.
C
      IN = 1
      ACCL = 0.D0
      LOLD = 2
      XI(1) = ALEFT
      XI(N+1) = ARIGHT
      DO 310 I = 1, NFXP1
           IF ( I .EQ. NFXP1 )                      GO TO 250
           DO 230 J = LOLD, NOLDP1
             LNEW = J
             IF ( FIXPNT(I) .LE. XIOLD(J) )         GO TO 240
  230      CONTINUE
  240      CONTINUE
           ACCR = ACCUM(LNEW) + (FIXPNT(I)-XIOLD(LNEW))*SLOPE(LNEW-1)
           NREGN = (ACCR-ACCL) / ACCUM(NOLDP1) * DFLOAT(N) - .5D0
           NREGN = MIN0(NREGN, N - IN - NFXP1 + I)
           XI(IN + NREGN + 1) = FIXPNT(I)
           GO TO 260
  250      ACCR = ACCUM(NOLDP1)
           LNEW = NOLDP1
           NREGN = N - IN
  260      IF ( NREGN .EQ. 0 )                      GO TO 300
           TEMP = ACCL
           TSUM = (ACCR - ACCL) / DFLOAT(NREGN+1)
           DO 290 J = 1, NREGN
             IN = IN + 1
             TEMP = TEMP + TSUM
             DO 270 L = LOLD, LNEW
               LCARRY = L
               IF ( TEMP .LE. ACCUM(L) )            GO TO 280
  270        CONTINUE
  280        CONTINUE
             LOLD = LCARRY
  290      XI(IN) = XIOLD(LOLD-1) + (TEMP - ACCUM(LOLD-1)) /
     1     SLOPE(LOLD-1)
  300      IN = IN + 1
           ACCL = ACCR
           LOLD = LNEW
  310 CONTINUE
      MODE = 1
  320 CONTINUE
      NP1 = N + 1
      IF ( IPRINT .LT. 1 )  WRITE(IOUT,340) N, (XI(I),I=1,NP1)
      NZ   = MSTAR * (N + 1)
      NDMZ = KD * N
      RETURN
C----------------------------------------------------------------
  340 FORMAT(/17H THE NEW MESH (OF,I5,16H SUBINTERVALS), ,100(/8F12.6))
  350 FORMAT(/21H MESH SELECTION INFO,/30H DEGREE OF EQUIDISTRIBUTION =
     1       , F8.5, 28H PREDICTION FOR REQUIRED N = , I8)
  360 FORMAT(/20H THE FORMER MESH (OF,I5,15H SUBINTERVALS),,
     1       100(/8F12.6))
  370 FORMAT (/23H  EXPECTED N TOO LARGE  )
      END
