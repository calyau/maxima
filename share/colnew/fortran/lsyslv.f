C---------------------------------------------------------------------
C                            p a r t  3
C          collocation system setup routines
C---------------------------------------------------------------------
C
      SUBROUTINE LSYSLV (MSING, XI, XIOLD, Z, DMZ, DELZ, DELDMZ,
     1           G, W, V, RHS, DMZO, INTEGS, IPVTG, IPVTW, RNORM,
     2           MODE, FSUB, DFSUB, GSUB, DGSUB, GUESS )
C*********************************************************************
C
C   purpose
C         this routine controls the set up and solution of a linear
C      system of collocation equations.
C         the matrix  g  is cast into an almost block diagonal
C      form by an appropriate ordering of the columns and solved
C      using the package of de boor-weiss [5]. the matrix is composed
C      of n blocks. the i-th block has the size
C                  integs(1,i) * integs(2,i).
C      it contains in its last rows the linearized collocation
C      equations, condensed as described in [2],
C      and the linearized side conditions corresponding to
C      the i-th subinterval.  integs(3,i)  steps of gaussian
C      elimination are applied to it to achieve a  partial plu
C      decomposition.  the right hand side vector is put into  rhs
C      and the solution vector is returned in  delz and deldmz.
C
C         lsyslv operates according to one of 5 modes:
C      mode = 0 - set up the collocation matrices  v , w , g
C                 and the right hand side  rhs ,  and solve.
C                 (for linear problems only.)
C      mode = 1 - set up the collocation matrices  v , w , g
C                 and the right hand sides  rhs  and  dmzo ,
C                 and solve. also set up  integs .
C                 (first iteration of nonlinear problems only).
C      mode = 2 - set up  rhs  only and compute its norm.
C      mode = 3 - set up  v, w, g  only and solve system.
C      mode = 4 - perform forward and backward substitution only
C                 (do not set up the matrices nor form the rhs).
C
C   variables
C
C      ig,izeta  - pointers to g,zeta respectively
C                       (necessary to keep track of blocks of g
C                       during matrix manipulations)
C      idmz,irhs,iv,iw - pointers to  rhs,v,w rspectively
C      df    - partial derivatives of f from dfsub
C      rnorm - euclidean norm of rhs
C      lside - number of side conditions in current and previous blocks
C      iguess = 1 when current soln is user specified via  guess
C             = 0 otherwise
C
C*********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  Z(1), DMZ(1), DELZ(1), DELDMZ(1), XI(1), XIOLD(1)
      DIMENSION  G(1), W(1), V(1),  RHS(1) , DMZO(1), DUMMY(1)
      DIMENSION  INTEGS(3,1), IPVTG(1), IPVTW(1)
      DIMENSION  ZVAL(40), F(40), DGZ(40), DMVAL(20), DF(800), AT(28)
C
      COMMON /COLOUT/ PRECIS, IOUT, IPRINT
      COMMON /COLLOC/ RHO(7), COEF(49)
      COMMON /COLORD/ K, NCOMP, MSTAR, KD,  MMAX, M(20)
      COMMON /COLSID/ ZETA(40), ALEFT, ARIGHT, IZETA, IZSAVE
      COMMON /COLAPR/ N, NOLD, NMAX, NZ, NDMZ
      COMMON /COLNLN/ NONLIN, ITER, LIMIT, ICARE, IGUESS
      COMMON /COLBAS/ B(28), ACOL(28,7), ASAVE(28,4)
C
      EXTERNAL DFSUB, DGSUB
C
      M1 = MODE + 1
      GO TO (10, 30, 30, 30, 310), M1
C
C...  linear problem initialization
C
   10 DO 20 I=1,MSTAR
   20 ZVAL(I) = 0.D0
C
C...  initialization
C
   30 IDMZ = 1
      IDMZO = 1
      IRHS = 1
      IG = 1
      IW = 1
      IV = 1
      IZETA = 1
      LSIDE = 0
      IOLD = 1
      NCOL = 2 * MSTAR
      RNORM = 0.D0
      IF ( MODE .GT. 1 )                            GO TO 80
C
C...  build integs (describing block structure of matrix)
C
      DO 70 I = 1,N
           INTEGS(2,I) = NCOL
           IF (I .LT. N)                            GO TO 40
           INTEGS(3,N) = NCOL
           LSIDE = MSTAR
           GO TO 60
   40      INTEGS(3,I) = MSTAR
   50      IF( LSIDE .EQ. MSTAR )                   GO TO 60
           IF ( ZETA(LSIDE+1) .GE. XI(I)+PRECIS )   GO TO 60
           LSIDE = LSIDE + 1
           GO TO 50
   60      NROW = MSTAR + LSIDE
   70 INTEGS(1,I) = NROW
   80 CONTINUE
      IF ( MODE .EQ. 2 )                            GO TO 90
C
C...  zero the matrices to be computed
C
      LW = KD * KD * N
      DO 84 L = 1, LW
   84   W(L) = 0.D0
C
C...  the do loop 290 sets up the linear system of equations.
C
  90  CONTINUE
      DO 290 I=1, N
C
C...       construct a block of  a  and a corresponding piece of  rhs.
C
           XII = XI(I)
           H = XI(I+1) - XI(I)
           NROW = INTEGS(1,I)
C
C...       go thru the ncomp collocation equations and side conditions
C...       in the i-th subinterval
C
  100      IF ( IZETA .GT. MSTAR )                  GO TO 140
           IF ( ZETA(IZETA) .GT. XII + PRECIS )      GO TO 140
C
C...       build equation for a side condition.
C
           IF ( MODE .EQ. 0 )                       GO TO 110
           IF ( IGUESS .NE. 1 )                     GO TO 102
C
C...       case where user provided current approximation
C
           CALL GUESS (XII, ZVAL, DMVAL)
           GO TO 110
C
C...       other nonlinear case
C
  102      IF ( MODE .NE. 1 )                       GO TO 106
           CALL APPROX (IOLD, XII, ZVAL, AT, COEF, XIOLD, NOLD,
     1          Z, DMZ, K, NCOMP, MMAX, M, MSTAR, 2, DUMMY, 0)
           GO TO 110
  106      CALL APPROX (I, XII, ZVAL, AT, DUMMY, XI, N, Z, DMZ,
     1                  K, NCOMP, MMAX, M, MSTAR, 1, DUMMY, 0)
  108      IF ( MODE .EQ. 3 )                       GO TO 120
C
C...       find  rhs  boundary value.
C
  110      CALL GSUB (IZETA, ZVAL, GVAL)
           RHS(NDMZ+IZETA) = -GVAL
           RNORM = RNORM + GVAL**2
           IF ( MODE .EQ. 2 )                       GO TO 130
C
C...       build a row of  a  corresponding to a boundary point
C
  120      CALL GDERIV (G(IG), NROW, IZETA, ZVAL, DGZ, 1, DGSUB)
  130      IZETA = IZETA + 1
           GO TO 100
C
C...       assemble collocation equations
C
  140      DO 220 J = 1, K
             HRHO = H * RHO(J)
             XCOL = XII + HRHO
C
C...         this value corresponds to a collocation (interior)
C...         point. build the corresponding  ncomp  equations.
C
             IF ( MODE .EQ. 0 )                     GO TO 200
             IF ( IGUESS .NE. 1 )                   GO TO 160
C
C...         use initial approximation provided by the user.
C
             CALL GUESS (XCOL, ZVAL, DMZO(IRHS) )
             GO TO 170
C
C...         find  rhs  values
C
  160        IF ( MODE .NE. 1 )                     GO TO 190
             CALL APPROX (IOLD, XCOL, ZVAL, AT, COEF, XIOLD, NOLD,
     1            Z, DMZ, K, NCOMP, MMAX, M, MSTAR, 2, DMZO(IRHS), 1)
C
  170        CALL FSUB (XCOL, ZVAL, F)
             DO 180 JJ = 1, NCOMP
               VALUE = DMZO(IRHS) - F(JJ)
               RHS(IRHS) = - VALUE
               RNORM = RNORM + VALUE**2
               IRHS = IRHS + 1
  180        CONTINUE
             GO TO 210
C
C...         evaluate former collocation solution
C
  190        CALL APPROX (I, XCOL, ZVAL, ACOL(1,J), COEF, XI, N,
     1            Z, DMZ, K, NCOMP, MMAX, M, MSTAR, 4, DUMMY, 0)
             IF ( MODE .EQ. 3 )                     GO TO 210
C
C...         fill in  rhs  values (and accumulate its norm).
C
             CALL FSUB (XCOL, ZVAL, F)
             DO 195 JJ = 1, NCOMP
               VALUE = DMZ(IRHS) - F(JJ)
               RHS(IRHS) = - VALUE
               RNORM = RNORM + VALUE**2
               IRHS = IRHS + 1
  195        CONTINUE
             GO TO 220
C
C...         the linear case
C
  200        CALL FSUB (XCOL, ZVAL, RHS(IRHS))
             IRHS = IRHS + NCOMP
C
C...         fill in ncomp rows of  w and v
C
  210        CALL VWBLOK (XCOL, HRHO, J, W(IW), V(IV), IPVTW(IDMZ),
     1       KD, ZVAL, DF, ACOL(1,J), DMZO(IDMZO), NCOMP, DFSUB, MSING)
             IF ( MSING .NE. 0 )                    RETURN
  220      CONTINUE
C
C...       build global bvp matrix  g
C
           IF ( MODE .NE. 2 )
     1      CALL GBLOCK (H, G(IG), NROW, IZETA, W(IW), V(IV), KD,
     2                  DUMMY, DELDMZ(IDMZ), IPVTW(IDMZ), 1 )
           IF ( I .LT. N )                          GO TO 280
           IZSAVE = IZETA
  240      IF ( IZETA .GT. MSTAR )                  GO TO 290
C
C...       build equation for a side condition.
C
           IF ( MODE .EQ. 0 )                       GO TO 250
           IF ( IGUESS .NE. 1 )                     GO TO 245
C
C...       case where user provided current approximation
C
           CALL GUESS (ARIGHT, ZVAL, DMVAL)
           GO TO 250
C
C...       other nonlinear case
C
  245      IF ( MODE .NE. 1 )                       GO TO 246
           CALL APPROX (NOLD+1, ARIGHT, ZVAL, AT, COEF, XIOLD, NOLD,
     1          Z, DMZ, K, NCOMP, MMAX, M, MSTAR, 1, DUMMY, 0)
           GO TO 250
  246      CALL APPROX (N+1, ARIGHT, ZVAL, AT, COEF, XI, N,
     1          Z, DMZ, K, NCOMP, MMAX, M, MSTAR, 1, DUMMY, 0)
  248      IF ( MODE .EQ. 3 )                       GO TO 260
C
C...       find  rhs  boundary value.
C
  250      CALL GSUB (IZETA, ZVAL, GVAL)
           RHS(NDMZ+IZETA) = - GVAL
           RNORM = RNORM + GVAL**2
           IF ( MODE .EQ. 2 )                       GO TO 270
C
C...       build a row of  a  corresponding to a boundary point
C
  260      CALL GDERIV (G(IG), NROW, IZETA+MSTAR, ZVAL, DGZ, 2, DGSUB)
  270      IZETA = IZETA + 1
           GO TO 240
C
C...       update counters -- i-th block completed
C
  280      IG = IG + NROW * NCOL
           IV = IV + KD * MSTAR
           IW = IW + KD * KD
           IDMZ = IDMZ + KD
           IF ( MODE .EQ. 1 )  IDMZO = IDMZO + KD
  290 CONTINUE
C
C...       assembly process completed
C
      IF ( MODE .EQ. 0 .OR. MODE .EQ. 3 )           GO TO 300
      RNORM = DSQRT(RNORM / DFLOAT(NZ+NDMZ))
      IF ( MODE .NE. 2 )                            GO TO 300
      RETURN
C
C...  solve the linear system.
C
C...  matrix decomposition
C
  300 CALL FCBLOK (G, INTEGS, N, IPVTG, DF, MSING)
C
C...  check for singular matrix
C
      MSING = - MSING
      IF( MSING .NE. 0 )                            RETURN
C
C...  perform forward and backward substitution for mode=4 only.
C
  310 CONTINUE
      DO 311 L = 1, NDMZ
        DELDMZ(L) = RHS(L)
  311 CONTINUE
      IZ = 1
      IDMZ = 1
      IW = 1
      IZET = 1
      DO 320 I=1, N
         NROW = INTEGS(1,I)
         IZETA = NROW + 1 - MSTAR
         IF ( I .EQ. N ) IZETA = IZSAVE
  322    IF ( IZET .EQ. IZETA )                     GO TO 324
           DELZ(IZ-1+IZET) = RHS(NDMZ+IZET)
           IZET = IZET + 1
         GO TO 322
  324    H = XI(I+1) - XI(I)
         CALL GBLOCK (H, G(1), NROW, IZETA, W(IW), V(1), KD,
     1                DELZ(IZ), DELDMZ(IDMZ), IPVTW(IDMZ), 2 )
         IZ = IZ + MSTAR
         IDMZ = IDMZ + KD
         IW = IW + KD * KD
         IF ( I .LT. N )                            GO TO 320
  326    IF ( IZET .GT. MSTAR )                     GO TO 320
           DELZ(IZ-1+IZET) = RHS(NDMZ+IZET)
           IZET = IZET + 1
         GO TO 326
  320 CONTINUE
C
C...  perform forward and backward substitution for mode=0,2, or 3.
C
      CALL SBBLOK (G, INTEGS, N, IPVTG, DELZ)
C
C...  finaly find deldmz
C
      CALL DMZSOL (KD, MSTAR, N, V, DELZ, DELDMZ)
C
      IF ( MODE .NE. 1 )                            RETURN
      DO 321 L = 1, NDMZ
        DMZ(L) = DMZO(L)
  321 CONTINUE
      IZ = 1
      IDMZ = 1
      IW = 1
      IZET = 1
      DO 350 I=1, N
         NROW = INTEGS(1,I)
         IZETA = NROW + 1 - MSTAR
         IF ( I .EQ. N ) IZETA = IZSAVE
  330    IF ( IZET .EQ. IZETA )                     GO TO 340
           Z(IZ-1+IZET) = DGZ(IZET)
           IZET = IZET + 1
         GO TO 330
  340    H = XI(I+1) - XI(I)
         CALL GBLOCK (H, G(1), NROW, IZETA, W(IW), DF, KD,
     1                Z(IZ), DMZ(IDMZ), IPVTW(IDMZ), 2 )
         IZ = IZ + MSTAR
         IDMZ = IDMZ + KD
         IW = IW + KD * KD
         IF ( I .LT. N )                            GO TO 350
  342    IF ( IZET .GT. MSTAR )                     GO TO 350
            Z(IZ-1+IZET) = DGZ(IZET)
            IZET = IZET + 1
         GO TO 342
  350 CONTINUE
      CALL SBBLOK (G, INTEGS, N, IPVTG, Z)
C
C...  finaly find dmz
C
      CALL DMZSOL (KD, MSTAR, N, V, Z, DMZ)
C
      RETURN
      END
