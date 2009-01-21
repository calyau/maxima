      SUBROUTINE GDERIV ( GI, NROW, IROW, ZVAL, DGZ, MODE, DGSUB)
C
C**********************************************************************
C
C   purpose:
C
C      construct a collocation matrix row according to mode:
C      mode = 1  -  a row corresponding to a initial condition
C                   (i.e. at the left end of the subinterval).
C      mode = 2  -  a row corresponding to a final condition.
C
C   variables:
C
C      gi     - the sub-block of the global bvp matrix in
C               which the equations are to be formed.
C      nrow   - no. of rows in gi.
C      irow   - the row in gi to be used for equations.
C      zval   - z(xi)
C      dg     - the derivatives of the side condition.
C
C**********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION GI(NROW,1), ZVAL(1), DGZ(1), DG(40)
C
      COMMON /COLORD/ KDUM, NDUM, MSTAR, KD, MMAX, M(20)
      COMMON /COLSID/ ZETA(40), ALEFT, ARIGHT, IZETA, IDUM
      COMMON /COLNLN/ NONLIN, ITER, LIMIT, ICARE, IGUESS
C
C...  zero jacobian dg
C
      DO 10 J=1,MSTAR
   10   DG(J) = 0.D0
C
C...  evaluate jacobian dg
C
      CALL DGSUB (IZETA, ZVAL, DG)
C
C...  evaluate  dgz = dg * zval  once for a new mesh
C
      IF (NONLIN .EQ. 0 .OR. ITER .GT. 0)           GO TO 30
      DOT = 0.D0
      DO 20 J = 1, MSTAR
   20   DOT = DOT  +  DG(J) * ZVAL(J)
      DGZ(IZETA) = DOT
C
C...  branch according to  m o d e
C
   30 IF ( MODE .EQ. 2 )                            GO TO 50
C
C...  provide coefficients of the j-th linearized side condition.
C...  specifically, at x=zeta(j) the j-th side condition reads
C...  dg(1)*z(1) + ... +dg(mstar)*z(mstar) + g = 0
C
C
C...  handle an initial condition
C
      DO 40 J = 1, MSTAR
        GI(IROW,J) =  DG(J)
   40 GI(IROW,MSTAR+J) = 0.D0
      RETURN
C
C...  handle a final condition
C
   50 DO 60 J= 1, MSTAR
        GI(IROW,J) = 0.D0
   60 GI(IROW,MSTAR+J) = DG(J)
      RETURN
      END
