      SUBROUTINE RKBAS (S, COEF, K, M, RKB, DM, MODE)
C
C**********************************************************************
C
C   purpose
C           evaluate mesh independent runge-kutta basis for given s
C
C   variables
C     s      - argument, i.e. the relative position for which
C              the basis is to be evaluated ( 0. .le. s .le. 1. ).
C     coef   - precomputed derivatives of the basis
C     k      - number of collocatin points per subinterval
C     m      - maximal order of the differential equation
C     rkb    - the runge-kutta basis (0-th to (m-1)-th derivatives )
C     dm     - basis elements for m-th derivative
C
C**********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION COEF(K,1), RKB(7,1), DM(1), T(10)
C
      IF ( K .EQ. 1 )                            GO TO 70
      KPM1 = K + M - 1
      DO 10 I = 1, KPM1
   10   T(I) = S / DFLOAT(I)
      DO 40 L = 1, M
         LB = K + L + 1
         DO 30 I = 1, K
           P = COEF(1,I)
           DO 20 J = 2, K
             P = P * T(LB-J)  + COEF(J,I)
   20      CONTINUE
           RKB(I,L) = P
   30    CONTINUE
   40 CONTINUE
      IF ( MODE .EQ. 0 )                         RETURN
      DO 60 I = 1, K
         P = COEF(1,I)
         DO 50 J = 2, K
   50       P = P * T(K+1-J) + COEF(J,I)
         DM(I) = P
   60 CONTINUE
      RETURN
   70 RKB(1,1) = 1.0D0
      DM(1) = 1.0D0
      RETURN
      END
