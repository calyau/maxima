      SUBROUTINE HORDER (I, UHIGH, HI, DMZ, NCOMP, K)
C
C**********************************************************************
C
C   purpose
C           determine highest order (piecewise constant) derivatives
C           of the current collocation solution
C
C   variables
C     hi     - the stepsize, hi = xi(i+1) - xi(i)
C     dmz    - vector of mj-th derivative of the solution
C     uhigh  - the array of highest order (piecewise constant)
C              derivatives of the approximate solution on
C              (xi(i),xi(i+1)), viz,
C                          (k+mj-1)
C              uhigh(j) = u   (x)    on (xi(i),xi(i+1))
C                          j
C
C**********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION UHIGH(1), DMZ(1)
C
      COMMON /COLLOC/ RHO(7), COEF(49)
C
      DN = 1.D0 / HI**(K-1)
C
C...  loop over the ncomp solution components
C
      DO 10 ID = 1, NCOMP
         UHIGH(ID) = 0.D0
   10 CONTINUE
      KIN = 1
      IDMZ = (I-1) * K * NCOMP + 1
      DO 30 J = 1, K
         FACT = DN * COEF(KIN)
         DO 20 ID = 1, NCOMP
            UHIGH(ID) = UHIGH(ID)  +  FACT * DMZ(IDMZ)
            IDMZ = IDMZ + 1
   20    CONTINUE
         KIN = KIN + K
   30 CONTINUE
      RETURN
      END
