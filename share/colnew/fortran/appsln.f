C
C----------------------------------------------------------------------
C                             p a r t  4
C               polynomial and service routines
C----------------------------------------------------------------------
C
      SUBROUTINE APPSLN (X, Z, FSPACE, ISPACE)
C
C*****************************************************************
C
C     purpose
C
C           set up a standard call to  approx  to evaluate the
C           approximate solution  z = z( u(x) )  at a point x
C           (it has been computed by a call to  colnew ).
C           the parameters needed for  approx  are retrieved
C           from the work arrays  ispace  and  fspace .
C
C*****************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION Z(1), FSPACE(1), ISPACE(1), A(28), DUMMY(1)
      IS6 = ISPACE(6)
      IS5 = ISPACE(1) + 2
      IS4 = IS5 + ISPACE(4) * (ISPACE(1) + 1)
      I = 1
      CALL APPROX (I, X, Z, A, FSPACE(IS6), FSPACE(1), ISPACE(1),
     1             FSPACE(IS5), FSPACE(IS4), ISPACE(2), ISPACE(3),
     2             ISPACE(5), ISPACE(8), ISPACE(4), 2, DUMMY, 0)
      RETURN
      END
