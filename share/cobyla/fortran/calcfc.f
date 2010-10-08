      SUBROUTINE CALCFC (N,M,X,F,CON)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(*),CON(*)
C
C    Test problem 2 (2D unit circle calculation)
C
          F=X(1)*X(2)
          CON(1)=1.0d0-X(1)**2-X(2)**2
      RETURN
      END
