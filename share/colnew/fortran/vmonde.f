      SUBROUTINE VMONDE ( RHO, COEF, K )
C
C**********************************************************************
C
C   purpose
C          solve vandermonde system v * x = e
C          with  v(i,j) = rho(j)**(i-1)/(i-1)! .
C
C**********************************************************************
C
      INTEGER K, I,IFAC,J,KM1,KMI
      DOUBLE PRECISION RHO(K), COEF(K)
C
      IF ( K .EQ. 1 )                             RETURN
      KM1 = K - 1
      DO 10 I = 1, KM1
         KMI = K - I
         DO 10 J = 1, KMI
           COEF(J) = (COEF(J+1) - COEF(J)) / (RHO(J+I) - RHO(J))
  10  CONTINUE
C
      IFAC = 1
      DO 40 I = 1, KM1
         KMI = K + 1 - I
         DO 30 J = 2, KMI
  30        COEF(J) = COEF(J) - RHO(J+I-1) * COEF(J-1)
         COEF(KMI) = DFLOAT(IFAC) * COEF(KMI)
         IFAC = IFAC * I
  40  CONTINUE
      COEF(1) = DFLOAT(IFAC) * COEF(1)
      RETURN
      END
