      SUBROUTINE SUBBAK ( W, NROW, NCOL, LAST, X )
C
C*********************************************************************
C
C     carries out backsubstitution for current block.
C
C    parameters
C       w, ipivot, nrow, ncol, last  are as on return from factrb.
C       x(1),...,x(ncol)  contains, on input, the right side for the
C               equations in this block after backsubstitution has been
C               carried up to but not including equation (last).
C               means that x(j) contains the right side of equation (j)
C               as modified during elimination, j=1,...,last, while
C               for j .gt. last, x(j) is already a component of the
C               solution vector.
C       x(1),...,x(ncol) contains, on output, the components of the
C               solution corresponding to the present block.
C
C*********************************************************************
C
      INTEGER  LAST,  I,J,K,KM1,LM1,LP1
      DOUBLE PRECISION W(NROW,NCOL),X(NCOL), T
C
      LP1 = LAST + 1
      IF ( LP1 .GT. NCOL )                          GO TO 30
      DO 20 J = LP1, NCOL
         T = - X(J)
         IF ( T .EQ. 0.D0 )                         GO TO 20
         DO 10 I = 1, LAST
   10       X(I) = X(I) + W(I,J) * T
   20 CONTINUE
   30 IF ( LAST .EQ. 1 )                            GO TO 60
      LM1 = LAST - 1
      DO 50 KB = 1, LM1
        KM1 = LAST - KB
        K = KM1 + 1
        X(K) = X(K)/W(K,K)
        T = - X(K)
        IF ( T .EQ. 0.D0 )                          GO TO 50
        DO 40 I = 1, KM1
   40     X(I) = X(I) + W(I,K) * T
   50 CONTINUE
   60 X(1) = X(1)/W(1,1)
      RETURN
      END
