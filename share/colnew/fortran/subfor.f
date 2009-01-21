      SUBROUTINE SUBFOR ( W, IPIVOT, NROW, LAST, X )
C
C**********************************************************************
C
C     carries out the forward pass of substitution for the current
C     block, i.e., the action on the right side corresponding to the
C     elimination carried out in  factrb  for this block.
C
C    parameters
C       w, ipivot, nrow, last  are as on return from factrb.
C       x(j)  is expected to contain, on input, the right side of j-th
C             equation for this block, j=1,...,nrow.
C       x(j)  contains, on output, the appropriately modified right
C             side of equation (j) in this block, j=1,...,last and
C             for j=last+1,...,nrow.
C
C*********************************************************************
C
      INTEGER IPIVOT(LAST), IP,K,KP1,LSTEP
      DOUBLE PRECISION W(NROW,LAST), X(NROW), T
C
      IF ( NROW .EQ. 1 )                            RETURN
      LSTEP = MIN0( NROW-1 , LAST )
      DO 20 K = 1, LSTEP
           KP1 = K + 1
           IP = IPIVOT(K)
           T = X(IP)
           X(IP) = X(K)
           X(K) = T
           IF ( T .EQ. 0.D0 )                       GO TO 20
           DO 10 I = KP1, NROW
   10         X(I) = X(I) + W(I,K) * T
   20 CONTINUE
   30 RETURN
      END
