      SUBROUTINE MFACDS(NN,Q,LENAA,MAXA)
C
C     SETS UP Q AS THE FACTORIZATION OF THE ENTIRE 
C     (NN+1) X (NN+1) MATRIX M.
C
C on input:
C 
C NN  is the dimension of the symmetric matrix AA, the upper left corner
C     of the augmented Jacobian matrix B.
C
C Q  contains AA in its first LENAA positions.
C
C LENAA  is the length of the one dimensional packed array AA.
C
C MAXA  is the integer array used for packed skyline storage.  It describes
C       AA and M, the symmetric piece of B.
C
C on output:
C
C NN, LENAA, and MAXA are unchanged.
C
C Q  contains an approximate factorization of M, in packed skyline storage
C    form.
C
C
C Calls  GMFADS .
C
      INTEGER I,IMAX,LENAA,LENQ,NN,MAXA(NN+2),NQ
      DOUBLE PRECISION Q(LENAA+NN+1)
C
      NQ=NN+1
      IMAX=MAXA(NN+2)-LENAA-2
      LENQ=MAXA(NN+2)-1
C
      DO 100 I=1,IMAX,1
         Q(LENAA+I)=0.0
100   CONTINUE
      Q(LENQ)=1.0D0
C
      CALL GMFADS(NQ,Q,LENQ,MAXA)
C
      RETURN
      END
