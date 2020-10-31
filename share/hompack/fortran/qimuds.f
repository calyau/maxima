      SUBROUTINE QIMUDS(Q,F,MAXA,NN,LENAA)
C
C  computes  f := [Q**(-1)] * f  .
C
C on input:
C
C Q  is the preconditioning matrix, and contains an approximate
C    factorization of M.
C
C f  is the right hand side vector, Q z = f .
C
C MAXA, NN, LENAA  describe Q in packed skyline storage format.
C
C on output:
C
C Q, MAXA, NN, LENAA  are unchanged.
C
C f  contains the solution z of Q z = f .
C
C
C Calls  SOLVDS .
C
      INTEGER LENAA,LENQ,NN,MAXA(NN+2),NQ
      DOUBLE PRECISION Q(LENAA+NN+1),F(NN+1)
C
      NQ=NN+1
      LENQ=MAXA(NN+2)-1
C
      CALL SOLVDS(NQ,Q,LENQ,MAXA,F)
C
      RETURN
      END
