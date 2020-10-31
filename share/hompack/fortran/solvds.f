      SUBROUTINE SOLVDS(NN,A,NWK,MAXA,V)
C
C     This subroutine solves a system of linear equations Bx=b, where
C     B is symmetric, and is represented by its LDU factorization.
C
C     Input variables:
C
C        NN  -- dimension of B.
C
C        A -- one dimensional real array containing the upper
C             triangular skyline portion of the LDU decomposition
C             of the symmetric matrix B.
C
C        NWK  -- number of elements in A.
C
C        MAXA -- an integer array of length NN+1 which contains the
C                location in A of the diagonal elements of B.
C                By convention, MAXA(NN+1) = NWK+1 .
C
C        V -- real array of length NN containing the vector b.
C
C
C     Output variables:
C
C        V -- solution of the system of equations B x = b .
C
C
C     No working storage is required by this routine.
C
      INTEGER K,KK,KL,KU,L,NN,MAXA(NN+1),N,NWK
      DOUBLE PRECISION A(NWK),C,V(NN)
      DO 180 N=1,NN
         KL=MAXA(N)+1
         KU=MAXA(N+1)-1
         IF(KU-KL)180,160,160
160      K=N
         C=0.0
         DO 170 KK=KL,KU
            K=K-1
            C=C+A(KK)*V(K)
170      CONTINUE
         V(N)=V(N)-C
180   CONTINUE
800   DO 480 N=1,NN
         K=MAXA(N)
         V(N)=V(N)/A(K)
480   CONTINUE
      IF (NN.EQ.1) RETURN
      N=NN
      DO 500 L=2,NN
         KL=MAXA(N) + 1
         KU=MAXA(N+1) - 1
         IF (KU-KL) 530,510,510
510      K=N
         DO 520 KK=KL,KU
            K=K - 1
            V(K)=V(K) - A(KK)*V(N)
520      CONTINUE
530      N=N - 1
500   CONTINUE
      RETURN
      END
