*DECK DSOLBT
      SUBROUTINE DSOLBT (M, N, A, B, C, Y, IP)
      INTEGER M, N, IP(M,N)
      DOUBLE PRECISION A(M,M,N), B(M,M,N), C(M,M,N), Y(M,N)
C-----------------------------------------------------------------------
C Solution of block-tridiagonal linear system.
C Coefficient matrix must have been previously processed by DDECBT.
C M, N, A,B,C, and IP  must not have been changed since call to DDECBT.
C Written by A. C. Hindmarsh.
C Input:
C     M = order of each block.
C     N = number of blocks in each direction of matrix.
C A,B,C = M by M by N arrays containing block LU decomposition
C         of coefficient matrix from DDECBT.
C    IP = M by N integer array of pivot information from DDECBT.
C     Y = array of length M*N containg the right-hand side vector
C         (treated as an M by N array here).
C Output:
C     Y = solution vector, of length M*N.
C
C External routines required: DGESL (LINPACK) and DDOT (BLAS).
C-----------------------------------------------------------------------
C
      INTEGER NM1, NM2, I, K, KB, KM1, KP1
      DOUBLE PRECISION DP, DDOT
      NM1 = N - 1
      NM2 = N - 2
C Forward solution sweep. ----------------------------------------------
      CALL DGESL (A, M, M, IP, Y, 0)
      DO 30 K = 2,NM1
        KM1 = K - 1
        DO 20 I = 1,M
          DP = DDOT (M, C(I,1,K), M, Y(1,KM1), 1)
          Y(I,K) = Y(I,K) - DP
 20       CONTINUE
        CALL DGESL (A(1,1,K), M, M, IP(1,K), Y(1,K), 0)
 30     CONTINUE
      DO 50 I = 1,M
        DP = DDOT (M, C(I,1,N), M, Y(1,NM1), 1)
     1     + DDOT (M, B(I,1,N), M, Y(1,NM2), 1)
        Y(I,N) = Y(I,N) - DP
 50     CONTINUE
      CALL DGESL (A(1,1,N), M, M, IP(1,N), Y(1,N), 0)
C Backward solution sweep. ---------------------------------------------
      DO 80 KB = 1,NM1
        K = N - KB
        KP1 = K + 1
        DO 70 I = 1,M
          DP = DDOT (M, B(I,1,K), M, Y(1,KP1), 1)
          Y(I,K) = Y(I,K) - DP
 70       CONTINUE
 80     CONTINUE
      DO 100 I = 1,M
        DP = DDOT (M, C(I,1,1), M, Y(1,3), 1)
        Y(I,1) = Y(I,1) - DP
 100    CONTINUE
      RETURN
C----------------------- End of Subroutine DSOLBT ----------------------
      END
