*DECK DHELS
      SUBROUTINE DHELS (A, LDA, N, Q, B)
      INTEGER LDA, N
      DOUBLE PRECISION A(LDA,*), B(*), Q(*)
C-----------------------------------------------------------------------
C This is part of the LINPACK routine DGESL with changes
C due to the fact that A is an upper Hessenberg matrix.
C-----------------------------------------------------------------------
C     DHELS solves the least squares problem
C
C           min (b-A*x, b-A*x)
C
C     using the factors computed by DHEQR.
C
C     On entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the output from DHEQR which contains the upper
C                triangular factor R in the QR decomposition of A.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                A is originally an (N+1) by N matrix.
C
C        Q       DOUBLE PRECISION(2*N)
C                The coefficients of the N givens rotations
C                used in the QR factorization of A.
C
C        B       DOUBLE PRECISION(N+1)
C                the right hand side vector.
C
C     On return
C
C        B       the solution vector  x .
C
C     Modification of LINPACK, by Peter Brown, LLNL.
C     Written 1/13/86.  This version dated 6/20/01.
C
C     BLAS called: DAXPY
C-----------------------------------------------------------------------
      INTEGER IQ, K, KB, KP1
      DOUBLE PRECISION C, S, T, T1, T2
C
C        Minimize (b-A*x, b-A*x)
C        First form Q*b.
C
         DO 20 K = 1, N
            KP1 = K + 1
            IQ = 2*(K-1) + 1
            C = Q(IQ)
            S = Q(IQ+1)
            T1 = B(K)
            T2 = B(KP1)
            B(K) = C*T1 - S*T2
            B(KP1) = S*T1 + C*T2
   20    CONTINUE
C
C        Now solve  R*x = Q*b.
C
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K)/A(K,K)
            T = -B(K)
            CALL DAXPY (K-1, T, A(1,K), 1, B(1), 1)
   40    CONTINUE
      RETURN
C----------------------- End of Subroutine DHELS -----------------------
      END
