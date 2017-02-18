*DECK DHESL
      SUBROUTINE DHESL (A, LDA, N, IPVT, B)
      INTEGER LDA, N, IPVT(*)
      DOUBLE PRECISION A(LDA,*), B(*)
C-----------------------------------------------------------------------
C This is essentially the LINPACK routine DGESL except for changes
C due to the fact that A is an upper Hessenberg matrix.
C-----------------------------------------------------------------------
C     DHESL solves the real system A * x = b
C     using the factors computed by DHEFA.
C
C     On entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the output from DHEFA.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C        IPVT    INTEGER(N)
C                the pivot vector from DHEFA.
C
C        B       DOUBLE PRECISION(N)
C                the right hand side vector.
C
C     On return
C
C        B       the solution vector  x .
C
C     Modification of LINPACK, by Peter Brown, LLNL.
C     Written 7/20/83.  This version dated 6/20/01.
C
C     BLAS called: DAXPY
C-----------------------------------------------------------------------
      INTEGER K, KB, L, NM1
      DOUBLE PRECISION T
C
      NM1 = N - 1
C
C        Solve  A * x = b
C        First solve  L*y = b
C
         IF (NM1 .LT. 1) GO TO 30
         DO 20 K = 1, NM1
            L = IPVT(K)
            T = B(L)
            IF (L .EQ. K) GO TO 10
               B(L) = B(K)
               B(K) = T
   10       CONTINUE
            B(K+1) = B(K+1) + T*A(K+1,K)
   20    CONTINUE
   30    CONTINUE
C
C        Now solve  U*x = y
C
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K)/A(K,K)
            T = -B(K)
            CALL DAXPY (K-1, T, A(1,K), 1, B(1), 1)
   40    CONTINUE
      RETURN
C----------------------- End of Subroutine DHESL -----------------------
      END
