*DECK DHEFA
      SUBROUTINE DHEFA (A, LDA, N, IPVT, INFO, JOB)
      INTEGER LDA, N, IPVT(*), INFO, JOB
      DOUBLE PRECISION A(LDA,*)
C-----------------------------------------------------------------------
C     This routine is a modification of the LINPACK routine DGEFA and
C     performs an LU decomposition of an upper Hessenberg matrix A.
C     There are two options available:
C
C          (1)  performing a fresh factorization
C          (2)  updating the LU factors by adding a row and a
C               column to the matrix A.
C-----------------------------------------------------------------------
C     DHEFA factors an upper Hessenberg matrix by elimination.
C
C     On entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the matrix to be factored.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C        JOB     INTEGER
C                JOB = 1    means that a fresh factorization of the
C                           matrix A is desired.
C                JOB .ge. 2 means that the current factorization of A
C                           will be updated by the addition of a row
C                           and a column.
C
C     On return
C
C        A       an upper triangular matrix and the multipliers
C                which were used to obtain it.
C                The factorization can be written  A = L*U  where
C                L  is a product of permutation and unit lower
C                triangular matrices and  U  is upper triangular.
C
C        IPVT    INTEGER(N)
C                an integer vector of pivot indices.
C
C        INFO    INTEGER
C                = 0  normal value.
C                = k  if  U(k,k) .eq. 0.0 .  This is not an error
C                     condition for this subroutine, but it does
C                     indicate that DHESL will divide by zero if called.
C
C     Modification of LINPACK, by Peter Brown, LLNL.
C     Written 7/20/83.  This version dated 6/20/01.
C    
C     BLAS called: DAXPY, IDAMAX
C-----------------------------------------------------------------------
      INTEGER IDAMAX, J, K, KM1, KP1, L, NM1
      DOUBLE PRECISION T
C
      IF (JOB .GT. 1) GO TO 80
C
C A new facorization is desired.  This is essentially the LINPACK
C code with the exception that we know there is only one nonzero
C element below the main diagonal.
C
C     Gaussian elimination with partial pivoting
C
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
C
C        Find L = pivot index
C
         L = IDAMAX (2, A(K,K), 1) + K - 1
         IPVT(K) = L
C
C        Zero pivot implies this column already triangularized
C
         IF (A(L,K) .EQ. 0.0D0) GO TO 40
C
C           Interchange if necessary
C
            IF (L .EQ. K) GO TO 10
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
   10       CONTINUE
C
C           Compute multipliers
C
            T = -1.0D0/A(K,K)
            A(K+1,K) = A(K+1,K)*T
C
C           Row elimination with column indexing
C
            DO 30 J = KP1, N
               T = A(L,J)
               IF (L .EQ. K) GO TO 20
                  A(L,J) = A(K,J)
                  A(K,J) = T
   20          CONTINUE
               CALL DAXPY (N-K, T, A(K+1,K), 1, A(K+1,J), 1)
   30       CONTINUE
         GO TO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
C
C The old factorization of A will be updated.  A row and a column
C has been added to the matrix A.
C N-1 is now the old order of the matrix.
C
  80  CONTINUE
      NM1 = N - 1
C
C Perform row interchanges on the elements of the new column, and
C perform elimination operations on the elements using the multipliers.
C
      IF (NM1 .LE. 1) GO TO 105
      DO 100 K = 2,NM1
        KM1 = K - 1
        L = IPVT(KM1)
        T = A(L,N)
        IF (L .EQ. KM1) GO TO 90
          A(L,N) = A(KM1,N)
          A(KM1,N) = T
  90    CONTINUE
        A(K,N) = A(K,N) + A(K,KM1)*T
 100    CONTINUE
 105  CONTINUE
C
C Complete update of factorization by decomposing last 2x2 block.
C
      INFO = 0
C
C        Find L = pivot index
C
         L = IDAMAX (2, A(NM1,NM1), 1) + NM1 - 1
         IPVT(NM1) = L
C
C        Zero pivot implies this column already triangularized
C
         IF (A(L,NM1) .EQ. 0.0D0) GO TO 140
C
C           Interchange if necessary
C
            IF (L .EQ. NM1) GO TO 110
               T = A(L,NM1)
               A(L,NM1) = A(NM1,NM1)
               A(NM1,NM1) = T
  110       CONTINUE
C
C           Compute multipliers
C
            T = -1.0D0/A(NM1,NM1)
            A(N,NM1) = A(N,NM1)*T
C
C           Row elimination with column indexing
C
               T = A(L,N)
               IF (L .EQ. NM1) GO TO 120
                  A(L,N) = A(NM1,N)
                  A(NM1,N) = T
  120          CONTINUE
               A(N,N) = A(N,N) + T*A(N,NM1)
         GO TO 150
  140    CONTINUE
            INFO = NM1
  150    CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
C----------------------- End of Subroutine DHEFA -----------------------
      END
