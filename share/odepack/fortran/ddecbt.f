*DECK DDECBT
      SUBROUTINE DDECBT (M, N, A, B, C, IP, IER)
      INTEGER M, N, IP(M,N), IER
      DOUBLE PRECISION A(M,M,N), B(M,M,N), C(M,M,N)
C-----------------------------------------------------------------------
C Block-tridiagonal matrix decomposition routine.
C Written by A. C. Hindmarsh.
C Latest revision:  November 10, 1983 (ACH)
C Reference:  UCID-30150
C             Solution of Block-Tridiagonal Systems of Linear
C             Algebraic Equations
C             A.C. Hindmarsh
C             February 1977
C The input matrix contains three blocks of elements in each block-row,
C including blocks in the (1,3) and (N,N-2) block positions.
C DDECBT uses block Gauss elimination and Subroutines DGEFA and DGESL
C for solution of blocks.  Partial pivoting is done within
C block-rows only.
C
C Note: this version uses LINPACK routines DGEFA/DGESL instead of
C of dec/sol for solution of blocks, and it uses the BLAS routine DDOT
C for dot product calculations.
C
C Input:
C     M = order of each block.
C     N = number of blocks in each direction of the matrix.
C         N must be 4 or more.  The complete matrix has order M*N.
C     A = M by M by N array containing diagonal blocks.
C         A(i,j,k) contains the (i,j) element of the k-th block.
C     B = M by M by N array containing the super-diagonal blocks
C         (in B(*,*,k) for k = 1,...,N-1) and the block in the (N,N-2)
C         block position (in B(*,*,N)).
C     C = M by M by N array containing the subdiagonal blocks
C         (in C(*,*,k) for k = 2,3,...,N) and the block in the
C         (1,3) block position (in C(*,*,1)).
C    IP = integer array of length M*N for working storage.
C Output:
C A,B,C = M by M by N arrays containing the block-LU decomposition
C         of the input matrix.
C    IP = M by N array of pivot information.  IP(*,k) contains
C         information for the k-th digonal block.
C   IER = 0  if no trouble occurred, or
C       = -1 if the input value of M or N was illegal, or
C       = k  if a singular matrix was found in the k-th diagonal block.
C Use DSOLBT to solve the associated linear system.
C
C External routines required: DGEFA and DGESL (from LINPACK) and
C DDOT (from the BLAS, or Basic Linear Algebra package).
C-----------------------------------------------------------------------
      INTEGER NM1, NM2, KM1, I, J, K
      DOUBLE PRECISION DP, DDOT
      IF (M .LT. 1 .OR. N .LT. 4) GO TO 210
      NM1 = N - 1
      NM2 = N - 2
C Process the first block-row. -----------------------------------------
      CALL DGEFA (A, M, M, IP, IER)
      K = 1
      IF (IER .NE. 0) GO TO 200
      DO 10 J = 1,M
        CALL DGESL (A, M, M, IP, B(1,J,1), 0)
        CALL DGESL (A, M, M, IP, C(1,J,1), 0)
 10     CONTINUE
C Adjust B(*,*,2). -----------------------------------------------------
      DO 40 J = 1,M
        DO 30 I = 1,M
          DP = DDOT (M, C(I,1,2), M, C(1,J,1), 1)
          B(I,J,2) = B(I,J,2) - DP
 30       CONTINUE
 40     CONTINUE
C Main loop.  Process block-rows 2 to N-1. -----------------------------
      DO 100 K = 2,NM1
        KM1 = K - 1
        DO 70 J = 1,M
          DO 60 I = 1,M
            DP = DDOT (M, C(I,1,K), M, B(1,J,KM1), 1)
            A(I,J,K) = A(I,J,K) - DP
 60         CONTINUE
 70       CONTINUE
        CALL DGEFA (A(1,1,K), M, M, IP(1,K), IER)
        IF (IER .NE. 0) GO TO 200
        DO 80 J = 1,M
 80       CALL DGESL (A(1,1,K), M, M, IP(1,K), B(1,J,K), 0)
 100    CONTINUE
C Process last block-row and return. -----------------------------------
      DO 130 J = 1,M
        DO 120 I = 1,M
          DP = DDOT (M, B(I,1,N), M, B(1,J,NM2), 1)
          C(I,J,N) = C(I,J,N) - DP
 120      CONTINUE
 130    CONTINUE
      DO 160 J = 1,M
        DO 150 I = 1,M
          DP = DDOT (M, C(I,1,N), M, B(1,J,NM1), 1)
          A(I,J,N) = A(I,J,N) - DP
 150      CONTINUE
 160    CONTINUE
      CALL DGEFA (A(1,1,N), M, M, IP(1,N), IER)
      K = N
      IF (IER .NE. 0) GO TO 200
      RETURN
C Error returns. -------------------------------------------------------
 200  IER = K
      RETURN
 210  IER = -1
      RETURN
C----------------------- End of Subroutine DDECBT ----------------------
      END
