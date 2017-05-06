*DECK DUSOL
      SUBROUTINE DUSOL (NEQ, TN, Y, SAVF, B, WGHT, N, DELTA, HL0, MNEWT,
     1   PSOL, NPSL, X, WP, IWP, WK, IFLAG)
      EXTERNAL PSOL
      INTEGER NEQ, N, MNEWT, NPSL, IWP, IFLAG
      DOUBLE PRECISION TN, Y, SAVF, B, WGHT, DELTA, HL0, X, WP, WK
      DIMENSION NEQ(*), Y(*), SAVF(*), B(*), WGHT(*), X(*),
     1   WP(*), IWP(*), WK(*)
C-----------------------------------------------------------------------
C This routine solves the linear system A * x = b using only a call
C to the user-supplied routine PSOL (no Krylov iteration).
C If the norm of the right-hand side vector b is smaller than DELTA,
C the vector X returned is X = b (if MNEWT = 0) or X = 0 otherwise.
C PSOL is called with an LR argument of 0.
C-----------------------------------------------------------------------
C
C      On entry
C
C          NEQ = problem size, passed to F and PSOL (NEQ(1) = N).
C
C           TN = current value of t.
C
C            Y = array containing current dependent variable vector.
C
C         SAVF = array containing current value of f(t,y).
C
C            B = the right hand side of the system A*x = b.
C
C         WGHT = the vector of length N containing the nonzero
C                elements of the diagonal scaling matrix.
C
C            N = the order of the matrix A, and the lengths
C                of the vectors WGHT, B and X.
C
C        DELTA = tolerance on residuals b - A*x in weighted RMS-norm.
C
C          HL0 = current value of (step size h) * (coefficient l0).
C
C        MNEWT = Newton iteration counter (.ge. 0).
C
C           WK = real work array used by PSOL.
C
C           WP = real work array used by preconditioner PSOL.
C
C          IWP = integer work array used by preconditioner PSOL.
C
C      On return
C
C         X    = the final computed approximation to the solution
C                of the system A*x = b.
C
C         NPSL = the number of calls to PSOL.
C
C        IFLAG = integer error flag:
C                0 means no trouble occurred.
C                3 means there was a recoverable error in PSOL
C                  caused by the preconditioner being out of date.
C               -1 means there was a nonrecoverable error in PSOL.
C
C-----------------------------------------------------------------------
      INTEGER I, IER
      DOUBLE PRECISION BNRM, DVNORM
C
      IFLAG = 0
      NPSL = 0
C-----------------------------------------------------------------------
C Test for an immediate return with X = 0 or X = b.
C-----------------------------------------------------------------------
      BNRM = DVNORM (N, B, WGHT)
      IF (BNRM .GT. DELTA) GO TO 30
      IF (MNEWT .GT. 0) GO TO 10
      CALL DCOPY (N, B, 1, X, 1)
      RETURN
 10   DO 20 I = 1,N
 20     X(I) = 0.0D0
      RETURN
C Make call to PSOL and copy result from B to X. -----------------------
 30   IER = 0
      CALL PSOL (NEQ, TN, Y, SAVF, WK, HL0, WP, IWP, B, 0, IER)
      NPSL = 1
      IF (IER .NE. 0) GO TO 100
      CALL DCOPY (N, B, 1, X, 1)
      RETURN
C-----------------------------------------------------------------------
C This block handles error returns forced by routine PSOL.
C-----------------------------------------------------------------------
 100  CONTINUE
      IF (IER .LT. 0) IFLAG = -1
      IF (IER .GT. 0) IFLAG = 3
      RETURN
C----------------------- End of Subroutine DUSOL -----------------------
      END
