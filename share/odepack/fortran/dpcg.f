*DECK DPCG
      SUBROUTINE DPCG (NEQ, TN, Y, SAVF, R, WGHT, N, MAXL, DELTA, HL0,
     1 JPRE, MNEWT, F, PSOL, NPSL, X, P, W, Z, LPCG, WP, IWP, WK, IFLAG)
      EXTERNAL F, PSOL
      INTEGER NEQ, N, MAXL, JPRE, MNEWT, NPSL, LPCG, IWP, IFLAG
      DOUBLE PRECISION TN,Y,SAVF,R,WGHT,DELTA,HL0,X,P,W,Z,WP,WK
      DIMENSION NEQ(*), Y(*), SAVF(*), R(*), WGHT(*), X(*), P(*), W(*),
     1   Z(*), WP(*), IWP(*), WK(*)
C-----------------------------------------------------------------------
C This routine computes the solution to the system A*x = b using a
C preconditioned version of the Conjugate Gradient algorithm.
C It is assumed here that the matrix A and the preconditioner
C matrix M are symmetric positive definite or nearly so.
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
C            R = the right hand side of the system A*x = b.
C
C         WGHT = array of length N containing scale factors.
C                1/WGHT(i) are the diagonal elements of the diagonal
C                scaling matrix D.
C
C            N = the order of the matrix A, and the lengths
C                of the vectors Y, SAVF, R, WGHT, P, W, Z, WK, and X.
C
C         MAXL = the maximum allowable number of iterates.
C
C        DELTA = tolerance on residuals b - A*x in weighted RMS-norm.
C
C          HL0 = current value of (step size h) * (coefficient l0).
C
C         JPRE = preconditioner type flag.
C
C        MNEWT = Newton iteration counter (.ge. 0).
C
C           WK = real work array used by routine DATP.
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
C         LPCG = the number of iterations performed, and current
C                order of the upper Hessenberg matrix HES.
C
C         NPSL = the number of calls to PSOL.
C
C        IFLAG = integer error flag:
C                0 means convergence in LPCG iterations, LPCG .le. MAXL.
C                1 means the convergence test did not pass in MAXL
C                  iterations, but the residual norm is .lt. 1,
C                  or .lt. norm(b) if MNEWT = 0, and so X is computed.
C                2 means the convergence test did not pass in MAXL
C                  iterations, residual .gt. 1, and X is undefined.
C                3 means there was a recoverable error in PSOL
C                  caused by the preconditioner being out of date.
C                4 means there was a zero denominator in the algorithm.
C                  The system matrix or preconditioner matrix is not
C                  sufficiently close to being symmetric pos. definite.
C               -1 means there was a nonrecoverable error in PSOL.
C
C-----------------------------------------------------------------------
      INTEGER I, IER
      DOUBLE PRECISION ALPHA,BETA,BNRM,PTW,RNRM,DDOT,DVNORM,ZTR,ZTR0
C
      IFLAG = 0
      NPSL = 0
      LPCG = 0
      DO 10 I = 1,N
 10     X(I) = 0.0D0
      BNRM = DVNORM (N, R, WGHT)
C Test for immediate return with X = 0 or X = b. -----------------------
      IF (BNRM .GT. DELTA) GO TO 20
      IF (MNEWT .GT. 0) RETURN
      CALL DCOPY (N, R, 1, X, 1)
      RETURN
C
 20   ZTR = 0.0D0
C Loop point for PCG iterations. ---------------------------------------
 30   CONTINUE
      LPCG = LPCG + 1
      CALL DCOPY (N, R, 1, Z, 1)
      IER = 0
      IF (JPRE .EQ. 0) GO TO 40
      CALL PSOL (NEQ, TN, Y, SAVF, WK, HL0, WP, IWP, Z, 3, IER)
      NPSL = NPSL + 1
      IF (IER .NE. 0) GO TO 100
 40   CONTINUE
      ZTR0 = ZTR
      ZTR = DDOT (N, Z, 1, R, 1)
      IF (LPCG .NE. 1) GO TO 50
      CALL DCOPY (N, Z, 1, P, 1)
      GO TO 70
 50   CONTINUE
      IF (ZTR0 .EQ. 0.0D0) GO TO 200
      BETA = ZTR/ZTR0
      DO 60 I = 1,N
 60     P(I) = Z(I) + BETA*P(I)
 70   CONTINUE
C-----------------------------------------------------------------------
C  Call DATP to compute A*p and return the answer in W.
C-----------------------------------------------------------------------
      CALL DATP (NEQ, Y, SAVF, P, WGHT, HL0, WK, F, W)
C
      PTW = DDOT (N, P, 1, W, 1)
      IF (PTW .EQ. 0.0D0) GO TO 200
      ALPHA = ZTR/PTW
      CALL DAXPY (N, ALPHA, P, 1, X, 1)
      ALPHA = -ALPHA
      CALL DAXPY (N, ALPHA, W, 1, R, 1)
      RNRM = DVNORM (N, R, WGHT)
      IF (RNRM .LE. DELTA) RETURN
      IF (LPCG .LT. MAXL) GO TO 30
      IFLAG = 2
      IF (RNRM .LE. 1.0D0) IFLAG = 1
      IF (RNRM .LE. BNRM .AND. MNEWT .EQ. 0) IFLAG = 1
      RETURN
C-----------------------------------------------------------------------
C This block handles error returns from PSOL.
C-----------------------------------------------------------------------
 100  CONTINUE
      IF (IER .LT. 0) IFLAG = -1
      IF (IER .GT. 0) IFLAG = 3
      RETURN
C-----------------------------------------------------------------------
C This block handles division by zero errors.
C-----------------------------------------------------------------------
 200  CONTINUE
      IFLAG = 4
      RETURN
C----------------------- End of Subroutine DPCG ------------------------
      END
