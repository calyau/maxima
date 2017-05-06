*DECK DSPIOM
      SUBROUTINE DSPIOM (NEQ, TN, Y, SAVF, B, WGHT, N, MAXL, KMP, DELTA,
     1            HL0, JPRE, MNEWT, F, PSOL, NPSL, X, V, HES, IPVT,
     2            LIOM, WP, IWP, WK, IFLAG)
      EXTERNAL F, PSOL
      INTEGER NEQ,N,MAXL,KMP,JPRE,MNEWT,NPSL,IPVT,LIOM,IWP,IFLAG
      DOUBLE PRECISION TN,Y,SAVF,B,WGHT,DELTA,HL0,X,V,HES,WP,WK
      DIMENSION NEQ(*), Y(*), SAVF(*), B(*), WGHT(*), X(*), V(N,*),
     1   HES(MAXL,MAXL), IPVT(*), WP(*), IWP(*), WK(*)
C-----------------------------------------------------------------------
C This routine solves the linear system A * x = b using a scaled
C preconditioned version of the Incomplete Orthogonalization Method.
C An initial guess of x = 0 is assumed.
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
C         B    = the right hand side of the system A*x = b.
C                B is also used as work space when computing the
C                final approximation.
C                (B is the same as V(*,MAXL+1) in the call to DSPIOM.)
C
C         WGHT = array of length N containing scale factors.
C                1/WGHT(i) are the diagonal elements of the diagonal
C                scaling matrix D.
C
C         N    = the order of the matrix A, and the lengths
C                of the vectors Y, SAVF, B, WGHT, and X.
C
C         MAXL = the maximum allowable order of the matrix HES.
C
C          KMP = the number of previous vectors the new vector VNEW
C                must be made orthogonal to.  KMP .le. MAXL.
C
C        DELTA = tolerance on residuals b - A*x in weighted RMS-norm.
C
C          HL0 = current value of (step size h) * (coefficient l0).
C
C         JPRE = preconditioner type flag.
C
C        MNEWT = Newton iteration counter (.ge. 0).
C
C           WK = real work array of length N used by DATV and PSOL.
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
C         V    = the N by (LIOM+1) array containing the LIOM
C                orthogonal vectors V(*,1) to V(*,LIOM).
C
C         HES  = the LU factorization of the LIOM by LIOM upper
C                Hessenberg matrix whose entries are the
C                scaled inner products of A*V(*,k) and V(*,i).
C
C         IPVT = an integer array containg pivoting information.
C                It is loaded in DHEFA and used in DHESL.
C
C         LIOM = the number of iterations performed, and current
C                order of the upper Hessenberg matrix HES.
C
C         NPSL = the number of calls to PSOL.
C
C        IFLAG = integer error flag:
C                0 means convergence in LIOM iterations, LIOM.le.MAXL.
C                1 means the convergence test did not pass in MAXL
C                  iterations, but the residual norm is .lt. 1,
C                  or .lt. norm(b) if MNEWT = 0, and so X is computed.
C                2 means the convergence test did not pass in MAXL
C                  iterations, residual .gt. 1, and X is undefined.
C                3 means there was a recoverable error in PSOL
C                  caused by the preconditioner being out of date.
C               -1 means there was a nonrecoverable error in PSOL.
C
C-----------------------------------------------------------------------
      INTEGER I, IER, INFO, J, K, LL, LM1
      DOUBLE PRECISION BNRM, BNRM0, PROD, RHO, SNORMW, DNRM2, TEM
C
      IFLAG = 0
      LIOM = 0
      NPSL = 0
C-----------------------------------------------------------------------
C The initial residual is the vector b.  Apply scaling to b, and test
C for an immediate return with X = 0 or X = b.
C-----------------------------------------------------------------------
      DO 10 I = 1,N
 10     V(I,1) = B(I)*WGHT(I)
      BNRM0 = DNRM2 (N, V, 1)
      BNRM = BNRM0
      IF (BNRM0 .GT. DELTA) GO TO 30
      IF (MNEWT .GT. 0) GO TO 20
      CALL DCOPY (N, B, 1, X, 1)
      RETURN
 20   DO 25 I = 1,N
 25     X(I) = 0.0D0
      RETURN
 30   CONTINUE
C Apply inverse of left preconditioner to vector b. --------------------
      IER = 0
      IF (JPRE .EQ. 0 .OR. JPRE .EQ. 2) GO TO 55
      CALL PSOL (NEQ, TN, Y, SAVF, WK, HL0, WP, IWP, B, 1, IER)
      NPSL = 1
      IF (IER .NE. 0) GO TO 300
C Calculate norm of scaled vector V(*,1) and normalize it. -------------
      DO 50 I = 1,N
 50     V(I,1) = B(I)*WGHT(I)
      BNRM = DNRM2(N, V, 1)
      DELTA = DELTA*(BNRM/BNRM0)
 55   TEM = 1.0D0/BNRM
      CALL DSCAL (N, TEM, V(1,1), 1)
C Zero out the HES array. ----------------------------------------------
      DO 65 J = 1,MAXL
        DO 60 I = 1,MAXL
 60       HES(I,J) = 0.0D0
 65     CONTINUE
C-----------------------------------------------------------------------
C Main loop on LL = l to compute the vectors V(*,2) to V(*,MAXL).
C The running product PROD is needed for the convergence test.
C-----------------------------------------------------------------------
      PROD = 1.0D0
      DO 90 LL = 1,MAXL
        LIOM = LL
C-----------------------------------------------------------------------
C Call routine DATV to compute VNEW = Abar*v(l), where Abar is
C the matrix A with scaling and inverse preconditioner factors applied.
C Call routine DORTHOG to orthogonalize the new vector vnew = V(*,l+1).
C Call routine DHEFA to update the factors of HES.
C-----------------------------------------------------------------------
        CALL DATV (NEQ, Y, SAVF, V(1,LL), WGHT, X, F, PSOL, V(1,LL+1),
     1        WK, WP, IWP, HL0, JPRE, IER, NPSL)
        IF (IER .NE. 0) GO TO 300
        CALL DORTHOG (V(1,LL+1), V, HES, N, LL, MAXL, KMP, SNORMW)
        CALL DHEFA (HES, MAXL, LL, IPVT, INFO, LL)
        LM1 = LL - 1
        IF (LL .GT. 1 .AND. IPVT(LM1) .EQ. LM1) PROD = PROD*HES(LL,LM1)
        IF (INFO .NE. LL) GO TO 70
C-----------------------------------------------------------------------
C The last pivot in HES was found to be zero.
C If vnew = 0 or l = MAXL, take an error return with IFLAG = 2.
C otherwise, continue the iteration without a convergence test.
C-----------------------------------------------------------------------
        IF (SNORMW .EQ. 0.0D0) GO TO 120
        IF (LL .EQ. MAXL) GO TO 120
        GO TO 80
C-----------------------------------------------------------------------
C Update RHO, the estimate of the norm of the residual b - A*x(l).
C test for convergence.  If passed, compute approximation x(l).
C If failed and l .lt. MAXL, then continue iterating.
C-----------------------------------------------------------------------
 70     CONTINUE
        RHO = BNRM*SNORMW*ABS(PROD/HES(LL,LL))
        IF (RHO .LE. DELTA) GO TO 200
        IF (LL .EQ. MAXL) GO TO 100
C If l .lt. MAXL, store HES(l+1,l) and normalize the vector v(*,l+1).
 80     CONTINUE
        HES(LL+1,LL) = SNORMW
        TEM = 1.0D0/SNORMW
        CALL DSCAL (N, TEM, V(1,LL+1), 1)
 90     CONTINUE
C-----------------------------------------------------------------------
C l has reached MAXL without passing the convergence test:
C If RHO is not too large, compute a solution anyway and return with
C IFLAG = 1.  Otherwise return with IFLAG = 2.
C-----------------------------------------------------------------------
 100  CONTINUE
      IF (RHO .LE. 1.0D0) GO TO 150
      IF (RHO .LE. BNRM .AND. MNEWT .EQ. 0) GO TO 150
 120  CONTINUE
      IFLAG = 2
      RETURN
 150  IFLAG = 1
C-----------------------------------------------------------------------
C Compute the approximation x(l) to the solution.
C Since the vector X was used as work space, and the initial guess
C of the Newton correction is zero, X must be reset to zero.
C-----------------------------------------------------------------------
 200  CONTINUE
      LL = LIOM
      DO 210 K = 1,LL
 210    B(K) = 0.0D0
      B(1) = BNRM
      CALL DHESL (HES, MAXL, LL, IPVT, B)
      DO 220 K = 1,N
 220    X(K) = 0.0D0
      DO 230 I = 1,LL
        CALL DAXPY (N, B(I), V(1,I), 1, X, 1)
 230    CONTINUE
      DO 240 I = 1,N
 240    X(I) = X(I)/WGHT(I)
      IF (JPRE .LE. 1) RETURN
      CALL PSOL (NEQ, TN, Y, SAVF, WK, HL0, WP, IWP, X, 2, IER)
      NPSL = NPSL + 1
      IF (IER .NE. 0) GO TO 300
      RETURN
C-----------------------------------------------------------------------
C This block handles error returns forced by routine PSOL.
C-----------------------------------------------------------------------
 300  CONTINUE
      IF (IER .LT. 0) IFLAG = -1
      IF (IER .GT. 0) IFLAG = 3
      RETURN
C----------------------- End of Subroutine DSPIOM ----------------------
      END
