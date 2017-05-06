*DECK DSPIGMR
      SUBROUTINE DSPIGMR (NEQ, TN, Y, SAVF, B, WGHT, N, MAXL, MAXLP1,
     1  KMP, DELTA, HL0, JPRE, MNEWT, F, PSOL, NPSL, X, V, HES, Q,
     2  LGMR, WP, IWP, WK, DL, IFLAG)
      EXTERNAL F, PSOL
      INTEGER NEQ,N,MAXL,MAXLP1,KMP,JPRE,MNEWT,NPSL,LGMR,IWP,IFLAG
      DOUBLE PRECISION TN,Y,SAVF,B,WGHT,DELTA,HL0,X,V,HES,Q,WP,WK,DL
      DIMENSION NEQ(*), Y(*), SAVF(*), B(*), WGHT(*), X(*), V(N,*),
     1    HES(MAXLP1,*), Q(*), WP(*), IWP(*), WK(*), DL(*)
C-----------------------------------------------------------------------
C This routine solves the linear system A * x = b using a scaled
C preconditioned version of the Generalized Minimal Residual method.
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
C            B = the right hand side of the system A*x = b.
C                B is also used as work space when computing
C                the final approximation.
C                (B is the same as V(*,MAXL+1) in the call to DSPIGMR.)
C
C         WGHT = the vector of length N containing the nonzero
C                elements of the diagonal scaling matrix.
C
C            N = the order of the matrix A, and the lengths
C                of the vectors WGHT, B and X.
C
C         MAXL = the maximum allowable order of the matrix HES.
C
C       MAXLP1 = MAXL + 1, used for dynamic dimensioning of HES.
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
C           WK = real work array used by routine DATV and PSOL.
C
C           DL = real work array used for calculation of the residual
C                norm RHO when the method is incomplete (KMP .lt. MAXL).
C                Not needed or referenced in complete case (KMP = MAXL).
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
C         LGMR = the number of iterations performed and
C                the current order of the upper Hessenberg
C                matrix HES.
C
C         NPSL = the number of calls to PSOL.
C
C         V    = the N by (LGMR+1) array containing the LGMR
C                orthogonal vectors V(*,1) to V(*,LGMR).
C
C         HES  = the upper triangular factor of the QR decomposition
C                of the (LGMR+1) by lgmr upper Hessenberg matrix whose
C                entries are the scaled inner-products of A*V(*,i)
C                and V(*,k).
C
C         Q    = real array of length 2*MAXL containing the components
C                of the Givens rotations used in the QR decomposition
C                of HES.  It is loaded in DHEQR and used in DHELS.
C
C        IFLAG = integer error flag:
C                0 means convergence in LGMR iterations, LGMR .le. MAXL.
C                1 means the convergence test did not pass in MAXL
C                  iterations, but the residual norm is .lt. 1,
C                  or .lt. norm(b) if MNEWT = 0, and so x is computed.
C                2 means the convergence test did not pass in MAXL
C                  iterations, residual .gt. 1, and X is undefined.
C                3 means there was a recoverable error in PSOL
C                  caused by the preconditioner being out of date.
C               -1 means there was a nonrecoverable error in PSOL.
C
C-----------------------------------------------------------------------
      INTEGER I, IER, INFO, IP1, I2, J, K, LL, LLP1
      DOUBLE PRECISION BNRM,BNRM0,C,DLNRM,PROD,RHO,S,SNORMW,DNRM2,TEM
C
      IFLAG = 0
      LGMR = 0
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
      BNRM = DNRM2 (N, V, 1)
      DELTA = DELTA*(BNRM/BNRM0)
 55   TEM = 1.0D0/BNRM
      CALL DSCAL (N, TEM, V(1,1), 1)
C Zero out the HES array. ----------------------------------------------
      DO 65 J = 1,MAXL
        DO 60 I = 1,MAXLP1
 60       HES(I,J) = 0.0D0
 65     CONTINUE
C-----------------------------------------------------------------------
C Main loop to compute the vectors V(*,2) to V(*,MAXL).
C The running product PROD is needed for the convergence test.
C-----------------------------------------------------------------------
      PROD = 1.0D0
      DO 90 LL = 1,MAXL
        LGMR = LL
C-----------------------------------------------------------------------
C Call routine DATV to compute VNEW = Abar*v(ll), where Abar is
C the matrix A with scaling and inverse preconditioner factors applied.
C Call routine DORTHOG to orthogonalize the new vector VNEW = V(*,LL+1).
C Call routine DHEQR to update the factors of HES.
C-----------------------------------------------------------------------
        CALL DATV (NEQ, Y, SAVF, V(1,LL), WGHT, X, F, PSOL, V(1,LL+1),
     1        WK, WP, IWP, HL0, JPRE, IER, NPSL)
        IF (IER .NE. 0) GO TO 300
        CALL DORTHOG (V(1,LL+1), V, HES, N, LL, MAXLP1, KMP, SNORMW)
        HES(LL+1,LL) = SNORMW
        CALL DHEQR (HES, MAXLP1, LL, Q, INFO, LL)
        IF (INFO .EQ. LL) GO TO 120
C-----------------------------------------------------------------------
C Update RHO, the estimate of the norm of the residual b - A*xl.
C If KMP .lt. MAXL, then the vectors V(*,1),...,V(*,LL+1) are not
C necessarily orthogonal for LL .gt. KMP.  The vector DL must then
C be computed, and its norm used in the calculation of RHO.
C-----------------------------------------------------------------------
        PROD = PROD*Q(2*LL)
        RHO = ABS(PROD*BNRM)
        IF ((LL.GT.KMP) .AND. (KMP.LT.MAXL)) THEN
          IF (LL .EQ. KMP+1) THEN
            CALL DCOPY (N, V(1,1), 1, DL, 1)
            DO 75 I = 1,KMP
              IP1 = I + 1
              I2 = I*2
              S = Q(I2)
              C = Q(I2-1)
              DO 70 K = 1,N
 70             DL(K) = S*DL(K) + C*V(K,IP1)
 75           CONTINUE
            ENDIF
          S = Q(2*LL)
          C = Q(2*LL-1)/SNORMW
          LLP1 = LL + 1
          DO 80 K = 1,N
 80         DL(K) = S*DL(K) + C*V(K,LLP1)
          DLNRM = DNRM2 (N, DL, 1)
          RHO = RHO*DLNRM
          ENDIF
C-----------------------------------------------------------------------
C Test for convergence.  If passed, compute approximation xl.
C if failed and LL .lt. MAXL, then continue iterating.
C-----------------------------------------------------------------------
        IF (RHO .LE. DELTA) GO TO 200
        IF (LL .EQ. MAXL) GO TO 100
C-----------------------------------------------------------------------
C Rescale so that the norm of V(1,LL+1) is one.
C-----------------------------------------------------------------------
        TEM = 1.0D0/SNORMW
        CALL DSCAL (N, TEM, V(1,LL+1), 1)
 90     CONTINUE
 100  CONTINUE
      IF (RHO .LE. 1.0D0) GO TO 150
      IF (RHO .LE. BNRM .AND. MNEWT .EQ. 0) GO TO 150
 120  CONTINUE
      IFLAG = 2
      RETURN
 150  IFLAG = 1
C-----------------------------------------------------------------------
C Compute the approximation xl to the solution.
C Since the vector X was used as work space, and the initial guess
C of the Newton correction is zero, X must be reset to zero.
C-----------------------------------------------------------------------
 200  CONTINUE
      LL = LGMR
      LLP1 = LL + 1
      DO 210 K = 1,LLP1
 210    B(K) = 0.0D0
      B(1) = BNRM
      CALL DHELS (HES, MAXLP1, LL, Q, B)
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
C
      RETURN
C----------------------- End of Subroutine DSPIGMR ---------------------
      END
