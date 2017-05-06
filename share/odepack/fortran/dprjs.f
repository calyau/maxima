*DECK DPRJS
      SUBROUTINE DPRJS (NEQ,Y,YH,NYH,EWT,FTEM,SAVF,WK,IWK,F,JAC)
      EXTERNAL F,JAC
      INTEGER NEQ, NYH, IWK
      DOUBLE PRECISION Y, YH, EWT, FTEM, SAVF, WK
      DIMENSION NEQ(*), Y(*), YH(NYH,*), EWT(*), FTEM(*), SAVF(*),
     1   WK(*), IWK(*)
      INTEGER IOWND, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER IPLOST, IESP, ISTATC, IYS, IBA, IBIAN, IBJAN, IBJGP,
     1   IPIAN, IPJAN, IPJGP, IPIGP, IPR, IPC, IPIC, IPISP, IPRSP, IPA,
     2   LENYH, LENYHM, LENWK, LREQ, LRAT, LREST, LWMIN, MOSS, MSBJ,
     3   NSLJ, NGP, NLU, NNZ, NSP, NZL, NZU
      DOUBLE PRECISION ROWNS,
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      DOUBLE PRECISION CON0, CONMIN, CCMXJ, PSMALL, RBIG, SETH
      COMMON /DLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   IOWND(6), IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      COMMON /DLSS01/ CON0, CONMIN, CCMXJ, PSMALL, RBIG, SETH,
     1   IPLOST, IESP, ISTATC, IYS, IBA, IBIAN, IBJAN, IBJGP,
     2   IPIAN, IPJAN, IPJGP, IPIGP, IPR, IPC, IPIC, IPISP, IPRSP, IPA,
     3   LENYH, LENYHM, LENWK, LREQ, LRAT, LREST, LWMIN, MOSS, MSBJ,
     4   NSLJ, NGP, NLU, NNZ, NSP, NZL, NZU
      INTEGER I, IMUL, J, JJ, JOK, JMAX, JMIN, K, KMAX, KMIN, NG
      DOUBLE PRECISION CON, DI, FAC, HL0, PIJ, R, R0, RCON, RCONT,
     1   SRUR, DVNORM
C-----------------------------------------------------------------------
C DPRJS is called to compute and process the matrix
C P = I - H*EL(1)*J , where J is an approximation to the Jacobian.
C J is computed by columns, either by the user-supplied routine JAC
C if MITER = 1, or by finite differencing if MITER = 2.
C if MITER = 3, a diagonal approximation to J is used.
C if MITER = 1 or 2, and if the existing value of the Jacobian
C (as contained in P) is considered acceptable, then a new value of
C P is reconstructed from the old value.  In any case, when MITER
C is 1 or 2, the P matrix is subjected to LU decomposition in CDRV.
C P and its LU decomposition are stored (separately) in WK.
C
C In addition to variables described previously, communication
C with DPRJS uses the following:
C Y     = array containing predicted values on entry.
C FTEM  = work array of length N (ACOR in DSTODE).
C SAVF  = array containing f evaluated at predicted y.
C WK    = real work space for matrices.  On output it contains the
C         inverse diagonal matrix if MITER = 3, and P and its sparse
C         LU decomposition if MITER is 1 or 2.
C         Storage of matrix elements starts at WK(3).
C         WK also contains the following matrix-related data:
C         WK(1) = SQRT(UROUND), used in numerical Jacobian increments.
C         WK(2) = H*EL0, saved for later use if MITER = 3.
C IWK   = integer work space for matrix-related data, assumed to
C         be equivalenced to WK.  In addition, WK(IPRSP) and IWK(IPISP)
C         are assumed to have identical locations.
C EL0   = EL(1) (input).
C IERPJ = output error flag (in Common).
C       = 0 if no error.
C       = 1  if zero pivot found in CDRV.
C       = 2  if a singular matrix arose with MITER = 3.
C       = -1 if insufficient storage for CDRV (should not occur here).
C       = -2 if other error found in CDRV (should not occur here).
C JCUR  = output flag showing status of (approximate) Jacobian matrix:
C          = 1 to indicate that the Jacobian is now current, or
C          = 0 to indicate that a saved value was used.
C This routine also uses other variables in Common.
C-----------------------------------------------------------------------
      HL0 = H*EL0
      CON = -HL0
      IF (MITER .EQ. 3) GO TO 300
C See whether J should be reevaluated (JOK = 0) or not (JOK = 1). ------
      JOK = 1
      IF (NST .EQ. 0 .OR. NST .GE. NSLJ+MSBJ) JOK = 0
      IF (ICF .EQ. 1 .AND. ABS(RC - 1.0D0) .LT. CCMXJ) JOK = 0
      IF (ICF .EQ. 2) JOK = 0
      IF (JOK .EQ. 1) GO TO 250
C
C MITER = 1 or 2, and the Jacobian is to be reevaluated. ---------------
 20   JCUR = 1
      NJE = NJE + 1
      NSLJ = NST
      IPLOST = 0
      CONMIN = ABS(CON)
      GO TO (100, 200), MITER
C
C If MITER = 1, call JAC, multiply by scalar, and add identity. --------
 100  CONTINUE
      KMIN = IWK(IPIAN)
      DO 130 J = 1, N
        KMAX = IWK(IPIAN+J) - 1
        DO 110 I = 1,N
 110      FTEM(I) = 0.0D0
        CALL JAC (NEQ, TN, Y, J, IWK(IPIAN), IWK(IPJAN), FTEM)
        DO 120 K = KMIN, KMAX
          I = IWK(IBJAN+K)
          WK(IBA+K) = FTEM(I)*CON
          IF (I .EQ. J) WK(IBA+K) = WK(IBA+K) + 1.0D0
 120      CONTINUE
        KMIN = KMAX + 1
 130    CONTINUE
      GO TO 290
C
C If MITER = 2, make NGP calls to F to approximate J and P. ------------
 200  CONTINUE
      FAC = DVNORM(N, SAVF, EWT)
      R0 = 1000.0D0 * ABS(H) * UROUND * N * FAC
      IF (R0 .EQ. 0.0D0) R0 = 1.0D0
      SRUR = WK(1)
      JMIN = IWK(IPIGP)
      DO 240 NG = 1,NGP
        JMAX = IWK(IPIGP+NG) - 1
        DO 210 J = JMIN,JMAX
          JJ = IWK(IBJGP+J)
          R = MAX(SRUR*ABS(Y(JJ)),R0/EWT(JJ))
 210      Y(JJ) = Y(JJ) + R
        CALL F (NEQ, TN, Y, FTEM)
        DO 230 J = JMIN,JMAX
          JJ = IWK(IBJGP+J)
          Y(JJ) = YH(JJ,1)
          R = MAX(SRUR*ABS(Y(JJ)),R0/EWT(JJ))
          FAC = -HL0/R
          KMIN =IWK(IBIAN+JJ)
          KMAX =IWK(IBIAN+JJ+1) - 1
          DO 220 K = KMIN,KMAX
            I = IWK(IBJAN+K)
            WK(IBA+K) = (FTEM(I) - SAVF(I))*FAC
            IF (I .EQ. JJ) WK(IBA+K) = WK(IBA+K) + 1.0D0
 220        CONTINUE
 230      CONTINUE
        JMIN = JMAX + 1
 240    CONTINUE
      NFE = NFE + NGP
      GO TO 290
C
C If JOK = 1, reconstruct new P from old P. ----------------------------
 250  JCUR = 0
      RCON = CON/CON0
      RCONT = ABS(CON)/CONMIN
      IF (RCONT .GT. RBIG .AND. IPLOST .EQ. 1) GO TO 20
      KMIN = IWK(IPIAN)
      DO 275 J = 1,N
        KMAX = IWK(IPIAN+J) - 1
        DO 270 K = KMIN,KMAX
          I = IWK(IBJAN+K)
          PIJ = WK(IBA+K)
          IF (I .NE. J) GO TO 260
          PIJ = PIJ - 1.0D0
          IF (ABS(PIJ) .GE. PSMALL) GO TO 260
            IPLOST = 1
            CONMIN = MIN(ABS(CON0),CONMIN)
 260      PIJ = PIJ*RCON
          IF (I .EQ. J) PIJ = PIJ + 1.0D0
          WK(IBA+K) = PIJ
 270      CONTINUE
        KMIN = KMAX + 1
 275    CONTINUE
C
C Do numerical factorization of P matrix. ------------------------------
 290  NLU = NLU + 1
      CON0 = CON
      IERPJ = 0
      DO 295 I = 1,N
 295    FTEM(I) = 0.0D0
      CALL CDRV (N,IWK(IPR),IWK(IPC),IWK(IPIC),IWK(IPIAN),IWK(IPJAN),
     1   WK(IPA),FTEM,FTEM,NSP,IWK(IPISP),WK(IPRSP),IESP,2,IYS)
      IF (IYS .EQ. 0) RETURN
      IMUL = (IYS - 1)/N
      IERPJ = -2
      IF (IMUL .EQ. 8) IERPJ = 1
      IF (IMUL .EQ. 10) IERPJ = -1
      RETURN
C
C If MITER = 3, construct a diagonal approximation to J and P. ---------
 300  CONTINUE
      JCUR = 1
      NJE = NJE + 1
      WK(2) = HL0
      IERPJ = 0
      R = EL0*0.1D0
      DO 310 I = 1,N
 310    Y(I) = Y(I) + R*(H*SAVF(I) - YH(I,2))
      CALL F (NEQ, TN, Y, WK(3))
      NFE = NFE + 1
      DO 320 I = 1,N
        R0 = H*SAVF(I) - YH(I,2)
        DI = 0.1D0*R0 - H*(WK(I+2) - SAVF(I))
        WK(I+2) = 1.0D0
        IF (ABS(R0) .LT. UROUND/EWT(I)) GO TO 320
        IF (ABS(DI) .EQ. 0.0D0) GO TO 330
        WK(I+2) = 0.1D0*R0/DI
 320    CONTINUE
      RETURN
 330  IERPJ = 2
      RETURN
C----------------------- End of Subroutine DPRJS -----------------------
      END
