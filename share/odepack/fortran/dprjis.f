*DECK DPRJIS
      SUBROUTINE DPRJIS (NEQ, Y, YH, NYH, EWT, RTEM, SAVR, S, WK, IWK,
     1   RES, JAC, ADDA)
      EXTERNAL RES, JAC, ADDA
      INTEGER NEQ, NYH, IWK
      DOUBLE PRECISION Y, YH, EWT, RTEM, SAVR, S, WK
      DIMENSION NEQ(*), Y(*), YH(NYH,*), EWT(*), RTEM(*),
     1   S(*), SAVR(*), WK(*), IWK(*)
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
      DOUBLE PRECISION RLSS
      COMMON /DLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   IOWND(6), IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      COMMON /DLSS01/ RLSS(6),
     1   IPLOST, IESP, ISTATC, IYS, IBA, IBIAN, IBJAN, IBJGP,
     2   IPIAN, IPJAN, IPJGP, IPIGP, IPR, IPC, IPIC, IPISP, IPRSP, IPA,
     3   LENYH, LENYHM, LENWK, LREQ, LRAT, LREST, LWMIN, MOSS, MSBJ,
     4   NSLJ, NGP, NLU, NNZ, NSP, NZL, NZU
      INTEGER I, IMUL, IRES, J, JJ, JMAX, JMIN, K, KMAX, KMIN, NG
      DOUBLE PRECISION CON, FAC, HL0, R, SRUR
C-----------------------------------------------------------------------
C DPRJIS is called to compute and process the matrix
C P = A - H*EL(1)*J, where J is an approximation to the Jacobian dr/dy,
C where r = g(t,y) - A(t,y)*s.  J is computed by columns, either by
C the user-supplied routine JAC if MITER = 1, or by finite differencing
C if MITER = 2.  J is stored in WK, rescaled, and ADDA is called to
C generate P.  The matrix P is subjected to LU decomposition in CDRV.
C P and its LU decomposition are stored separately in WK.
C
C In addition to variables described previously, communication
C with DPRJIS uses the following:
C Y     = array containing predicted values on entry.
C RTEM  = work array of length N (ACOR in DSTODI).
C SAVR  = array containing r evaluated at predicted y. On output it
C         contains the residual evaluated at current values of t and y.
C S     = array containing predicted values of dy/dt (SAVF in DSTODI).
C WK    = real work space for matrices.  On output it contains P and
C         its sparse LU decomposition.  Storage of matrix elements
C         starts at WK(3).
C         WK also contains the following matrix-related data.
C         WK(1) = SQRT(UROUND), used in numerical Jacobian increments.
C IWK   = integer work space for matrix-related data, assumed to be
C         equivalenced to WK.  In addition,  WK(IPRSP) and IWK(IPISP)
C         are assumed to have identical locations.
C EL0   = EL(1) (input).
C IERPJ = output error flag (in COMMON).
C         =  0 if no error.
C         =  1 if zero pivot found in CDRV.
C         = IRES (= 2 or 3) if RES returned IRES = 2 or 3.
C         = -1 if insufficient storage for CDRV (should not occur).
C         = -2 if other error found in CDRV (should not occur here).
C JCUR  = output flag = 1 to indicate that the Jacobian matrix
C         (or approximation) is now current.
C This routine also uses other variables in Common.
C-----------------------------------------------------------------------
      HL0 = H*EL0
      CON = -HL0
      JCUR = 1
      NJE = NJE + 1
      GO TO (100, 200), MITER
C
C If MITER = 1, call RES, then call JAC and ADDA for each column. ------
 100  IRES = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NFE = NFE + 1
      IF (IRES .GT. 1) GO TO 600
      KMIN = IWK(IPIAN)
      DO 130 J = 1,N
        KMAX = IWK(IPIAN+J)-1
        DO 110 I = 1,N
 110      RTEM(I) = 0.0D0
        CALL JAC (NEQ, TN, Y, S, J, IWK(IPIAN), IWK(IPJAN), RTEM)
        DO 120 I = 1,N
 120      RTEM(I) = RTEM(I)*CON
        CALL ADDA (NEQ, TN, Y, J, IWK(IPIAN), IWK(IPJAN), RTEM)
        DO 125 K = KMIN,KMAX
          I = IWK(IBJAN+K)
          WK(IBA+K) = RTEM(I)
 125      CONTINUE
        KMIN = KMAX + 1
 130    CONTINUE
      GO TO 290
C
C If MITER = 2, make NGP + 1 calls to RES to approximate J and P. ------
 200  CONTINUE
      IRES = -1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NFE = NFE + 1
      IF (IRES .GT. 1) GO TO 600
      SRUR = WK(1)
      JMIN = IWK(IPIGP)
      DO 240 NG = 1,NGP
        JMAX = IWK(IPIGP+NG) - 1
        DO 210 J = JMIN,JMAX
          JJ = IWK(IBJGP+J)
          R = MAX(SRUR*ABS(Y(JJ)),0.01D0/EWT(JJ))
 210      Y(JJ) = Y(JJ) + R
        CALL RES (NEQ,TN,Y,S,RTEM,IRES)
        NFE = NFE + 1
        IF (IRES .GT. 1) GO TO 600
        DO 230 J = JMIN,JMAX
          JJ = IWK(IBJGP+J)
          Y(JJ) = YH(JJ,1)
          R = MAX(SRUR*ABS(Y(JJ)),0.01D0/EWT(JJ))
          FAC = -HL0/R
          KMIN = IWK(IBIAN+JJ)
          KMAX = IWK(IBIAN+JJ+1) - 1
          DO 220 K = KMIN,KMAX
            I = IWK(IBJAN+K)
            RTEM(I) = (RTEM(I) - SAVR(I))*FAC
 220        CONTINUE
        CALL ADDA (NEQ, TN, Y, JJ, IWK(IPIAN), IWK(IPJAN), RTEM)
        DO 225 K = KMIN,KMAX
          I = IWK(IBJAN+K)
          WK(IBA+K) = RTEM(I)
 225      CONTINUE
 230      CONTINUE
        JMIN = JMAX + 1
 240    CONTINUE
      IRES = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NFE = NFE + 1
      IF (IRES .GT. 1) GO TO 600
C
C Do numerical factorization of P matrix. ------------------------------
 290  NLU = NLU + 1
      IERPJ = 0
      DO 295 I = 1,N
 295    RTEM(I) = 0.0D0
      CALL CDRV (N,IWK(IPR),IWK(IPC),IWK(IPIC),IWK(IPIAN),IWK(IPJAN),
     1  WK(IPA),RTEM,RTEM,NSP,IWK(IPISP),WK(IPRSP),IESP,2,IYS)
      IF (IYS .EQ. 0) RETURN
      IMUL = (IYS - 1)/N
      IERPJ = -2
      IF (IMUL .EQ. 8) IERPJ = 1
      IF (IMUL .EQ. 10) IERPJ = -1
      RETURN
C Error return for IRES = 2 or IRES = 3 return from RES. ---------------
 600  IERPJ = IRES
      RETURN
C----------------------- End of Subroutine DPRJIS ----------------------
      END
