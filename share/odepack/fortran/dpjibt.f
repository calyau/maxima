*DECK DPJIBT
      SUBROUTINE DPJIBT (NEQ, Y, YH, NYH, EWT, RTEM, SAVR, S, WM, IWM,
     1   RES, JAC, ADDA)
      EXTERNAL RES, JAC, ADDA
      INTEGER NEQ, NYH, IWM
      DOUBLE PRECISION Y, YH, EWT, RTEM, SAVR, S, WM
      DIMENSION NEQ(*), Y(*), YH(NYH,*), EWT(*), RTEM(*),
     1   S(*), SAVR(*), WM(*), IWM(*)
      INTEGER IOWND, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      DOUBLE PRECISION ROWNS,
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      COMMON /DLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   IOWND(6), IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER I, IER, IIA, IIB, IIC, IPA, IPB, IPC, IRES, J, J1, J2,
     1   K, K1, LENP, LBLOX, LPB, LPC, MB, MBSQ, MWID, NB
      DOUBLE PRECISION CON, FAC, HL0, R, SRUR
C-----------------------------------------------------------------------
C DPJIBT is called by DSTODI to compute and process the matrix
C P = A - H*EL(1)*J , where J is an approximation to the Jacobian dr/dy,
C and r = g(t,y) - A(t,y)*s.  Here J is computed by the user-supplied
C routine JAC if MITER = 1, or by finite differencing if MITER = 2.
C J is stored in WM, rescaled, and ADDA is called to generate P.
C P is then subjected to LU decomposition by DDECBT in preparation
C for later solution of linear systems with P as coefficient matrix.
C
C In addition to variables described previously, communication
C with DPJIBT uses the following:
C Y     = array containing predicted values on entry.
C RTEM  = work array of length N (ACOR in DSTODI).
C SAVR  = array used for output only.  On output it contains the
C         residual evaluated at current values of t and y.
C S     = array containing predicted values of dy/dt (SAVF in DSTODI).
C WM    = real work space for matrices.  On output it contains the
C         LU decomposition of P.
C         Storage of matrix elements starts at WM(3).
C         WM also contains the following matrix-related data:
C         WM(1) = SQRT(UROUND), used in numerical Jacobian increments.
C IWM   = integer work space containing pivot information, starting at
C         IWM(21).  IWM also contains block structure parameters
C         MB = IWM(1) and NB = IWM(2).
C EL0   = EL(1) (input).
C IERPJ = output error flag.
C         = 0 if no trouble occurred,
C         = 1 if the P matrix was found to be unfactorable,
C         = IRES (= 2 or 3) if RES returned IRES = 2 or 3.
C JCUR  = output flag = 1 to indicate that the Jacobian matrix
C         (or approximation) is now current.
C This routine also uses the Common variables EL0, H, TN, UROUND,
C MITER, N, NFE, and NJE.
C-----------------------------------------------------------------------
      NJE = NJE + 1
      HL0 = H*EL0
      IERPJ = 0
      JCUR = 1
      MB = IWM(1)
      NB = IWM(2)
      MBSQ = MB*MB
      LBLOX = MBSQ*NB
      LPB = 3 + LBLOX
      LPC = LPB + LBLOX
      LENP = 3*LBLOX
      GO TO (100, 200), MITER
C If MITER = 1, call RES, then JAC, and multiply by scalar. ------------
 100  IRES = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NFE = NFE + 1
      IF (IRES .GT. 1) GO TO 600
      DO 110 I = 1,LENP
 110    WM(I+2) = 0.0D0
      CALL JAC (NEQ, TN, Y, S, MB, NB, WM(3), WM(LPB), WM(LPC))
      CON = -HL0
      DO 120 I = 1,LENP
 120    WM(I+2) = WM(I+2)*CON
      GO TO 260
C
C If MITER = 2, make 3*MB + 1 calls to RES to approximate J. -----------
 200  CONTINUE
      IRES = -1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NFE = NFE + 1
      IF (IRES .GT. 1) GO TO 600
      MWID = 3*MB
      SRUR = WM(1)
      DO 205 I = 1,LENP
 205    WM(2+I) = 0.0D0
      DO 250 K = 1,3
        DO 240 J = 1,MB
C         Increment Y(I) for group of column indices, and call RES. ----
          J1 = J+(K-1)*MB
          DO 210 I = J1,N,MWID
            R = MAX(SRUR*ABS(Y(I)),0.01D0/EWT(I))
            Y(I) = Y(I) + R
 210      CONTINUE
          CALL RES (NEQ, TN, Y, S, RTEM, IRES)
          NFE = NFE + 1
          IF (IRES .GT. 1) GO TO 600
          DO 215 I = 1,N
 215        RTEM(I) = RTEM(I) - SAVR(I)
          K1 = K
          DO 230 I = J1,N,MWID
C           Get Jacobian elements in column I (block-column K1). -------
            Y(I) = YH(I,1)
            R = MAX(SRUR*ABS(Y(I)),0.01D0/EWT(I))
            FAC = -HL0/R
C           Compute and load elements PA(*,J,K1). ----------------------
            IIA = I - J
            IPA = 2 + (J-1)*MB + (K1-1)*MBSQ
            DO 221 J2 = 1,MB
 221          WM(IPA+J2) = RTEM(IIA+J2)*FAC
            IF (K1 .LE. 1) GO TO 223
C           Compute and load elements PB(*,J,K1-1). --------------------
            IIB = IIA - MB
            IPB = IPA + LBLOX - MBSQ
            DO 222 J2 = 1,MB
 222          WM(IPB+J2) = RTEM(IIB+J2)*FAC
 223        CONTINUE
            IF (K1 .GE. NB) GO TO 225
C           Compute and load elements PC(*,J,K1+1). --------------------
            IIC = IIA + MB
            IPC = IPA + 2*LBLOX + MBSQ
            DO 224 J2 = 1,MB
 224          WM(IPC+J2) = RTEM(IIC+J2)*FAC
 225        CONTINUE
            IF (K1 .NE. 3) GO TO 227
C           Compute and load elements PC(*,J,1). -----------------------
            IPC = IPA - 2*MBSQ + 2*LBLOX
            DO 226 J2 = 1,MB
 226          WM(IPC+J2) = RTEM(J2)*FAC
 227        CONTINUE
            IF (K1 .NE. NB-2) GO TO 229
C           Compute and load elements PB(*,J,NB). ----------------------
            IIB = N - MB
            IPB = IPA + 2*MBSQ + LBLOX
            DO 228 J2 = 1,MB
 228          WM(IPB+J2) = RTEM(IIB+J2)*FAC
 229      K1 = K1 + 3
 230      CONTINUE
 240    CONTINUE
 250  CONTINUE
C RES call for first corrector iteration. ------------------------------
      IRES = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NFE = NFE + 1
      IF (IRES .GT. 1) GO TO 600
C Add matrix A. --------------------------------------------------------
 260  CONTINUE
      CALL ADDA (NEQ, TN, Y, MB, NB, WM(3), WM(LPB), WM(LPC))
C Do LU decomposition on P. --------------------------------------------
      CALL DDECBT (MB, NB, WM(3), WM(LPB), WM(LPC), IWM(21), IER)
      IF (IER .NE. 0) IERPJ = 1
      RETURN
C Error return for IRES = 2 or IRES = 3 return from RES. ---------------
 600  IERPJ = IRES
      RETURN
C----------------------- End of Subroutine DPJIBT ----------------------
      END
