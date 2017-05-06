*DECK DPREPJI
      SUBROUTINE DPREPJI (NEQ, Y, YH, NYH, EWT, RTEM, SAVR, S, WM, IWM,
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
      INTEGER I, I1, I2, IER, II, IRES, J, J1, JJ, LENP,
     1   MBA, MBAND, MEB1, MEBAND, ML, ML3, MU
      DOUBLE PRECISION CON, FAC, HL0, R, SRUR, YI, YJ, YJJ
C-----------------------------------------------------------------------
C DPREPJI is called by DSTODI to compute and process the matrix
C P = A - H*EL(1)*J , where J is an approximation to the Jacobian dr/dy,
C where r = g(t,y) - A(t,y)*s.  Here J is computed by the user-supplied
C routine JAC if MITER = 1 or 4, or by finite differencing if MITER =
C 2 or 5.  J is stored in WM, rescaled, and ADDA is called to generate
C P. P is then subjected to LU decomposition in preparation
C for later solution of linear systems with P as coefficient
C matrix.  This is done by DGEFA if MITER = 1 or 2, and by
C DGBFA if MITER = 4 or 5.
C
C In addition to variables described previously, communication
C with DPREPJI uses the following:
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
C         IWM(21).  IWM also contains the band parameters
C         ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5.
C EL0   = el(1) (input).
C IERPJ = output error flag.
C         = 0 if no trouble occurred,
C         = 1 if the P matrix was found to be singular,
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
      GO TO (100, 200, 300, 400, 500), MITER
C If MITER = 1, call RES, then JAC, and multiply by scalar. ------------
 100  IRES = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NFE = NFE + 1
      IF (IRES .GT. 1) GO TO 600
      LENP = N*N
      DO 110 I = 1,LENP
 110    WM(I+2) = 0.0D0
      CALL JAC ( NEQ, TN, Y, S, 0, 0, WM(3), N )
      CON = -HL0
      DO 120 I = 1,LENP
 120    WM(I+2) = WM(I+2)*CON
      GO TO 240
C If MITER = 2, make N + 1 calls to RES to approximate J. --------------
 200  CONTINUE
      IRES = -1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NFE = NFE + 1
      IF (IRES .GT. 1) GO TO 600
      SRUR = WM(1)
      J1 = 2
      DO 230 J = 1,N
        YJ = Y(J)
        R = MAX(SRUR*ABS(YJ),0.01D0/EWT(J))
        Y(J) = Y(J) + R
        FAC = -HL0/R
        CALL RES ( NEQ, TN, Y, S, RTEM, IRES )
        NFE = NFE + 1
        IF (IRES .GT. 1) GO TO 600
        DO 220 I = 1,N
 220      WM(I+J1) = (RTEM(I) - SAVR(I))*FAC
        Y(J) = YJ
        J1 = J1 + N
 230    CONTINUE
      IRES = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NFE = NFE + 1
      IF (IRES .GT. 1) GO TO 600
C Add matrix A. --------------------------------------------------------
 240  CONTINUE
      CALL ADDA(NEQ, TN, Y, 0, 0, WM(3), N)
C Do LU decomposition on P. --------------------------------------------
      CALL DGEFA (WM(3), N, N, IWM(21), IER)
      IF (IER .NE. 0) IERPJ = 1
      RETURN
C Dummy section for MITER = 3
 300  RETURN
C If MITER = 4, call RES, then JAC, and multiply by scalar. ------------
 400  IRES = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NFE = NFE + 1
      IF (IRES .GT. 1) GO TO 600
      ML = IWM(1)
      MU = IWM(2)
      ML3 = ML + 3
      MBAND = ML + MU + 1
      MEBAND = MBAND + ML
      LENP = MEBAND*N
      DO 410 I = 1,LENP
 410    WM(I+2) = 0.0D0
      CALL JAC ( NEQ, TN, Y, S, ML, MU, WM(ML3), MEBAND)
      CON = -HL0
      DO 420 I = 1,LENP
 420    WM(I+2) = WM(I+2)*CON
      GO TO 570
C If MITER = 5, make ML + MU + 2 calls to RES to approximate J. --------
 500  CONTINUE
      IRES = -1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NFE = NFE + 1
      IF (IRES .GT. 1) GO TO 600
      ML = IWM(1)
      MU = IWM(2)
      ML3 = ML + 3
      MBAND = ML + MU + 1
      MBA = MIN(MBAND,N)
      MEBAND = MBAND + ML
      MEB1 = MEBAND - 1
      SRUR = WM(1)
      DO 560 J = 1,MBA
        DO 530 I = J,N,MBAND
          YI = Y(I)
          R = MAX(SRUR*ABS(YI),0.01D0/EWT(I))
 530      Y(I) = Y(I) + R
        CALL RES ( NEQ, TN, Y, S, RTEM, IRES)
        NFE = NFE + 1
        IF (IRES .GT. 1) GO TO 600
        DO 550 JJ = J,N,MBAND
          Y(JJ) = YH(JJ,1)
          YJJ = Y(JJ)
          R = MAX(SRUR*ABS(YJJ),0.01D0/EWT(JJ))
          FAC = -HL0/R
          I1 = MAX(JJ-MU,1)
          I2 = MIN(JJ+ML,N)
          II = JJ*MEB1 - ML + 2
          DO 540 I = I1,I2
 540        WM(II+I) = (RTEM(I) - SAVR(I))*FAC
 550      CONTINUE
 560    CONTINUE
      IRES = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NFE = NFE + 1
      IF (IRES .GT. 1) GO TO 600
C Add matrix A. --------------------------------------------------------
  570 CONTINUE
      CALL ADDA(NEQ, TN, Y, ML, MU, WM(ML3), MEBAND)
C Do LU decomposition of P. --------------------------------------------
      CALL DGBFA (WM(3), MEBAND, N, ML, MU, IWM(21), IER)
      IF (IER .NE. 0) IERPJ = 1
      RETURN
C Error return for IRES = 2 or IRES = 3 return from RES. ---------------
 600  IERPJ = IRES
      RETURN
C----------------------- End of Subroutine DPREPJI ---------------------
      END
